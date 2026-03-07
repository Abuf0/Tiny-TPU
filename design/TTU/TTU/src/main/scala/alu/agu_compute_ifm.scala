package alu

import spinal.core._
import spinal.lib._
import config.TTConfig
import config._

/**
 * AGU_COMPUTE (Scheme B: collect lane_num tokens -> then scan K per cycle)
 *
 * What changes vs your current version:
 *  - lane_num lanes represent lane_num DIFFERENT patches (different (oh,ow)).
 *  - In one cycle, emit lane_num IFM addresses for the SAME (kh,kw,c) (same k-step).
 *  - One tile needs K_H*K_W*C_IN groups (K steps). Each step emits lane_num reqs.
 *
 * Assumptions:
 *  - loop_ctrl provides tokens one-by-one (oh,ow,token_id).
 *  - We collect exactly lane_num tokens into a local buffer before starting K-scan.
 *  - All lanes in one emitted group share the same req_packetid (use token_id of lane0).
 *    (If your loop controller's token_id differs per token, you should provide a separate "batch_id"
 *     to use as packet_id; here we assume it matches across the collected tokens.)
 *  - Layout currently supports NHWC only (LAYOUT_TYPE == 0).
 */
case class agu_compute_ifm(cfg: TTConfig, aw: Int, lw: Int, pw: Int, lane_num: Int) extends Component {
  val io = new Bundle {
    val agu_compute_dispatch = slave Stream (agu_compute_dp(cfg))      // config
    val loop_ctrl           = slave Stream (loop_ctrl_scan(cfg,lane_num))      // token stream (oh,ow,token_id)
    val load_reqs           = Vec(master Stream(load_req(aw, lw, pw)), lane_num) // lane_num reqs per cycle
    val agu_compute_back    = master Stream (alu_back(cfg))           // back to scb
  }

  // -------------------------
  // Defaults
  // -------------------------
  for (l <- 0 until lane_num) {
    io.load_reqs(l).valid                 := False
    io.load_reqs(l).payload.req_valid     := False
    io.load_reqs(l).payload.req_addr      := 0
    io.load_reqs(l).payload.req_laneid    := U(l, lw bits)
    io.load_reqs(l).payload.req_packetid  := 0
    io.load_reqs(l).payload.req_ispad     := False
    io.load_reqs(l).payload.req_oh  := 0
    io.load_reqs(l).payload.req_ow  := 0
    io.load_reqs(l).payload.req_oc  := 0
  }

  assert(lw >= log2Up(lane_num), "lw must be >= log2Up(lane_num)")

  // -------------------------
  // Config latch
  // -------------------------
  val flow_agu_init = Flow(agu_compute_dp(cfg))
  flow_agu_init.valid := False
  flow_agu_init.payload.assignFromBits(B(0, flow_agu_init.payload.getBitsWidth bits))

  val agu_cfg     = Reg(Flow(agu_compute_dp(cfg))) init(flow_agu_init)
  val cfg_vld_reg = Reg(Bool()) init(False)

  // Busy means we are scanning K for a buffered token batch.
  val agu_busy = Reg(Bool()) init(False)

  // Accept config only when not busy
  io.agu_compute_dispatch.ready := !agu_busy
  when(io.agu_compute_dispatch.fire) {
    agu_cfg := io.agu_compute_dispatch.asFlow
    cfg_vld_reg := True
  }

  // Back channel (simple ack)
  io.agu_compute_back.valid.setAsReg() init(False)
  io.agu_compute_back.payload.CMD_ID.setAsReg() init(0)
  when(io.agu_compute_back.ready) {
    io.agu_compute_back.valid := agu_cfg.valid
    io.agu_compute_back.payload.CMD_ID := agu_cfg.payload.CMD_ID
  }

  // -------------------------
  // Token buffer: collect lane_num tokens (oh,ow,token_id)
  // -------------------------
  val tokCount = Reg(UInt(log2Up(lane_num + 1) bits)) init(0)
  val tokFull  = tokCount === U(lane_num)

  val tok_oh_buf = Vec(Reg(UInt(12 bits)) init(0), lane_num)
  val tok_ow_buf = Vec(Reg(UInt(12 bits)) init(0), lane_num)
  val tok_id0    = Reg(UInt(io.loop_ctrl.payload.token_id.getBitsWidth bits)) init(0)

  // Only collect tokens when not busy (single-buffer scheme)
  io.loop_ctrl.ready := !agu_busy //&& !tokFull

//  when(io.loop_ctrl.fire) {
//    val w = tokCount.resize(log2Up(lane_num) bits)
//
//    // store token into slot tokCount
//    tok_oh_buf(w) := io.loop_ctrl.payload.oh.asUInt
//    tok_ow_buf(w) := io.loop_ctrl.payload.ow.asUInt
//
//    // take lane0 token_id as "packet id" for the whole batch
//    when(tokCount === 0) {
//      tok_id0 := io.loop_ctrl.payload.token_id.asUInt
//    } otherwise {
//      // Optional safety: if your loop_ctrl token_id is NOT batch id, remove this assert
//      // assert(io.loop_ctrl.payload.token_id.asUInt === tok_id0, "token_id mismatch inside one collected batch")
//    }
//
//    tokCount := tokCount + 1
//  }
  when(io.loop_ctrl.fire) {
    for (l <- 0 until lane_num) {
      tok_oh_buf(l) := io.loop_ctrl.payload.oh(l).asUInt
      tok_ow_buf(l) := io.loop_ctrl.payload.ow(l).asUInt
    }
  }

  // Start scanning K once buffer is full AND cfg valid
  val startBatch = (!agu_busy) && io.loop_ctrl.fire/*&& (tokCount === U(lane_num))*/ && cfg_vld_reg

  // -------------------------
  // Micro-loop state shared across all lanes: (kh, kw, c) for current k-step
  // -------------------------
  val kh_cnt = Reg(UInt(4 bits)) init(0)
  val kw_cnt = Reg(UInt(4 bits)) init(0)
  val c_cnt  = Reg(UInt(12 bits)) init(0)

  // cfg fields as UInt
  val BASE       = agu_cfg.payload.BASE.asUInt.resize(aw) // byte base
  val ELEM_BYTES = agu_cfg.payload.ELEM_BYTES.asUInt      // 1/2/4
  val LAYOUT     = agu_cfg.payload.LAYOUT_TYPE.asUInt

  val H_IN     = agu_cfg.payload.H_IN.asUInt
  val W_IN     = agu_cfg.payload.W_IN.asUInt
  val C_IN     = agu_cfg.payload.C_IN.asUInt
  val K_H      = agu_cfg.payload.K_H.asUInt
  val K_W      = agu_cfg.payload.K_W.asUInt
  val STRIDE_H = agu_cfg.payload.STRIDE_H.asUInt
  val STRIDE_W = agu_cfg.payload.STRIDE_W.asUInt
  val DIL_H    = agu_cfg.payload.DILATION_H.asUInt
  val DIL_W    = agu_cfg.payload.DILATION_W.asUInt
  val PAD_TOP  = agu_cfg.payload.PAD_TOP.asUInt
  val PAD_LEFT = agu_cfg.payload.PAD_LEFT.asUInt
  val TILE_H0  = agu_cfg.payload.TILE_H0.asUInt
  val TILE_W0  = agu_cfg.payload.TILE_W0.asUInt
  val TILE_C0  = agu_cfg.payload.TILE_C0.asUInt

  // next(kh,kw,c) with carry c -> kw -> kh
  def nextStateShared(kh: UInt, kw: UInt, c: UInt): (UInt, UInt, UInt, Bool) = {
    val nkh  = UInt(4 bits)
    val nkw  = UInt(4 bits)
    val nc   = UInt(12 bits)
    val done = Bool()

    nkh := kh; nkw := kw; nc := c; done := False

    val lastC  = (c  === (C_IN - 1))
    val lastKW = (kw === (K_W  - 1))
    val lastKH = (kh === (K_H  - 1))

    when(lastC) {
      nc := 0
      when(lastKW) {
        nkw := 0
        when(lastKH) {
          nkh := 0
          done := True
        } otherwise {
          nkh := kh + 1
        }
      } otherwise {
        nkw := kw + 1
      }
    } otherwise {
      nc := c + 1
    }
    (nkh, nkw, nc, done)
  }

  // Start batch: set busy and reset k-state
  when(startBatch) {
    agu_busy := True
    kh_cnt := 0
    kw_cnt := 0
    c_cnt  := 0
  }

  // -------------------------
  // Address helper (NHWC)
  // -------------------------
  def elemByteOffset(idx: UInt, elemBytes: UInt): UInt = {
    val idx32 = idx.resize(32)
    Mux(elemBytes === U(1, 3 bits), idx32,
      Mux(elemBytes === U(2, 3 bits), (idx32 |<< 1),
        Mux(elemBytes === U(4, 3 bits), (idx32 |<< 2),
          idx32
        )
      )
    )
  }

  // -------------------------
  // Emit lane_num reqs / cycle (each lane = different (oh,ow), same (kh,kw,c))
  // -------------------------
  val allReady   = io.load_reqs.map(_.ready).andR
  val group_fire = agu_busy && allReady

  val idx_linear_dbg = Vec.fill(lane_num)(UInt(32 bits))
  val ih_dbg = Vec.fill(lane_num)(UInt(12 bits))
  val iw_dbg = Vec.fill(lane_num)(UInt(12 bits))
  val c_dbg = Vec.fill(lane_num)(UInt(12 bits))
  for (l <- 0 until lane_num) {
    idx_linear_dbg(l) := 0
    ih_dbg(l) := 0
    iw_dbg(l) := 0
    c_dbg(l) := 0
  }

  when(agu_busy) {
    for (l <- 0 until lane_num) {
      io.load_reqs(l).valid := True
      io.load_reqs(l).payload.req_valid    := True
      io.load_reqs(l).payload.req_laneid   := U(l, lw bits)
      io.load_reqs(l).payload.req_packetid := ((kh_cnt * K_W + kw_cnt)*C_IN + c_cnt).resize(pw) // todo  //tok_id0.resize(pw)
      io.load_reqs(l).payload.req_oh       := tok_oh_buf(l)
      io.load_reqs(l).payload.req_ow       := tok_ow_buf(l)

      // Compute ih/iw for THIS lane's token (oh,ow)
      val oh_l = tok_oh_buf(l)
      val ow_l = tok_ow_buf(l)

      val ih_s = SInt(17 bits)
      val iw_s = SInt(17 bits)

      ih_s := (TILE_H0.resize(17).asSInt +
        (oh_l.resize(17).asSInt * STRIDE_H.resize(17).asSInt) +
        (kh_cnt.resize(17).asSInt * DIL_H.resize(17).asSInt) -
        PAD_TOP.resize(17).asSInt).resize(17)

      iw_s := (TILE_W0.resize(17).asSInt +
        (ow_l.resize(17).asSInt * STRIDE_W.resize(17).asSInt) +
        (kw_cnt.resize(17).asSInt * DIL_W.resize(17).asSInt) -
        PAD_LEFT.resize(17).asSInt).resize(17)

      val ih_in = (ih_s >= 0) && (ih_s < H_IN.resize(17).asSInt)
      val iw_in = (iw_s >= 0) && (iw_s < W_IN.resize(17).asSInt)
      val in_bounds = ih_in && iw_in

      when(!in_bounds) {
        io.load_reqs(l).payload.req_ispad := True
        io.load_reqs(l).payload.req_addr  := 0
      } otherwise {
        io.load_reqs(l).payload.req_ispad := False

        val ih_u  = ih_s.asUInt.resize(12)
        val iw_u  = iw_s.asUInt.resize(12)
        val cin_u = (TILE_C0 + c_cnt).resize(12)

        val idx_linear = UInt(32 bits)
        idx_linear := (((ih_u.resize(32) * W_IN.resize(32)) + iw_u.resize(32)) * C_IN.resize(32) + cin_u.resize(32)).resize(32)
        idx_linear_dbg(l) := idx_linear
        ih_dbg(l) := ih_u
        iw_dbg(l) := iw_u
        c_dbg(l) := cin_u

        val byte_off  = elemByteOffset(idx_linear, ELEM_BYTES)
        val addr_calc = (BASE.resize(32) + byte_off).resize(aw)

        io.load_reqs(l).payload.req_addr := addr_calc

        when(LAYOUT =/= 0) {
          // TODO other layouts
        }
      }
    }
  }

  // -------------------------
  // Advance K-state after group_fire (ONE step per cycle)
  // -------------------------
  when(group_fire) {
    val done_cur = (kh_cnt === (K_H - 1)) && (kw_cnt === (K_W - 1)) && (c_cnt === (C_IN - 1))
    val (nkh, nkw, nc, _) = nextStateShared(kh_cnt, kw_cnt, c_cnt)

    when(done_cur) {
      // finish this batch
      agu_busy := False
      tokCount := 0
      kh_cnt := 0
      kw_cnt := 0
      c_cnt  := 0
    } otherwise {
      kh_cnt := nkh
      kw_cnt := nkw
      c_cnt  := nc
    }
  }
}