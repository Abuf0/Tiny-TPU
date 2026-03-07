package alu
import spinal.core._
import spinal.lib._
import config.TTConfig
import config._

case class agu_compute_kwise(cfg: TTConfig, aw:Int, lw: Int, pw: Int, lane_num: Int) extends Component {
  val io = new Bundle {
    val agu_compute_dispatch = slave Stream (agu_compute_dp(cfg)) // from scb, decode info, just for config
    val loop_ctrl = slave Stream(loop_ctrl_scan(cfg,lane_num)) // from loop controller, scan out tile, trig agu work
    val load_reqs = Vec(master Stream(load_req(aw, lw, pw)), lane_num) // to LOAD REQ FIFO( independent-2-AGU, for IFM/KER )
    val agu_compute_back = master Stream (alu_back(cfg))  // to scb
  }

  val flow_agu_init = Flow(agu_compute_dp(cfg))
  flow_agu_init.valid := False
  flow_agu_init.payload.assignFromBits(B(0,flow_agu_init.payload.getBitsWidth bits))
  val agu_cfg = Reg(Flow(agu_compute_dp(cfg))) init(flow_agu_init)
  val agu_compute_work = Reg(Bool()) init(False)
  val agu_cfg_vld = Reg(Bool()) init(False)

  when(io.loop_ctrl.fire){
    agu_compute_work := True
  }

  io.agu_compute_dispatch.ready := !agu_compute_work
  agu_cfg := io.agu_compute_dispatch.asFlow

  io.agu_compute_back.valid.setAsReg() init(False)
  io.agu_compute_back.payload.CMD_ID.setAsReg() init(0)
  when(io.agu_compute_back.ready){
    io.agu_compute_back.valid := agu_cfg.valid
    io.agu_compute_back.payload.CMD_ID := agu_cfg.payload.CMD_ID
  }
  when(io.agu_compute_back.fire){
    agu_cfg_vld := True
  }

  for(l <- 0 until lane_num){
    io.load_reqs(l).valid := False
    io.load_reqs(l).payload.req_valid := False
    io.load_reqs(l).payload.req_addr := 0
    io.load_reqs(l).payload.req_laneid := U(l, lw bits)
    io.load_reqs(l).payload.req_packetid := 0
    io.load_reqs(l).payload.req_ispad := False
  }
  // 建议：保证 lw 足够
  assert(lw >= log2Up(lane_num), "lw must be >= log2Up(lane_num)")

  // -------------------------
  // Token latch (1-deep)
  // -------------------------
  val tok_valid = Reg(Bool()) init(False)
  val tok_oh    = Reg(UInt(12 bits)) init(0)
  val tok_ow    = Reg(UInt(12 bits)) init(0)
  val tok_id    = Reg(UInt(io.loop_ctrl.payload.token_id.getBitsWidth bits)) init(0)

  // token 只有在 cfg valid && 不忙 && 没有token缓存时接收
  io.loop_ctrl.ready := /*agu_cfg_vld &&*/ !tok_valid && !agu_compute_work
  when(io.loop_ctrl.fire){
    tok_valid := True
    tok_oh    := io.loop_ctrl.payload.oh(0).asUInt
    tok_ow    := io.loop_ctrl.payload.ow(0).asUInt
    tok_id    := io.loop_ctrl.payload.token_id(0).asUInt
  }

  // -------------------------
  // Micro-loop state: (kh, kw, c)
  // -------------------------
  val kh_cnt = Reg(UInt(4 bits)) init(0)
  val kw_cnt = Reg(UInt(4 bits)) init(0)
  val c_cnt  = Reg(UInt(12 bits)) init(0)

  when(tok_valid && !agu_compute_work){
    agu_compute_work := True
    kh_cnt := 0
    kw_cnt := 0
    c_cnt  := 0
  }

  // cfg fields as UInt
  val BASE       = agu_cfg.payload.BASE.asUInt.resize(aw) // assume byte addr
  val ELEM_BYTES = agu_cfg.payload.ELEM_BYTES.asUInt
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
  def nextState(kh: UInt, kw: UInt, c: UInt): (UInt, UInt, UInt, Bool) = {
    val nkh  = UInt(4 bits)
    val nkw  = UInt(4 bits)
    val nc   = UInt(12 bits)
    val done = Bool()

    nkh := kh; nkw := kw; nc := c; done := False

    val lastC  = (c  === (C_IN - 1))
    val lastKW = (kw === (K_W  - 1))
    val lastKH = (kh === (K_H  - 1))

    when(lastC){
      nc := 0
      when(lastKW){
        nkw := 0
        when(lastKH){
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

  // -------------------------
  // Per-lane unroll: lane0=current, lane1=next, ...
  // -------------------------
  val lane_kh   = Vec(UInt(4 bits), lane_num)
  val lane_kw   = Vec(UInt(4 bits), lane_num)
  val lane_c    = Vec(UInt(12 bits), lane_num)
  val lane_done = Vec(Bool(), lane_num) // from this lane onward => beyond patch end
  val idx_linear_dbg = Vec.fill(lane_num)(UInt(32 bits))
  val ihs_dbg = Vec.fill(lane_num)(SInt(17 bits))
  val iws_dbg = Vec.fill(lane_num)(SInt(17 bits))
  val unrollEn = tok_valid && agu_compute_work


  // lane0 默认
  lane_kh(0)   := Mux(unrollEn, kh_cnt, U(0,4 bits))
  lane_kw(0)   := Mux(unrollEn, kw_cnt, U(0,4 bits))
  lane_c(0)    := Mux(unrollEn, c_cnt,  U(0,12 bits))
  lane_done(0) := !unrollEn  // 不使能时，直接认为 done（padding）

  idx_linear_dbg(0) := 0
  ihs_dbg(0) := 0
  iws_dbg(0) := 0

  for(l <- 1 until lane_num){
    val (nkh, nkw, nc, d) = nextState(lane_kh(l-1), lane_kw(l-1), lane_c(l-1))

    idx_linear_dbg(l) := 0
    ihs_dbg(l) := 0
    iws_dbg(l) := 0
    lane_kh(l)   := Mux(unrollEn, nkh, U(0,4 bits))
    lane_kw(l)   := Mux(unrollEn, nkw, U(0,4 bits))
    lane_c(l)    := Mux(unrollEn, nc,  U(0,12 bits))

    // 只在 unrollEn 时传播，否则一律 done
    lane_done(l) := Mux(unrollEn, (lane_done(l-1) || d), True)
  }

  // -------------------------
  // Group handshake (B策略：所有 lane 都算“有效发射slot”)
  // -------------------------
  val allReady  = io.load_reqs.map(_.ready).andR
  val group_fire = agu_compute_work && tok_valid && allReady

  // --------------- Address helper (NHWC) ---------------
  //  def elemByteOffset(idx: UInt, elemBytes: UInt): UInt = {
  //    val off = UInt(32 bits)
  //    off := idx.resize(32)
  //    switch(elemBytes) {
  //      is(U(1,3 bits)) { off := idx.resize(32) }
  //      is(U(2,3 bits)) { off := (idx << 1).resize(32) }
  //      is(U(4,3 bits)) { off := (idx << 2).resize(32) }
  //      default         { off := idx.resize(32) }
  //    }
  //    off
  //  }

  def elemByteOffset(idx: UInt, elemBytes: UInt): UInt = {
    val idx32 = idx.resize(32)
    Mux(elemBytes === U(1,3 bits), idx32,
      Mux(elemBytes === U(2,3 bits), (idx32 |<< 1),
        Mux(elemBytes === U(4,3 bits), (idx32 |<< 2),
          idx32
        )
      )
    )
  }

  // -------------------------
  // Emit lane_num reqs / cycle (fixed width)
  // -------------------------
  when(agu_compute_work && tok_valid){
    for(l <- 0 until lane_num){
      io.load_reqs(l).valid := True
      io.load_reqs(l).payload.req_valid    := True
      io.load_reqs(l).payload.req_laneid   := U(l, lw bits)
      io.load_reqs(l).payload.req_packetid := tok_id.resize(pw)


      // B策略：lane_done==True 表示“本 lane 对应的 element 已超出 patch 末尾”
      when(lane_done(l)){
        io.load_reqs(l).payload.req_ispad := True
        io.load_reqs(l).payload.req_addr  := 0  // don't care, Load Engine should ignore
      } otherwise {
        // NHWC addr for this lane's (kh,kw,c)
        val ih_s = SInt(17 bits)
        val iw_s = SInt(17 bits)

        ih_s := (TILE_H0.resize(17).asSInt +
          (tok_oh.resize(17).asSInt * STRIDE_H.resize(17).asSInt) +
          (lane_kh(l).resize(17).asSInt * DIL_H.resize(17).asSInt) -
          PAD_TOP.resize(17).asSInt).resize(17)

        iw_s := (TILE_W0.resize(17).asSInt +
          (tok_ow.resize(17).asSInt * STRIDE_W.resize(17).asSInt) +
          (lane_kw(l).resize(17).asSInt * DIL_W.resize(17).asSInt) -
          PAD_LEFT.resize(17).asSInt).resize(17)

        ihs_dbg(l) := ih_s
        iws_dbg(l) := iw_s

        val ih_in = (ih_s >= 0) && (ih_s < H_IN.resize(17).asSInt)
        val iw_in = (iw_s >= 0) && (iw_s < W_IN.resize(17).asSInt)
        val in_bounds = ih_in && iw_in

        val ih_u  = ih_s.asUInt.resize(12)
        val iw_u  = iw_s.asUInt.resize(12)
        val cin_u = (TILE_C0 + lane_c(l)).resize(12)

        val idx_linear = UInt(32 bits)
        idx_linear := (((ih_u.resize(32) * W_IN.resize(32)) + iw_u.resize(32)) * C_IN.resize(32) + cin_u.resize(32)).resize(32)
        idx_linear_dbg(l) := idx_linear

        val byte_off = elemByteOffset(idx_linear, ELEM_BYTES)
        val addr_calc = (BASE.resize(32) + byte_off).resize(aw)
        assert(cfg.L1_DW >= (lane_num * ELEM_BYTES * 8), "L1 DW must be >= (lane_num * element_width)")

        io.load_reqs(l).payload.req_addr  := addr_calc
        io.load_reqs(l).payload.req_ispad := !in_bounds

        when(LAYOUT =/= 0) {
          // TODO: other layouts
        }
      }
    }
  }

  // -------------------------
  // Advance state after group_fire
  // -------------------------
  when(group_fire){
    // 这一组发了 lane_num 个 slot，其中一旦 lane_done(l) 变 True 说明 patch 已经在本组内结束
    val patch_done_this_group = unrollEn && lane_done(lane_num-1)

    // 正常情况下（patch 还没结束），推进到“发完 lane_num 个 element 之后”的状态
    val (nkh, nkw, nc, _) = nextState(lane_kh(lane_num - 1), lane_kw(lane_num - 1), lane_c(lane_num - 1))

    when(patch_done_this_group){
      // token 完成（本组内已经 padding 补齐）
      agu_compute_work := False
      tok_valid := False
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

