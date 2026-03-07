package alu
import spinal.core._
import spinal.lib._
import config.TTConfig
import config._

/**
 * AGU_COMPUTE_KER
 *  - Same-cycle lane_num addresses = same (kh,kw,c) but different outch (OC)
 *  - Supports TILE_OC0 + OC_TILE (oc tile scan), pads lanes beyond OC_TILE
 *
 * Assumption: KER in L1 is packed as:
 *    for kIndex = ((kh*K_W + kw)*C_IN + c):
 *      store OC_TILE weights contiguously (oc 0..OC_TILE-1)
 *
 * Address:
 *   kIndex  = ((kh*K_W + kw)*C_IN + c)
 *   oc_idx  = oc_step + lane
 *   wIndex  = kIndex*OC_TILE + oc_idx
 *   addr    = BASE + wIndex*ELEM_BYTES
 */
case class agu_compute_ker(cfg: TTConfig, aw:Int, lw: Int, pw: Int, lane_num: Int) extends Component {
  val io = new Bundle {
    val agu_compute_dispatch = slave Stream (agu_compute_dp(cfg)) // config
    val loop_ctrl            = slave Stream(loop_ctrl_scan(cfg,lane_num)) // trigger + pid(token_id)
    val load_reqs            = Vec(master Stream(load_req(aw, lw, pw)), lane_num)
    val agu_compute_back      = master Stream (alu_back(cfg))
  }

  // -------------------------
  // Defaults
  // -------------------------
  for(l <- 0 until lane_num){
    io.load_reqs(l).valid := False
    io.load_reqs(l).payload.req_valid := False
    io.load_reqs(l).payload.req_addr := 0
    io.load_reqs(l).payload.req_laneid := U(l, lw bits)
    io.load_reqs(l).payload.req_packetid := 0
    io.load_reqs(l).payload.req_ispad := False
    io.load_reqs(l).payload.req_oh := 0
    io.load_reqs(l).payload.req_ow := 0
    io.load_reqs(l).payload.req_oc := 0
  }
  assert(lw >= log2Up(lane_num), "lw must be >= log2Up(lane_num)")

  // -------------------------
  // cfg latch
  // -------------------------
  val flowInit = Flow(agu_compute_dp(cfg))
  flowInit.valid := False
  flowInit.payload.assignFromBits(B(0, flowInit.payload.getBitsWidth bits))

  val agu_cfg    = Reg(Flow(agu_compute_dp(cfg))) init(flowInit)
  val cfg_vld    = Reg(Bool()) init(False)
  val agu_busy   = Reg(Bool()) init(False)

  io.agu_compute_dispatch.ready := !agu_busy
  when(io.agu_compute_dispatch.fire){
    agu_cfg := io.agu_compute_dispatch.asFlow
    cfg_vld := True
  }

  // back channel ack
  io.agu_compute_back.valid.setAsReg() init(False)
  io.agu_compute_back.payload.CMD_ID.setAsReg() init(0)
  when(io.agu_compute_back.ready){
    io.agu_compute_back.valid := agu_cfg.valid
    io.agu_compute_back.payload.CMD_ID := agu_cfg.payload.CMD_ID
  }

  // -------------------------
  // Start from loop_ctrl (pid)
  // -------------------------
  val pid_r = Reg(UInt(io.loop_ctrl.payload.token_id.getBitsWidth bits)) init(0)
  io.loop_ctrl.ready := cfg_vld && !agu_busy
  when(io.loop_ctrl.fire){
    //pid_r    := io.loop_ctrl.payload.token_id.asUInt  // todo
    agu_busy := True
  }

  // -------------------------
  // Shared counters: (kh,kw,c) and oc_step
  // -------------------------
  val kh_cnt   = Reg(UInt(4 bits))  init(0)
  val kw_cnt   = Reg(UInt(4 bits))  init(0)
  val c_cnt    = Reg(UInt(12 bits)) init(0)
  val oc_step  = Reg(UInt(12 bits)) init(0) // 0, lane_num, 2*lane_num, ...

  when(io.loop_ctrl.fire){
    kh_cnt  := 0
    kw_cnt  := 0
    c_cnt   := 0
    oc_step := 0
  }

  // cfg fields
  val BASE       = agu_cfg.payload.BASE.asUInt.resize(aw)
  val ELEM_BYTES = agu_cfg.payload.ELEM_BYTES.asUInt
  val LAYOUT     = agu_cfg.payload.LAYOUT_TYPE.asUInt

  val C_IN   = agu_cfg.payload.C_IN.asUInt
  val K_H    = agu_cfg.payload.K_H.asUInt
  val K_W    = agu_cfg.payload.K_W.asUInt

  val TILE_OC0 = RegNextWhen(io.loop_ctrl.oc, io.loop_ctrl.fire) init(0)//agu_cfg.payload.TILE_C0.asUInt
  val OC_TILE  = agu_cfg.payload.OC_TILE.asUInt

  // next state for (kh,kw,c): carry c -> kw -> kh
  def nextKState(kh: UInt, kw: UInt, c: UInt): (UInt, UInt, UInt, Bool) = {
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

  // byte offset helper
  def elemByteOffset(idx: UInt, elemBytes: UInt): UInt = {
    val idx32 = idx.resize(32)
    Mux(elemBytes === U(1,3 bits), idx32,
      Mux(elemBytes === U(2,3 bits), (idx32 |<< 1),
        Mux(elemBytes === U(4,3 bits), (idx32 |<< 2), idx32)
      )
    )
  }

  // -------------------------
  // Emit group
  // -------------------------
  val allReady   = io.load_reqs.map(_.ready).andR
  val group_fire = agu_busy && allReady

  val idx_linear_dbg = Vec.fill(lane_num)(UInt(32 bits))
  val kIndex_dbg = Vec.fill(lane_num)(UInt(32 bits))
  val wIndex_dbg = Vec.fill(lane_num)(UInt(32 bits))
  val c_dbg = Vec.fill(lane_num)(UInt(32 bits))
  for (l <- 0 until lane_num) {
    idx_linear_dbg(l) := 0
    kIndex_dbg(l) := 0
    wIndex_dbg(l) := 0
    c_dbg(l) := 0
  }

  when(agu_busy){
    // kIndex = ((kh*K_W + kw)*C_IN + c)
    val kIndex = UInt(32 bits)
    kIndex := (((kh_cnt.resize(32) * K_W.resize(32)) + kw_cnt.resize(32)) * C_IN.resize(32) + c_cnt.resize(32)).resize(32)

    for(l <- 0 until lane_num){
      io.load_reqs(l).valid := True
      io.load_reqs(l).payload.req_valid    := True
      io.load_reqs(l).payload.req_laneid   := U(l, lw bits)
      io.load_reqs(l).payload.req_packetid := ((kh_cnt * K_W + kw_cnt)*C_IN + c_cnt).resize(pw)  // todo //pid_r.resize(pw)

      // tile内索引：0..OC_TILE-1
      val oc_in_tile = (oc_step + U(l, 12 bits)).resize(12)
      val oc_valid   = oc_in_tile < OC_TILE

      io.load_reqs(l).payload.req_oc       := oc_in_tile

      when(!oc_valid){
        io.load_reqs(l).payload.req_ispad := True
        io.load_reqs(l).payload.req_addr  := 0
      } otherwise {
        io.load_reqs(l).payload.req_ispad := False

        // --- Kh Kw C Oc 排布：Oc 最内层 ---
        // kIndex = ((kh*K_W + kw)*C_IN + c)
        val kIndex = UInt(32 bits)
        kIndex := (((kh_cnt.resize(32) * K_W.resize(32)) + kw_cnt.resize(32)) * C_IN.resize(32) + c_cnt.resize(32)).resize(32)

        // 全局 oc（注意：BASE 是全局 kernel buffer 起点，所以必须加 TILE_OC0）
        val oc_global = (TILE_OC0.asUInt + oc_in_tile).resize(32)

        // wIndex = (((kh*K_W + kw)*C_IN + c) * OC_TOTAL) + oc_global
        // 这里 OC_TOTAL 应该是“全局输出通道数”，不是 OC_TILE
        // 如果你现在 cfg 里没有 OC_TOTAL 字段，那你需要把它加到 agu_compute_dp 里（例如 OC_OUT）
        val OC_TOTAL = agu_cfg.payload.C_OUT.asUInt.resize(32)   // <-- 你需要确认字段名/补字段

        val wIndex = UInt(32 bits)
        wIndex := (kIndex * OC_TOTAL + oc_global).resize(32)

        val byte_off  = elemByteOffset(wIndex, ELEM_BYTES)
        val addr_calc = (BASE.resize(32) + byte_off).resize(aw)
        io.load_reqs(l).payload.req_addr := addr_calc

        when(LAYOUT =/= 0){
          // TODO other layout formulas / pack formats
        }
      }

    }
  }

  // -------------------------
  // Advance after group_fire
  // -------------------------
  when(group_fire){
    // 先扫 OC
    val oc_next = (oc_step + U(lane_num, 12 bits)).resize(12)
    val oc_wrap = oc_next >= OC_TILE

    when(oc_wrap){
      oc_step := 0
      // OC 扫完，推进到下一个 kIndex
      val (nkh, nkw, nc, doneK) = nextKState(kh_cnt, kw_cnt, c_cnt)

      when(doneK){
        // whole K done for this tile => stop
        agu_busy := False
        kh_cnt := 0
        kw_cnt := 0
        c_cnt  := 0
        oc_step:= 0
      } otherwise {
        kh_cnt := nkh
        kw_cnt := nkw
        c_cnt  := nc
      }
    } otherwise {
      // continue next oc block
      oc_step := oc_next
    }
  }
}