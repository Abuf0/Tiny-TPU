package alu
import spinal.core._
import spinal.lib._
import config.TTConfig
import config._

case class store_engine(cfg: TTConfig, aw:Int, lw: Int, pw: Int, dw: Int, lane_num: Int, elem_bytes: Int) extends Component {
  val io = new Bundle {
    val store_dispatch = slave(Stream(store_dp(cfg))) // todo
    val packet_st = slave(Stream(tpu_packet(pw, lane_num*elem_bytes*8)))  // from tensor core to STORE FIFO
    val store_engine_wr = Vec(master(sram_wport(aw, dw)), cfg.L1_BANK) // to  L1 fabric, need arbiter with DMA
    val store_engine_back = master(Stream(alu_back(cfg)))
  }

  // todo : when store engine busy?? hardware-aware or software-aware??? need pingpong??
  // todo : store dispatch just for config???
  io.store_dispatch.ready := True
  io.store_engine_back.valid.setAsReg() init(False)
  io.store_engine_back.payload.CMD_ID.setAsReg() init(0)
  when(io.store_dispatch.valid){
    io.store_engine_back.valid := True
    io.store_engine_back.payload.CMD_ID := io.store_dispatch.CMD_ID
  } .otherwise{
    io.store_engine_back.valid := False
  }
  for(b <- 0 until cfg.L1_BANK){
    io.store_engine_wr(b).valid := False
    io.store_engine_wr(b).addr  := 0
    io.store_engine_wr(b).data := 0
    io.store_engine_wr(b).strb := 0
  }
  val store_cfg = RegNextWhen(io.store_dispatch.payload, io.store_dispatch.valid) // init
  val store_req_fifo = StreamFifo(sram_wport(aw,dw), cfg.ST_REQ_DEPTH)
  val store_req = Stream(sram_wport(aw,dw))
  store_req.valid := io.packet_st.valid
  store_req.payload.valid := io.packet_st.valid
  store_req.payload.data := io.packet_st.data
  store_req.payload.strb := B((dw/8) bits, default -> True)
  io.packet_st.ready := store_req.ready
  store_req.payload.addr := 0
  store_req.payload.ready := True // todo dont care
  val store_oc = (store_cfg.OC0.asUInt + io.packet_st.payload.oc)
  val store_ow = (store_cfg.OW0.asUInt + io.packet_st.payload.ow)
  val store_oh = (store_cfg.OH0.asUInt + io.packet_st.payload.oh)

  when(store_cfg.LAYOUT_TYPE === 0){
    store_req.payload.addr := (store_cfg.BASE.asUInt + store_cfg.ELEM_BYTES.asUInt * (store_oc + store_cfg.C_OUT.asUInt * (store_ow + store_cfg.W_OUT.asUInt * store_oh))).resize(aw)
  }

  val bankBits    = log2Up(cfg.L1_BANK)
  val lineOffBits = log2Up(dw / 8) // bytes per line

  def bankSel(addr: UInt): UInt =
    addr(lineOffBits + bankBits - 1 downto lineOffBits)

  val bsel = bankSel(store_req_fifo.io.pop.payload.addr)
  store_req_fifo.io.push << store_req
  store_req_fifo.io.pop.ready := io.store_engine_wr(bsel).ready
  io.store_engine_wr(bsel).valid := store_req_fifo.io.pop.valid && store_req_fifo.io.pop.payload.valid
  io.store_engine_wr(bsel).addr := store_req_fifo.io.pop.payload.addr
  io.store_engine_wr(bsel).data := store_req_fifo.io.pop.payload.data
  io.store_engine_wr(bsel).strb := store_req_fifo.io.pop.payload.strb

}

