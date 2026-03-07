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

  // todo
  io.store_dispatch.ready := True
  io.packet_st.ready := True
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


}

