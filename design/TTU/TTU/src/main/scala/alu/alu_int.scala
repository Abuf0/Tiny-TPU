package alu
import spinal.core._
import spinal.lib._
import config.TTConfig
import config._

case class alu_int(cfg: TTConfig) extends Component {
  val io = new Bundle {
    val alu_int_dispatch = slave Stream (alu_int_dp(cfg))
    val alu_int_back = master Stream (alu_back(cfg))
  }
  io.alu_int_dispatch.ready := False
  io.alu_int_back.valid := False
  io.alu_int_back.payload.CMD_ID := 0

}

