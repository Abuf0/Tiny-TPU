package alu
import spinal.core._
import spinal.lib._
import config.TTConfig
import config._

case class alu_fp(cfg: TTConfig) extends Component {
  val io = new Bundle {
    val alu_fp_dispatch = slave Stream (alu_fp_dp(cfg))
    val alu_fp_back = master Stream (alu_back(cfg))
  }
  io.alu_fp_dispatch.ready := False
  io.alu_fp_back.valid := False
  io.alu_fp_back.payload.CMD_ID := 0

}

