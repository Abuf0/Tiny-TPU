package alu
import spinal.core._
import spinal.lib._
import config.TTConfig
import config._

case class agu_dma(cfg: TTConfig) extends Component {
  val io = new Bundle {
    val agu_dma_dispatch = slave Stream (agu_dma_dp(cfg)) // from scb, decode info
    val dma_disp_cmd = master Stream(dma_dp(cfg)) // to FIFO
    val agu_dma_back = master Stream (alu_back(cfg))  // to scb
  }
  val flow_dma_disp_init = Flow(dma_dp(cfg))
  flow_dma_disp_init.valid := False
  flow_dma_disp_init.payload.assignFromBits(B(0,flow_dma_disp_init.payload.getBitsWidth bits))
  val dma_disp = Reg(Flow(dma_dp(cfg))) init(flow_dma_disp_init)

  /***
   * 以single agu trans为例， todo根据正确的agu_dma配置完成AGU功能（AGU_DMA优先级低，AGU_COMPUTE优先级高）
   */
  io.agu_dma_dispatch.ready := True // todo
  when(io.agu_dma_dispatch.valid){
    dma_disp.valid := True
    dma_disp.payload.CMD_ID := io.agu_dma_dispatch.CMD_ID
    dma_disp.payload.is_last_trans := True  // TODO
    dma_disp.payload.dma_cfg.isLoad := io.agu_dma_dispatch.payload.WORD_0(31)
    dma_disp.payload.dma_cfg.extAddr := io.agu_dma_dispatch.payload.WORD_0(30 downto 0).resize(cfg.memAxi.addressWidth).asUInt
    dma_disp.payload.dma_cfg.l1Addr := io.agu_dma_dispatch.payload.WORD_1.resize(cfg.memAxi.addressWidth).asUInt
    dma_disp.payload.dma_cfg.bytes := io.agu_dma_dispatch.payload.WORD_2.asUInt
    dma_disp.payload.dma_cfg.trans_len := io.agu_dma_dispatch.payload.WORD_3.resize(8).asUInt
  }
  io.dma_disp_cmd.valid := dma_disp.valid && io.dma_disp_cmd.ready
  io.dma_disp_cmd.payload := dma_disp.payload

  io.agu_dma_back.valid := io.dma_disp_cmd.valid && io.agu_dma_back.ready // todo
  io.agu_dma_back.payload.CMD_ID := dma_disp.payload.CMD_ID

}

