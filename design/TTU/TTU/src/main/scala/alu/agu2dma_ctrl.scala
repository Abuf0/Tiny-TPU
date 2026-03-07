package alu
import spinal.core._
import spinal.lib._
import config.TTConfig
import config._

case class agu2dma_ctrl(cfg: TTConfig) extends Component {
  val io = new Bundle {
    val dma_agu_ctrl = slave Stream (Bits(cfg.CMD_AW bits)) // from scb, global control for DMA_AGU
    val dma_disp_cmd = slave Stream(dma_dp(cfg))  // from AGU_DMA to FIFO
    val dma_agu_dispatch = master Stream (dma_dp(cfg))  // from FIFO to DMA
  }

  val dma_cmds = StreamFifo(dma_dp(cfg),cfg.AGU_DMA_DEPTH)
  val dma_agu_cmd_id = Reg(Bits(cfg.CMD_AW bits)) init(0)
  val agu_dispatch = Stream (dma_dp(cfg))

  //io.dma_disp_cmd.ready := (dma_cmds.io.occupancy >= 1) // near full
  dma_cmds.io.pop >> agu_dispatch
  dma_cmds.io.push << io.dma_disp_cmd

  agu_dispatch.ready := io.dma_agu_dispatch.ready
  io.dma_agu_dispatch.valid := agu_dispatch.valid
  io.dma_agu_dispatch.payload.dma_cfg := agu_dispatch.payload.dma_cfg
  io.dma_agu_dispatch.payload.is_last_trans := agu_dispatch.payload.is_last_trans
  io.dma_agu_dispatch.payload.CMD_ID := dma_agu_cmd_id  // replaced by DMA_AGU

  val agu2dma_work = Reg(Bool()) init(False)
  when(io.dma_agu_ctrl.valid){
    agu2dma_work := True
    dma_agu_cmd_id := io.dma_agu_ctrl.payload
  } .elsewhen(dma_cmds.io.availability === 0){
    agu2dma_work := False
  }

  io.dma_agu_ctrl.ready := !agu2dma_work


}

