package dma
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.amba4.axi._
import config.TTConfig
import config._
import spinal.lib.system.dma.sg.{DmaMemoryLayout, DmaSg}

case class dma_wrap(cfg: TTConfig) extends Component {
    val io = new Bundle {
      val dma_cmd_req = slave Stream (DmaCmd(cfg.memAxi.addressWidth))
      val dma_dispatch = slave Stream (dma_dp(cfg))
      val dma_agu_dispatch = slave Stream (dma_dp(cfg))
      val dma_back = master Stream (alu_back(cfg))
      val dma0_axi = master(Axi4(cfg.memAxi))
      val dma1_axi = master(Axi4(cfg.memAxi))
      val dma0_wr  = master(sram_wport(cfg.L1_AW, cfg.L1_DW))
      val dma0_rd  = master(sram_rport(cfg.L1_AW, cfg.L1_DW))
      val dma1_wr  = master(sram_wport(cfg.L1_AW, cfg.L1_DW))
      val dma1_rd  = master(sram_rport(cfg.L1_AW, cfg.L1_DW))
    }

  val dma0 = new DmaEngineAxiToL1(cfg)
  val dma1 = new DmaEngineAxiToL1(cfg)
  val dma0_cmd = (Stream(DmaCmd(cfg.memAxi.addressWidth)))  // todo decode from dispatch & cmd_req
  val dma1_cmd = (Stream(DmaCmd(cfg.memAxi.addressWidth)))
  val flow_dma_disp_init = Flow(dma_dp(cfg))
  flow_dma_disp_init.valid := False
  flow_dma_disp_init.payload.assignFromBits(B(0,flow_dma_disp_init.payload.getBitsWidth bits))
  val dma0_disp = Reg(Flow(dma_dp(cfg))) init(flow_dma_disp_init)
  val dma1_disp = Reg(Flow(dma_dp(cfg))) init(flow_dma_disp_init)
  when(dma0.io.busy && !dma1.io.busy){
    when(dma0.io.done){
      dma0_disp.valid := False
    }
  } .elsewhen(!dma0.io.busy) {
    when(io.dma_dispatch.valid) {
      dma0_disp := io.dma_dispatch.asFlow
    } .elsewhen(io.dma_agu_dispatch.valid){
      dma0_disp := io.dma_agu_dispatch.asFlow
    } .otherwise{
      dma0_disp.valid := False
    }
  }
  when(!dma0.io.busy && dma1.io.busy){
    when(dma1.io.done){
      dma1_disp.valid := False
    }
  } .elsewhen(dma0.io.busy && !dma1.io.busy) {
    when(io.dma_dispatch.valid) {
      dma1_disp := io.dma_dispatch.asFlow
    } .elsewhen(io.dma_agu_dispatch.valid){
      dma1_disp := io.dma_agu_dispatch.asFlow
    } .otherwise{
      dma1_disp.valid := False
    }
  }

  //dma0_cmd <> io.dma_cmd_req
  // todo
  io.dma_cmd_req.ready := False
  dma0_cmd.valid := False
  dma0_cmd.payload := io.dma_cmd_req.payload
  dma1_cmd.valid := False
  dma1_cmd.payload := io.dma_cmd_req.payload

  io.dma_dispatch.ready := !(dma0.io.busy && dma1.io.busy)
  io.dma_agu_dispatch.ready := !(dma0.io.busy && dma1.io.busy)
  io.dma_back.valid.setAsReg() init(False)
  io.dma_back.payload.CMD_ID.setAsReg() init(0)

  when(io.dma_back.ready){
    when(io.dma_cmd_req.valid){
      io.dma_back.valid := dma0.io.done
      dma0_cmd <> io.dma_cmd_req
    } .otherwise{
      when(dma0.io.done && dma0_disp.is_last_trans){
        io.dma_back.valid := True
        io.dma_back.payload.CMD_ID := dma0_disp.CMD_ID
      } .elsewhen(dma1.io.done && dma1_disp.is_last_trans){
        io.dma_back.valid := True
        io.dma_back.payload.CMD_ID := dma1_disp.CMD_ID
      } .otherwise{
        io.dma_back.valid := False
      }
      dma0_cmd.valid := dma0_disp.valid
      dma0_cmd.payload := dma0_disp.payload.dma_cfg
      dma1_cmd.valid := dma1_disp.valid
      dma1_cmd.payload := dma1_disp.payload.dma_cfg
      //io.dma_back.valid := dma0.io.done || dma1.io.done // todo add fifo
      //io.dma_back.payload.CMD_ID := 0 // todo
    }
  } .otherwise{
    io.dma_back.valid := False
    dma0_cmd.valid := False
    dma1_cmd.valid := False
  }

  // 外部总线
  io.dma0_axi <> dma0.io.axi
  io.dma1_axi <> dma1.io.axi

  // 内部 L1Fabric 端口
  io.dma0_wr  <> dma0.io.wr
  io.dma0_rd  <> dma0.io.rd
  io.dma1_wr  <> dma1.io.wr
  io.dma1_rd  <> dma1.io.rd

  // 内部指令
  dma0.io.cmd <> dma0_cmd
  dma1.io.cmd <> dma1_cmd

//  io.dma0_busy := dma0.io.busy
//  io.dma1_busy := dma1.io.busy
//  io.dma0_done := dma0.io.done
//  io.dma1_done := dma1.io.done
}




