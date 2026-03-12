import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.amba4.axi._
import config._
import tensor_core._
import alu._
import dma._
import common_lib._

case class ttu_core(cfg: TTConfig) extends Component {
  val io = new Bundle {
      // CPU -> NPU 控制面（寄存器读写）
      val ctrl_axi = slave(AxiLite4(cfg.ctrlAxiLite))

      // 两路 DMA -> 外部内存系统（DRAM/SRAM controller）
      val dma0_axi = master(Axi4(cfg.memAxi))
      val dma1_axi = master(Axi4(cfg.memAxi))

      // 中断：job done / error
      val irq  = out Bool()
      val fp_mac_a = in Bits(32 bits)
      val fp_mac_b = in Bits(32 bits)
      val fp_mac_c = in Bits(32 bits)
      val fp_mac_z = out Bits(32 bits)
  }

  val fp_mac_unit = new fp_mac(23,8,0)
  fp_mac_unit.io.a := io.fp_mac_a
  fp_mac_unit.io.b := io.fp_mac_b
  fp_mac_unit.io.c := io.fp_mac_c
  io.fp_mac_z := fp_mac_unit.io.z

  val fetch_unit = new fetch(cfg)
  val decode_unit = new decode(cfg)
  val scb_unit = new scoreboard(cfg)

  // todo
  val dma_unit = new dma_wrap(cfg)
  val agu_dma_unit = new agu_dma(cfg)
  val agu2dma_ctrl_unit = new agu2dma_ctrl(cfg)
  val alu_int_unit = new alu_int(cfg)
  val alu_fp_unit = new alu_fp(cfg)
  val loop_ctrl_unit = new loop_controller(cfg,cfg.TPU_H)
  val agu_compute_ifm_unit = new agu_compute_ifm(cfg, cfg.L1_AW, log2Up(cfg.TPU_H), 12, cfg.TPU_H)
  val load_engine_ifm_unit = new load_engine(cfg, cfg.L1_AW, log2Up(cfg.TPU_H), 12, cfg.L1_DW, cfg.TPU_H, cfg.TPU_IDWMAX/8)
  val agu_compute_ker_unit = new agu_compute_ker(cfg, cfg.L1_AW, log2Up(cfg.TPU_W), 12, cfg.TPU_W)
  val load_engine_ker_unit = new load_engine(cfg, cfg.L1_AW, log2Up(cfg.TPU_W), 12, cfg.L1_DW, cfg.TPU_W, cfg.TPU_IDWMAX/8)
  val tensor_unit = new tensor_core(cfg,12)
  val store_engine_unit = new store_engine(cfg, cfg.L1_AW, log2Up(cfg.TPU_O), 12, cfg.L1_DW, cfg.TPU_O, cfg.TPU_ODWMAX/8)

  val L1_FAB = new L1_Fabric(cfg)
  val L1_IFM_0 = new sram_1R1W_wrap(cfg, cfg.L1_AW, cfg.L1_DW, cfg.L1_ISIZE, cfg.L1_BANK)
  val L1_KER_0 = new sram_1R1W_wrap(cfg, cfg.L1_AW, cfg.L1_DW, cfg.L1_KSIZE, cfg.L1_BANK)
  val L1_OFM_0 = new sram_1R1W_wrap(cfg, cfg.L1_AW, cfg.L1_DW, cfg.L1_OSIZE, cfg.L1_BANK)
  val L1_IFM_1 = new sram_1R1W_wrap(cfg, cfg.L1_AW, cfg.L1_DW, cfg.L1_ISIZE, cfg.L1_BANK)
  val L1_KER_1 = new sram_1R1W_wrap(cfg, cfg.L1_AW, cfg.L1_DW, cfg.L1_KSIZE, cfg.L1_BANK)
  val L1_OFM_1 = new sram_1R1W_wrap(cfg, cfg.L1_AW, cfg.L1_DW, cfg.L1_OSIZE, cfg.L1_BANK)

  // todo
  //val MEM_CMD = new sram_1R1W_wrap(cfg, cfg.CMD_AW, cfg.L1_DW, cfg.CMD_MSIZE, cfg.CMD_DW/cfg.L1_DW, cfg.CMD_BASE)
  val MEM_CMD = new sram_1R1W_wrap(cfg, cfg.CMD_AW, cfg.CMD_DW, cfg.CMD_MSIZE, 1)
  // =========
  // 1) 配置寄存器 & start
  // =========
  val flow_cmd_config_init = Flow(cmd_cfg(cfg))
  flow_cmd_config_init.valid := False
  flow_cmd_config_init.payload.assignFromBits(B(0,cmd_cfg(cfg).getBitsWidth bits))

  val axil_cfg = AxiLite4SlaveFactory(io.ctrl_axi)
  val cmd_config = Reg(Flow(cmd_cfg(cfg))) init(flow_cmd_config_init) // todo reginit
  val dma_cmd_req = Reg(Flow(DmaCmd(cfg.memAxi.addressWidth)))  // todo reginit

  axil_cfg.readAndWrite(cmd_config.payload.cmd_base, 0x10)
  axil_cfg.readAndWrite(cmd_config.payload.cmd_size, 0x18)
  axil_cfg.onWrite(0x1C) {  // 写这个地址表示“提交配置”
    cmd_config.valid := True
  }
  val fetch_start_pending = RegInit(False)
  val cfg_loading = RegInit(False)

  when(cfg_loading && dma_unit.io.dma_back.valid){
    cfg_loading := False
    fetch_start_pending := True
    cmd_config.valid := False
  } .elsewhen(cmd_config.valid){
    cfg_loading := True
  }

  axil_cfg.readAndWrite(cmd_config.payload.cmdcpy_en, 0x20) // address aligned
  axil_cfg.readAndWrite(cmd_config.payload.cmdcpy_src, 0x24)
  axil_cfg.readAndWrite(cmd_config.payload.cmdcpy_dst, 0x28)
  axil_cfg.readAndWrite(cmd_config.payload.cmdcpy_bytes, 0x2C)
  axil_cfg.readAndWrite(cmd_config.payload.cmdcpy_trans_len, 0x30)

  when(cmd_config.valid && !dma_unit.io.dma_back.valid){
    dma_cmd_req.payload.isLoad := True
    dma_cmd_req.payload.extAddr := cmd_config.cmdcpy_src
    dma_cmd_req.payload.l1Addr := cmd_config.cmdcpy_dst
    dma_cmd_req.payload.bytes := cmd_config.cmdcpy_bytes
    dma_cmd_req.payload.trans_len := cmd_config.cmdcpy_trans_len
    dma_cmd_req.valid := cmd_config.cmdcpy_en
  } .otherwise{
    dma_cmd_req.valid := False
  }

  when(fetch_unit.io.start_stream.fire) {
    fetch_start_pending := False
  }

  fetch_unit.io.cmd_config := cmd_config
  fetch_unit.io.start_stream.valid   := fetch_start_pending
  fetch_unit.io.start_stream.payload := True


  fetch_unit.io.flush_in := False // todo
  fetch_unit.io.stall_in := False // todo
  fetch_unit.io.cmd_stream_out <> decode_unit.io.cmd_stream_in
  fetch_unit.io.last_cmd_id <> scb_unit.io.last_cmd_id

  decode_unit.io.cmd_id_in := fetch_unit.io.cmd_id_out
  decode_unit.io.dec_out <> scb_unit.io.dec_in

  scb_unit.io.agu_dma_dispatch <> agu_dma_unit.io.agu_dma_dispatch
  scb_unit.io.agu_dma_back <> agu_dma_unit.io.agu_dma_back
  scb_unit.io.agu_compute_ifm_dispatch <> agu_compute_ifm_unit.io.agu_compute_dispatch
  scb_unit.io.agu_compute_ifm_back <> agu_compute_ifm_unit.io.agu_compute_back
  scb_unit.io.agu_compute_ker_dispatch <> agu_compute_ker_unit.io.agu_compute_dispatch
  scb_unit.io.agu_compute_ker_back <> agu_compute_ker_unit.io.agu_compute_back
  scb_unit.io.dma_dispatch <> dma_unit.io.dma_dispatch
  scb_unit.io.dma_back <> dma_unit.io.dma_back
  scb_unit.io.loop_ctrl_dispatch <> loop_ctrl_unit.io.loop_ctrl_dispatch
  scb_unit.io.tensor_dispatch <> tensor_unit.io.tensor_dispatch
  scb_unit.io.tensor_back <> tensor_unit.io.tensor_back
  scb_unit.io.store_engine_dispatch <> store_engine_unit.io.store_dispatch
  scb_unit.io.store_engine_back <> store_engine_unit.io.store_engine_back
  scb_unit.io.alu_int_dispatch <> alu_int_unit.io.alu_int_dispatch
  scb_unit.io.alu_int_back <> alu_int_unit.io.alu_int_back
  scb_unit.io.alu_fp_dispatch <> alu_fp_unit.io.alu_fp_dispatch
  scb_unit.io.alu_fp_back <> alu_fp_unit.io.alu_fp_back
  scb_unit.io.dma_agu_ctrl <> agu2dma_ctrl_unit.io.dma_agu_ctrl

  io.irq := scb_unit.io.all_task_done

  dma_unit.io.dma0_axi <> io.dma0_axi
  dma_unit.io.dma1_axi <> io.dma1_axi
  dma_unit.io.dma_cmd_req.payload := dma_cmd_req.payload
  dma_unit.io.dma_cmd_req.valid := dma_cmd_req.valid
  dma_unit.io.dma0_wr.ready := False
  dma_unit.io.dma0_rd.ready := False
  dma_unit.io.dma0_rd.rvalid := False
  dma_unit.io.dma0_rd.rdata := 0

  agu_dma_unit.io.dma_disp_cmd <> agu2dma_ctrl_unit.io.dma_disp_cmd
  agu2dma_ctrl_unit.io.dma_agu_dispatch <> dma_unit.io.dma_agu_dispatch

  loop_ctrl_unit.io.loop_ctrl.ready := agu_compute_ifm_unit.io.loop_ctrl.ready && agu_compute_ker_unit.io.loop_ctrl.ready
  agu_compute_ifm_unit.io.loop_ctrl.valid := loop_ctrl_unit.io.loop_ctrl.valid
  agu_compute_ifm_unit.io.loop_ctrl.payload := loop_ctrl_unit.io.loop_ctrl.payload
  agu_compute_ker_unit.io.loop_ctrl.valid := loop_ctrl_unit.io.loop_ctrl.valid
  agu_compute_ker_unit.io.loop_ctrl.payload := loop_ctrl_unit.io.loop_ctrl.payload

  agu_compute_ifm_unit.io.load_reqs <> load_engine_ifm_unit.io.load_reqs
  agu_compute_ker_unit.io.load_reqs <> load_engine_ker_unit.io.load_reqs

  load_engine_ifm_unit.io.packet_st <> tensor_unit.io.packet_ifm
  load_engine_ker_unit.io.packet_st <> tensor_unit.io.packet_ker
  tensor_unit.io.packet_ofm <> store_engine_unit.io.packet_st
  tensor_unit.io.OTILE_CFG := loop_ctrl_unit.io.OTILE_cfg

  load_engine_ifm_unit.io.load_engine_rd <> L1_FAB.io.load_ifm_rd
  load_engine_ker_unit.io.load_engine_rd <> L1_FAB.io.load_ker_rd
  store_engine_unit.io.store_engine_wr <> L1_FAB.io.store_engine_wr




  def CMD_MEM_OFF_L =  log2Up(cfg.DMA_DW/8)
  def CMD_MEM_OFF_H =  log2Up(cfg.CMD_DW/cfg.L1_DW)+log2Up(cfg.DMA_DW/8)
  when(cfg_loading){
    // cmd tie DMA0
    //MEM_CMD.io.sram_wif(0) <> dma_unit.io.dma0_wr
    //MEM_CMD.io.sram_rif(0) <> dma_unit.io.dma0_rd
    val widx = (dma_unit.io.dma0_wr.addr(CMD_MEM_OFF_H-1 downto CMD_MEM_OFF_L))
    val ridx = (dma_unit.io.dma0_rd.addr(CMD_MEM_OFF_H-1 downto CMD_MEM_OFF_L))
    dma_unit.io.dma0_wr.ready := MEM_CMD.io.sram_wif(0).ready
    dma_unit.io.dma0_rd.ready := MEM_CMD.io.sram_rif(0).ready
    MEM_CMD.io.sram_wif(0).valid := dma_unit.io.dma0_wr.valid
    MEM_CMD.io.sram_wif(0).addr := ((dma_unit.io.dma0_wr.addr - cfg.CMD_BASE)).resized
    MEM_CMD.io.sram_wif(0).data := B(dma_unit.io.dma0_wr.data,cfg.CMD_DW/cfg.L1_DW)
    MEM_CMD.io.sram_wif(0).strb := (dma_unit.io.dma0_wr.strb << (cfg.DMA_DW/8)*widx).resized
    MEM_CMD.io.sram_rif(0).valid := dma_unit.io.dma0_rd.valid
    MEM_CMD.io.sram_rif(0).addr := (dma_unit.io.dma0_rd.addr - cfg.CMD_BASE).resized
//    for (i <- 0 until cfg.CMD_DW/cfg.L1_DW) {
//      when(i===widx) {
//        dma_unit.io.dma0_wr.ready := MEM_CMD.io.sram_wif(i).ready
//        MEM_CMD.io.sram_wif(i).valid := dma_unit.io.dma0_wr.valid
//        MEM_CMD.io.sram_wif(i).addr := (dma_unit.io.dma0_wr.addr >> CMD_MEM_OFF_H).resized
//        MEM_CMD.io.sram_wif(i).data := dma_unit.io.dma0_wr.data
//        MEM_CMD.io.sram_wif(i).strb := dma_unit.io.dma0_wr.strb
//      } .otherwise{
//        MEM_CMD.io.sram_wif(i).valid := False
//        MEM_CMD.io.sram_wif(i).addr := 0
//        MEM_CMD.io.sram_wif(i).data := 0
//        MEM_CMD.io.sram_wif(i).strb := 0
//      }
//      when(i===ridx) {
//        dma_unit.io.dma0_rd.ready := MEM_CMD.io.sram_rif(i).ready
//        dma_unit.io.dma0_rd.rvalid := MEM_CMD.io.sram_rif(i).rvalid
//        dma_unit.io.dma0_rd.rdata := MEM_CMD.io.sram_rif(i).rdata
//        MEM_CMD.io.sram_rif(i).valid := dma_unit.io.dma0_rd.valid
//        MEM_CMD.io.sram_rif(i).addr := dma_unit.io.dma0_rd.addr.resized
//      } .otherwise{
//        MEM_CMD.io.sram_rif(i).valid := False
//        MEM_CMD.io.sram_rif(i).addr := 0
//      }
//    }
    L1_FAB.io.dma0_wr.valid := False
    L1_FAB.io.dma0_wr.addr := dma_unit.io.dma0_wr.addr
    L1_FAB.io.dma0_wr.data := dma_unit.io.dma0_wr.data
    L1_FAB.io.dma0_wr.strb := dma_unit.io.dma0_wr.strb
    L1_FAB.io.dma0_rd.valid := False
    L1_FAB.io.dma0_rd.addr := dma_unit.io.dma0_rd.addr
  } .otherwise{
    L1_FAB.io.dma0_wr <> dma_unit.io.dma0_wr
    L1_FAB.io.dma0_rd <> dma_unit.io.dma0_rd
    MEM_CMD.io.sram_wif(0).valid := False
    MEM_CMD.io.sram_wif(0).addr := 0
    MEM_CMD.io.sram_wif(0).data := 0
    MEM_CMD.io.sram_wif(0).strb := 0
    MEM_CMD.io.sram_rif(0).valid := fetch_unit.io.cmd_mem_if.mem_rd
    MEM_CMD.io.sram_rif(0).addr := fetch_unit.io.cmd_mem_if.mem_addr - cfg.CMD_BASE
//    for (i <- 0 until cfg.CMD_DW/cfg.L1_DW) {
//      MEM_CMD.io.sram_wif(i).valid := False
//      MEM_CMD.io.sram_wif(i).addr := 0
//      MEM_CMD.io.sram_wif(i).data := 0
//      MEM_CMD.io.sram_wif(i).strb := 0
//      MEM_CMD.io.sram_rif(i).valid := fetch_unit.io.cmd_mem_if.mem_rd
//      MEM_CMD.io.sram_rif(i).addr := fetch_unit.io.cmd_mem_if.mem_addr
//    }
  }
//  for (i <- 0 until cfg.CMD_DW/cfg.L1_DW) {
//    fetch_unit.io.cmd_mem_if.mem_rdata((i+1)*cfg.L1_DW-1 downto i*cfg.L1_DW) := MEM_CMD.io.sram_rif(i).rdata.asUInt
//  }
  fetch_unit.io.cmd_mem_if.mem_rdata := MEM_CMD.io.sram_rif(0).rdata.asUInt
  //L1_FAB.io.dma0_wr <> dma_unit.io.dma0_wr
  L1_FAB.io.dma1_wr <> dma_unit.io.dma1_wr
  //L1_FAB.io.dma0_rd <> dma_unit.io.dma0_rd
  L1_FAB.io.dma1_rd <> dma_unit.io.dma1_rd


  for (i <- 0 until cfg.L1_BANK) {
    L1_FAB.io.ifm_wr(i) <> L1_IFM_0.io.sram_wif(i)
    L1_FAB.io.ifm_rd(i) <> L1_IFM_0.io.sram_rif(i)
    L1_FAB.io.ker_wr(i) <> L1_KER_0.io.sram_wif(i)
    L1_FAB.io.ker_rd(i) <> L1_KER_0.io.sram_rif(i)
    L1_FAB.io.ofm_wr(i) <> L1_OFM_0.io.sram_wif(i)
    L1_FAB.io.ofm_rd(i) <> L1_OFM_0.io.sram_rif(i)
  }
  for (i <- cfg.L1_BANK until 2*cfg.L1_BANK) {
    L1_FAB.io.ifm_wr(i) <> L1_IFM_1.io.sram_wif(i - cfg.L1_BANK)
    L1_FAB.io.ifm_rd(i) <> L1_IFM_1.io.sram_rif(i - cfg.L1_BANK)
    L1_FAB.io.ker_wr(i) <> L1_KER_1.io.sram_wif(i - cfg.L1_BANK)
    L1_FAB.io.ker_rd(i) <> L1_KER_1.io.sram_rif(i - cfg.L1_BANK)
    L1_FAB.io.ofm_wr(i) <> L1_OFM_1.io.sram_wif(i - cfg.L1_BANK)
    L1_FAB.io.ofm_rd(i) <> L1_OFM_1.io.sram_rif(i - cfg.L1_BANK)
  }


}