import spinal.core._
import spinal.lib._
import config._
// todo irq ==>
case class fetch(cfg: TTConfig) extends Component {
  val io = new Bundle {
    val flush_in = in Bool()
    val stall_in = in Bool()
    val start_stream = slave Stream(Bool())
    val cmd_config = slave Flow(cmd_cfg(cfg))
    val cmd_mem_if = master(mem_if(cfg, cfg.CMD_AW, cfg.CMD_DW))
    val cmd_id_out = out Bits (cfg.CMD_AW bits)
    val cmd_stream_out = master Stream (Bits(cfg.CMD_DW bits))
    val last_cmd_id = master Flow(UInt(cfg.CMD_AW bits))
  }

  def MEM_RDLY = 1  // fetch instr rdata delay, min=1
  val cmd_fifo = StreamFifo(Bits(cfg.CMD_DW+cfg.CMD_AW bits), cfg.CMD_DEEPTH)
  val fetch_en = Reg(Bool()) init(False)

  io.cmd_mem_if.mem_rd.setAsReg() init(False)
  io.cmd_mem_if.mem_wr.setAsReg() init(False)
  io.cmd_mem_if.mem_addr.setAsReg() init(0)
  io.cmd_mem_if.mem_wdata.setAsReg() init(0)
  io.cmd_mem_if.mem_wmask.setAsReg() init(0)
  val flow_init = Flow(cmd_cfg(cfg))
  flow_init.valid := False
  flow_init.payload.assignFromBits(B(0,cmd_cfg(cfg).getBitsWidth bits))
  val flow_cr_init = Flow(Bits(cfg.CMD_AW bits))
  flow_cr_init.valid := False
  flow_cr_init.payload.assignFromBits(B(0,cfg.CMD_AW bits))
  val cmd_cfg_r = Reg(Flow(cmd_cfg(cfg))) init(flow_init)
  val cmd_fifo_near_full = (cmd_fifo.io.occupancy >= cfg.CMD_DEEPTH-1-MEM_RDLY)
  val cmd_cnt = Reg(UInt(cfg.CMD_AW bits)) init(0)
  val cmd_fetch_done = (cmd_cnt === io.cmd_config.payload.cmd_size.asUInt-1)

  io.last_cmd_id.valid.setAsReg() init(False)
  io.last_cmd_id.payload.setAsReg() init(0)

  when(io.flush_in){
    io.start_stream.ready := False
  } .otherwise{
    io.start_stream.ready := True
  }

  when(io.flush_in || cmd_fetch_done){
    io.cmd_mem_if.mem_rd := False
    io.cmd_mem_if.mem_wr := False
    io.cmd_mem_if.mem_addr := 0
    fetch_en := False
    cmd_cnt := 0
  } .elsewhen(io.start_stream.fire){
    fetch_en := True
    cmd_cnt := 0
    cmd_cfg_r := io.cmd_config
    io.cmd_mem_if.mem_rd := True
    io.cmd_mem_if.mem_addr := (io.cmd_config.payload.cmd_base >> (log2Up(cfg.CMD_DW/8))).asUInt.resized
  } .elsewhen(!(io.stall_in || cmd_fifo_near_full || cmd_fetch_done) && fetch_en){  // todo cmd_size
    io.cmd_mem_if.mem_rd := True
    io.cmd_mem_if.mem_addr := io.cmd_mem_if.mem_addr + cfg.CMD_WB
    cmd_cnt := cmd_cnt + 1
  } .otherwise{
    io.cmd_mem_if.mem_rd := False
  }

  when(io.cmd_config.valid || io.flush_in){
    io.last_cmd_id.valid := False
  } .elsewhen(cmd_fetch_done){
    io.last_cmd_id.valid := True
    io.last_cmd_id.payload := (io.cmd_mem_if.mem_addr >> (log2Up(cfg.CMD_DW/8))).resize(cfg.CMD_AW)
  }

  val cmd_mem_rvalid = Vec.fill(MEM_RDLY)(Reg(Flow(Bits(cfg.CMD_AW bits))) init(flow_cr_init))
  val cmd_rdata_stream = Stream(Bits(cfg.CMD_DW+cfg.CMD_AW bits))

  for (i <- 0 until MEM_RDLY) {
    if(i==0) {
      cmd_mem_rvalid(i).valid := io.cmd_mem_if.mem_rd
      cmd_mem_rvalid(i).payload := (io.cmd_mem_if.mem_addr >> (log2Up(cfg.CMD_DW/8))).resize(cfg.CMD_AW).asBits
    } else {
      cmd_mem_rvalid(i) := cmd_mem_rvalid(i-1)
    }
  }
  cmd_rdata_stream.valid := cmd_mem_rvalid(MEM_RDLY-1).valid
  cmd_rdata_stream.payload := io.cmd_mem_if.mem_rdata ## cmd_mem_rvalid(MEM_RDLY-1).payload

  val cmd_stream = Stream(Bits(cfg.CMD_DW+cfg.CMD_AW bits))
  cmd_stream.ready := io.cmd_stream_out.ready
  io.cmd_stream_out.valid := cmd_stream.valid
  io.cmd_stream_out.payload := cmd_stream.payload(cfg.CMD_DW+cfg.CMD_AW-1 downto cfg.CMD_AW)
  io.cmd_id_out := cmd_stream.payload(cfg.CMD_AW-1 downto 0)

  cmd_fifo.io.push << cmd_rdata_stream
  cmd_fifo.io.pop >> cmd_stream

}