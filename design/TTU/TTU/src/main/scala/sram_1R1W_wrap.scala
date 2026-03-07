import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.amba4.axi._
import config.TTConfig
import config._

class sram_1R1W_wrap(cfg: TTConfig, AW: Int, DW: Int, SIZE: Int, BANK: Int) extends Component {
  val io = new Bundle {
    val sram_wif = Vec(slave(sram_wport(AW,DW)), BANK)
    val sram_rif = Vec(slave(sram_rport(AW,DW)), BANK)
  }

  val RAM = List.fill(BANK*DW/8)(Mem(UInt(8 bits),SIZE/BANK))
  val we = Vec.fill(BANK)(Bool())
  val re = Vec.fill(BANK)(Bool())
  val rdata = Vec.fill(BANK)(Bits(DW bits))

  val bankBits    = if(BANK==1) 0 else log2Up(cfg.L1_BANK)
  // FIXED: skip line offset bits (byte addr -> line addr)
  val lineOffBits = log2Up(cfg.L1_DW / 8) // bytes per line
  // FIXED bankSel: select bank from [lineOffBits + bankBits -1 : lineOffBits]
  def bankSel(addr: UInt): UInt = addr(lineOffBits + bankBits - 1 downto lineOffBits)

  val waddr = Vec.fill(BANK)(UInt(log2Up(SIZE/BANK) bits))
  val raddr = Vec.fill(BANK)(UInt(log2Up(SIZE/BANK) bits))

  for (i <- 0 until BANK) {
    io.sram_wif(i).ready := True
    io.sram_rif(i).ready := True
    val bs = 0 //+ SIZE * i
    val es = SIZE - 1 //+ SIZE * i
    val bank_whit = if(BANK==1) True else (bankSel(io.sram_wif(i).addr) === i)
    val bank_rhit = if(BANK==1) True else (bankSel(io.sram_rif(i).addr) === i)
    waddr(i) := ((io.sram_wif(i).addr - bs) >> (lineOffBits + bankBits)).resize(log2Up(SIZE/BANK))
    raddr(i) := ((io.sram_rif(i).addr - bs) >> (lineOffBits + bankBits)).resize(log2Up(SIZE/BANK))
    we(i) := (io.sram_wif(i).addr >= bs) && (io.sram_wif(i).addr <= es) && io.sram_wif(i).valid && bank_whit
    re(i) := (io.sram_rif(i).addr >= bs) && (io.sram_rif(i).addr <= es) && io.sram_rif(i).valid && bank_rhit
    when(!we(i)){
      waddr(i) := 0
    }
    when(!re(i)){
      raddr(i) := 0
    }
    // todo wr conflict
    for (j <- 0 until DW/8) {
      val lsb = 8 * j
      val msb = lsb + 8 - 1

      RAM(i*DW/8+j).write(
        address = waddr(i),
        data = io.sram_wif(i).data.asUInt(msb downto lsb),
        enable = we(i) && io.sram_wif(i).strb(j)
      )
      rdata(i)(msb downto lsb) := RAM(i*DW/8+j).readSync(raddr(i), enable = re(i)).asBits
    }
    io.sram_rif(i).rdata := rdata(i)
    io.sram_rif(i).rvalid.setAsReg() init(False)
    io.sram_rif(i).rvalid := re(i)  //io.sram_rif(i).valid
  }

//  val re_id = re.sFindFirst(_ === True)
//  io.sram_rif.rdata := rdata(re_id._2)
//  io.sram_rif.rvalid.setAsReg() init(False)
//  io.sram_rif.rvalid := io.sram_rif.valid


}