import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.amba4.axi._
import config.TTConfig
import config._

class L1_Fabric_old(cfg: TTConfig) extends Component {
  val io = new Bundle {
    val dma0_wr = slave(sram_wport(cfg.L1_AW, cfg.L1_DW))
    val dma0_rd = slave(sram_rport(cfg.L1_AW, cfg.L1_DW))
    val dma1_wr = slave(sram_wport(cfg.L1_AW, cfg.L1_DW))
    val dma1_rd = slave(sram_rport(cfg.L1_AW, cfg.L1_DW))

    val ifm_wr   = Vec(master(sram_wport(cfg.L1_AW, cfg.L1_DW)), 2*cfg.L1_BANK)
    val ker_wr   = Vec(master(sram_wport(cfg.L1_AW, cfg.L1_DW)), 2*cfg.L1_BANK)
    val ofm_wr   = Vec(master(sram_wport(cfg.L1_AW, cfg.L1_DW)), 2*cfg.L1_BANK)

    val ifm_rd   = Vec(master(sram_rport(cfg.L1_AW, cfg.L1_DW)), 2*cfg.L1_BANK)
    val ker_rd   = Vec(master(sram_rport(cfg.L1_AW, cfg.L1_DW)), 2*cfg.L1_BANK)
    val ofm_rd   = Vec(master(sram_rport(cfg.L1_AW, cfg.L1_DW)), 2*cfg.L1_BANK)
  }

  // -------------------------
  // Decode helpers (建议后续改成 prefix decode)
  // -------------------------
  def isIFM(addr: UInt) = (addr >= U(cfg.L1_IBASE, cfg.L1_AW bits)) && (addr < U(cfg.L1_IBASE + cfg.L1_ISIZE, cfg.L1_AW bits))
  def isKER(addr: UInt) = (addr >= U(cfg.L1_KBASE, cfg.L1_AW bits)) && (addr < U(cfg.L1_KBASE + cfg.L1_KSIZE, cfg.L1_AW bits))
  def isOFM(addr: UInt) = (addr >= U(cfg.L1_OBASE, cfg.L1_AW bits)) && (addr < U(cfg.L1_OBASE + cfg.L1_OSIZE, cfg.L1_AW bits))

  println(f"IFM: ${cfg.L1_IBASE} ~ ${cfg.L1_IBASE + cfg.L1_ISIZE}")
  println(f"KER: ${cfg.L1_KBASE} ~ ${cfg.L1_KBASE + cfg.L1_KSIZE}")
  println(f"OFM: ${cfg.L1_OBASE} ~ ${cfg.L1_OBASE + cfg.L1_OSIZE}")

  val bankBits = log2Up(cfg.L1_BANK)
  val ppBit    = cfg.L1_PPBIT

  def ppSel(addr: UInt): UInt = addr(ppBit).asUInt // 0=ping,1=pong
  def bankSel(addr: UInt): UInt = addr(bankBits-1 downto 0)
  def ppBankIdx(pp: UInt, b: UInt): UInt = Cat(pp, b).asUInt

  def localAddrIFM(addr: UInt): UInt = (addr - U(cfg.L1_IBASE, cfg.L1_AW bits)).resized
  def localAddrKER(addr: UInt): UInt = (addr - U(cfg.L1_KBASE, cfg.L1_AW bits)).resized
  def localAddrOFM(addr: UInt): UInt = (addr - U(cfg.L1_OBASE, cfg.L1_AW bits)).resized

  // -------------------------
  // Defaults: clear all L1 ports (避免latch的核心：先赋默认值)
  // -------------------------
  for (p <- 0 until 2*cfg.L1_BANK) {
    // WR ports
    io.ifm_wr(p).valid := False
    io.ifm_wr(p).addr  := 0
    io.ifm_wr(p).data  := 0
    io.ifm_wr(p).strb  := 0

    io.ker_wr(p).valid := False
    io.ker_wr(p).addr  := 0
    io.ker_wr(p).data  := 0
    io.ker_wr(p).strb  := 0

    io.ofm_wr(p).valid := False
    io.ofm_wr(p).addr  := 0
    io.ofm_wr(p).data  := 0
    io.ofm_wr(p).strb  := 0

    // RD request ports
    io.ifm_rd(p).valid := False
    io.ifm_rd(p).addr  := 0

    io.ker_rd(p).valid := False
    io.ker_rd(p).addr  := 0

    io.ofm_rd(p).valid := False
    io.ofm_rd(p).addr  := 0
  }

  // DMA ready 默认 0（关键修复点：不命中/不grant 就不能 ready=1）
  io.dma0_wr.ready := False
  io.dma1_wr.ready := False
  io.dma0_rd.ready := False
  io.dma1_rd.ready := False

  // 新增：DMA读响应默认值（避免latch）
  io.dma0_rd.rvalid := False
  io.dma0_rd.rdata  := 0
  io.dma1_rd.rvalid := False
  io.dma1_rd.rdata  := 0

  // -------------------------
  // RR arbiter helper (2 inputs)
  // -------------------------
  def rrGrant(req0: Bool, req1: Bool, last: UInt): (Bool, Bool) = {
    val prefer1 = (last === 0)
    val g1 = (prefer1 && req1) || (!req0 && req1)
    val g0 = (!g1) && req0
    (g0, g1)
  }

  // 每个 target bank 一个 lastGrant（WR/RD 分开，IFM/KER/OFM 分开）
  val last_ifm_wr = Vec.fill(2*cfg.L1_BANK)(Reg(UInt(1 bits)) init(0))
  val last_ker_wr = Vec.fill(2*cfg.L1_BANK)(Reg(UInt(1 bits)) init(0))
  val last_ofm_wr = Vec.fill(2*cfg.L1_BANK)(Reg(UInt(1 bits)) init(0))

  val last_ifm_rd = Vec.fill(2*cfg.L1_BANK)(Reg(UInt(1 bits)) init(0))
  val last_ker_rd = Vec.fill(2*cfg.L1_BANK)(Reg(UInt(1 bits)) init(0))
  val last_ofm_rd = Vec.fill(2*cfg.L1_BANK)(Reg(UInt(1 bits)) init(0))

  // -------------------------
  // Per-bank WR arbitration: IFM/KER/OFM each
  // -------------------------
  def arb2Wr(
              d0: sram_wport, d1: sram_wport,
              t: Vec[sram_wport],
              last: Vec[UInt],
              isType: UInt => Bool,
              localAddr: UInt => UInt
            ): Unit = {
    for (pp <- 0 until 2) {
      for (b <- 0 until cfg.L1_BANK) {
        val idx = ppBankIdx(U(pp, 1 bits), U(b, bankBits bits))

        val d0_hit = d0.valid && isType(d0.addr) && (ppSel(d0.addr) === U(pp)) && (bankSel(d0.addr) === U(b, bankBits bits))
        val d1_hit = d1.valid && isType(d1.addr) && (ppSel(d1.addr) === U(pp)) && (bankSel(d1.addr) === U(b, bankBits bits))

        val (g0, g1) = rrGrant(d0_hit, d1_hit, last(idx))
        val tgtReady = t(idx).ready

        when(g0) { io.dma0_wr.ready := tgtReady }
        when(g1) { io.dma1_wr.ready := tgtReady }

        when(g0) {
          t(idx).valid := True
          t(idx).addr  := localAddr(d0.addr).resized
          t(idx).data  := d0.data
          t(idx).strb  := d0.strb
        } elsewhen(g1) {
          t(idx).valid := True
          t(idx).addr  := localAddr(d1.addr).resized
          t(idx).data  := d1.data
          t(idx).strb  := d1.strb
        }

        when(t(idx).valid && tgtReady) {
          last(idx) := Mux(g0, U(0, 1 bits), U(1, 1 bits))
        }
      }
    }
  }

  // 实例化 WR arb：IFM/KER/OFM
  arb2Wr(io.dma0_wr, io.dma1_wr, io.ifm_wr, last_ifm_wr, (a: UInt) => isIFM(a), (a: UInt) => localAddrIFM(a))
  arb2Wr(io.dma0_wr, io.dma1_wr, io.ker_wr, last_ker_wr, (a: UInt) => isKER(a), (a: UInt) => localAddrKER(a))
  arb2Wr(io.dma0_wr, io.dma1_wr, io.ofm_wr, last_ofm_wr, (a: UInt) => isOFM(a), (a: UInt) => localAddrOFM(a))

  // -------------------------
  // Per-bank RD request + response arbitration (补全rvalid/rdata回传)
  // -------------------------
  def arb2RdReq(
                 d0: sram_rport, d1: sram_rport,
                 t: Vec[sram_rport],
                 last: Vec[UInt],
                 isType: UInt => Bool,
                 localAddr: UInt => UInt
               ): Unit = {
    for (pp <- 0 until 2) {
      for (b <- 0 until cfg.L1_BANK) {
        val idx = ppBankIdx(U(pp, 1 bits), U(b, bankBits bits))

        // 1. 读请求命中判断
        val d0_hit = d0.valid && isType(d0.addr) && (ppSel(d0.addr) === U(pp)) && (bankSel(d0.addr) === U(b, bankBits bits))
        val d1_hit = d1.valid && isType(d1.addr) && (ppSel(d1.addr) === U(pp)) && (bankSel(d1.addr) === U(b, bankBits bits))

        // 2. RR仲裁
        val (g0, g1) = rrGrant(d0_hit, d1_hit, last(idx))
        val tgtReady = t(idx).ready

        // 3. 驱动DMA读就绪（和写逻辑一致）
        when(g0) { io.dma0_rd.ready := tgtReady }
        when(g1) { io.dma1_rd.ready := tgtReady }

        // 4. 驱动L1 Bank读请求
        when(g0) {
          t(idx).valid := True
          t(idx).addr  := localAddr(d0.addr).resized
        } elsewhen(g1) {
          t(idx).valid := True
          t(idx).addr  := localAddr(d1.addr).resized
        }

        // 5. 更新lastGrant（请求被接受时）
        when(t(idx).valid && tgtReady) {
          last(idx) := Mux(g0, U(0, 1 bits), U(1, 1 bits))
        }

        // -------------------------
        // 核心补全：读响应（rvalid/rdata）路由回DMA
        // -------------------------
        // 规则：只有当前bank被grant给某个DMA，且bank返回rvalid时，才把响应路由给对应DMA
        when(g0 && t(idx).rvalid) {
          io.dma0_rd.rvalid := True
          io.dma0_rd.rdata  := t(idx).rdata
        }
        when(g1 && t(idx).rvalid) {
          io.dma1_rd.rvalid := True
          io.dma1_rd.rdata  := t(idx).rdata
        }
      }
    }
  }

  // 实例化 RD arb：IFM/KER/OFM（响应会自动路由到对应DMA）
  arb2RdReq(io.dma0_rd, io.dma1_rd, io.ifm_rd, last_ifm_rd, (a: UInt) => isIFM(a), (a: UInt) => localAddrIFM(a))
  arb2RdReq(io.dma0_rd, io.dma1_rd, io.ker_rd, last_ker_rd, (a: UInt) => isKER(a), (a: UInt) => localAddrKER(a))
  arb2RdReq(io.dma0_rd, io.dma1_rd, io.ofm_rd, last_ofm_rd, (a: UInt) => isOFM(a), (a: UInt) => localAddrOFM(a))
}