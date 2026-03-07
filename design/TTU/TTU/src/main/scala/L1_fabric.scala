import spinal.core._
import spinal.lib._
import config.TTConfig
import config._

class L1_Fabric(cfg: TTConfig) extends Component {
  val io = new Bundle {
    // DMA ports (2 masters)
    val dma0_wr = slave(sram_wport(cfg.L1_AW, cfg.L1_DW))
    val dma0_rd = slave(sram_rport(cfg.L1_AW, cfg.L1_DW))
    val dma1_wr = slave(sram_wport(cfg.L1_AW, cfg.L1_DW))
    val dma1_rd = slave(sram_rport(cfg.L1_AW, cfg.L1_DW))

    // NEW: two load engines (banked read ports)
    val load_ifm_rd = Vec(slave(sram_rport(cfg.L1_AW, cfg.L1_DW)), cfg.L1_BANK)
    val load_ker_rd = Vec(slave(sram_rport(cfg.L1_AW, cfg.L1_DW)), cfg.L1_BANK)

    // NEW: store engine (banked write ports)
    val store_engine_wr = Vec(slave(sram_wport(cfg.L1_AW, cfg.L1_DW)), cfg.L1_BANK)

    // L1 bank ports (2*BANK for ping/pong)
    val ifm_wr   = Vec(master(sram_wport(cfg.L1_AW, cfg.L1_DW)), 2*cfg.L1_BANK)
    val ker_wr   = Vec(master(sram_wport(cfg.L1_AW, cfg.L1_DW)), 2*cfg.L1_BANK)
    val ofm_wr   = Vec(master(sram_wport(cfg.L1_AW, cfg.L1_DW)), 2*cfg.L1_BANK)

    val ifm_rd   = Vec(master(sram_rport(cfg.L1_AW, cfg.L1_DW)), 2*cfg.L1_BANK)
    val ker_rd   = Vec(master(sram_rport(cfg.L1_AW, cfg.L1_DW)), 2*cfg.L1_BANK)
    val ofm_rd   = Vec(master(sram_rport(cfg.L1_AW, cfg.L1_DW)), 2*cfg.L1_BANK)
  }

  // -------------------------
  // Decode helpers
  // -------------------------
  def isIFM(addr: UInt) = (addr >= U(cfg.L1_IBASE, cfg.L1_AW bits)) && (addr < U(cfg.L1_IBASE + cfg.L1_ISIZE, cfg.L1_AW bits))
  def isKER(addr: UInt) = (addr >= U(cfg.L1_KBASE, cfg.L1_AW bits)) && (addr < U(cfg.L1_KBASE + cfg.L1_KSIZE, cfg.L1_AW bits))
  def isOFM(addr: UInt) = (addr >= U(cfg.L1_OBASE, cfg.L1_AW bits)) && (addr < U(cfg.L1_OBASE + cfg.L1_OSIZE, cfg.L1_AW bits))

  println(f"IFM: ${cfg.L1_IBASE} ~ ${cfg.L1_IBASE + cfg.L1_ISIZE}")
  println(f"KER: ${cfg.L1_KBASE} ~ ${cfg.L1_KBASE + cfg.L1_KSIZE}")
  println(f"OFM: ${cfg.L1_OBASE} ~ ${cfg.L1_OBASE + cfg.L1_OSIZE}")

  val bankBits    = log2Up(cfg.L1_BANK)
  val ppBit       = cfg.L1_PPBIT

  // FIXED: skip line offset bits (byte addr -> line addr)
  val lineOffBits = log2Up(cfg.L1_DW / 8) // bytes per line

  def ppSel(addr: UInt): UInt = addr(ppBit).asUInt // 0=ping,1=pong

  // FIXED bankSel: select bank from [lineOffBits + bankBits -1 : lineOffBits]
  def bankSel(addr: UInt): UInt = addr(lineOffBits + bankBits - 1 downto lineOffBits)

  def ppBankIdx(pp: UInt, b: UInt): UInt = Cat(pp, b).asUInt

  def localAddrIFM(addr: UInt): UInt = (addr - U(cfg.L1_IBASE, cfg.L1_AW bits)).resized
  def localAddrKER(addr: UInt): UInt = (addr - U(cfg.L1_KBASE, cfg.L1_AW bits)).resized
  def localAddrOFM(addr: UInt): UInt = (addr - U(cfg.L1_OBASE, cfg.L1_AW bits)).resized

  // -------------------------
  // Defaults: clear all L1 bank ports
  // -------------------------
  for (p <- 0 until 2*cfg.L1_BANK) {
    // WR
    io.ifm_wr(p).valid := False; io.ifm_wr(p).addr := 0; io.ifm_wr(p).data := 0; io.ifm_wr(p).strb := 0
    io.ker_wr(p).valid := False; io.ker_wr(p).addr := 0; io.ker_wr(p).data := 0; io.ker_wr(p).strb := 0
    io.ofm_wr(p).valid := False; io.ofm_wr(p).addr := 0; io.ofm_wr(p).data := 0; io.ofm_wr(p).strb := 0

    // RD req
    io.ifm_rd(p).valid := False; io.ifm_rd(p).addr := 0
    io.ker_rd(p).valid := False; io.ker_rd(p).addr := 0
    io.ofm_rd(p).valid := False; io.ofm_rd(p).addr := 0
  }

  // DMA defaults
  io.dma0_wr.ready := True  //False
  io.dma1_wr.ready := True  //False
  io.dma0_rd.ready := True  //False
  io.dma1_rd.ready := True  //False

  io.dma0_rd.rvalid := False
  io.dma0_rd.rdata  := 0
  io.dma1_rd.rvalid := False
  io.dma1_rd.rdata  := 0

  // LOAD IFM/KER defaults
  for(b <- 0 until cfg.L1_BANK){
    io.load_ifm_rd(b).ready  := True  //False
    io.load_ifm_rd(b).rvalid := False
    io.load_ifm_rd(b).rdata  := 0

    io.load_ker_rd(b).ready  := True  //False
    io.load_ker_rd(b).rvalid := False
    io.load_ker_rd(b).rdata  := 0
  }

  // STORE defaults
  for(b <- 0 until cfg.L1_BANK){
    io.store_engine_wr(b).ready := True  //False
  }

  // -------------------------
  // RR arbiters
  // -------------------------
  def rrGrant2(req0: Bool, req1: Bool, last: UInt): (Bool, Bool) = {
    val prefer1 = (last === 0)
    val g1 = (prefer1 && req1) || (!req0 && req1)
    val g0 = (!g1) && req0
    (g0, g1)
  }

  // last: 0/1/2
  def rrGrant3(r0: Bool, r1: Bool, r2: Bool, last: UInt): (Bool,Bool,Bool) = {
    val g0,g1,g2 = Bool()
    g0 := False; g1 := False; g2 := False
    switch(last){
      is(U(0,2 bits)){
        when(r1){ g1 := True } elsewhen(r2){ g2 := True } elsewhen(r0){ g0 := True }
      }
      is(U(1,2 bits)){
        when(r2){ g2 := True } elsewhen(r0){ g0 := True } elsewhen(r1){ g1 := True }
      }
      default{
        when(r0){ g0 := True } elsewhen(r1){ g1 := True } elsewhen(r2){ g2 := True }
      }
    }
    (g0,g1,g2)
  }

  // last: 0/1/2/3
  def rrGrant4(r0: Bool, r1: Bool, r2: Bool, r3: Bool, last: UInt):(Bool,Bool,Bool,Bool)={
    val g0,g1,g2,g3 = Bool()
    g0:=False; g1:=False; g2:=False; g3:=False

    def pick(o0:Bool,o1:Bool,o2:Bool,o3:Bool): Unit = {
      when(o2){ g2:=True }
        .elsewhen(o3){ g3:=True }
        .elsewhen(o0){ g0:=True }
        .elsewhen(o1){ g1:=True }
    }

    switch(last){ // todo : bug ,confuse order
//      is(U(0,2 bits)) { pick(r1,r2,r3,r0) }
//      is(U(1,2 bits)) { pick(r2,r3,r0,r1) }
//      is(U(2,2 bits)) { pick(r3,r0,r1,r2) }
      default         { pick(r0,r1,r2,r3) } // last==3
    }
    (g0,g1,g2,g3)
  }

  // -------------------------
  // lastGrant registers
  // -------------------------
  // 2-input WR (DMA only)
  val last_ifm_wr = Vec.fill(2*cfg.L1_BANK)(Reg(UInt(1 bits)) init(0))
  val last_ker_wr = Vec.fill(2*cfg.L1_BANK)(Reg(UInt(1 bits)) init(0))

  // 3-input WR (DMA0/DMA1/STORE) for OFM
  val last_ofm_wr = Vec.fill(2*cfg.L1_BANK)(Reg(UInt(2 bits)) init(0))

  // 4-input RD (DMA0/DMA1/LOAD_IFM/LOAD_KER)
  val last_ifm_rd = Vec.fill(2*cfg.L1_BANK)(Reg(UInt(2 bits)) init(0))
  val last_ker_rd = Vec.fill(2*cfg.L1_BANK)(Reg(UInt(2 bits)) init(0))
  val last_ofm_rd = Vec.fill(2*cfg.L1_BANK)(Reg(UInt(2 bits)) init(0))

  // -------------------------
  // arb2Wr: DMA0/DMA1 only
  // -------------------------
  def arb2Wr(
              d0: sram_wport, d1: sram_wport,
              t: Vec[sram_wport],
              last: Vec[UInt],
              isType: UInt => Bool,
              localAddr: UInt => UInt
            ): Unit = {
    for(pp <- 0 until 2){
      for(b <- 0 until cfg.L1_BANK){
        val idx = ppBankIdx(U(pp,1 bits), U(b,bankBits bits))

        val d0_hit = d0.valid && isType(d0.addr) && (ppSel(d0.addr)===U(pp)) && (bankSel(d0.addr)===U(b,bankBits bits))
        val d1_hit = d1.valid && isType(d1.addr) && (ppSel(d1.addr)===U(pp)) && (bankSel(d1.addr)===U(b,bankBits bits))

        val (g0,g1) = rrGrant2(d0_hit,d1_hit,last(idx))
        val tgtReady = t(idx).ready

        when(g0){ io.dma0_wr.ready := tgtReady }
        when(g1){ io.dma1_wr.ready := tgtReady }

        when(g0){
          t(idx).valid := True
          t(idx).addr  := localAddr(d0.addr).resized
          t(idx).data  := d0.data
          t(idx).strb  := d0.strb
        } elsewhen(g1){
          t(idx).valid := True
          t(idx).addr  := localAddr(d1.addr).resized
          t(idx).data  := d1.data
          t(idx).strb  := d1.strb
        }

        when(t(idx).valid && tgtReady){
          last(idx) := Mux(g0, U(0,1 bits), U(1,1 bits))
        }
      }
    }
  }

  // -------------------------
  // arb3Wr: DMA0/DMA1/STORE (STORE is banked)
  // STORE only allowed on OFM region by isType check
  // -------------------------
  def arb3Wr(
              d0: sram_wport, d1: sram_wport, s: Vec[sram_wport],
              t: Vec[sram_wport],
              last: Vec[UInt],
              isType: UInt => Bool,
              localAddr: UInt => UInt
            ): Unit = {
    for(pp <- 0 until 2){
      for(b <- 0 until cfg.L1_BANK){
        val idx = ppBankIdx(U(pp,1 bits), U(b,bankBits bits))

        val d0_hit = d0.valid && isType(d0.addr) && (ppSel(d0.addr)===U(pp)) && (bankSel(d0.addr)===U(b,bankBits bits))
        val d1_hit = d1.valid && isType(d1.addr) && (ppSel(d1.addr)===U(pp)) && (bankSel(d1.addr)===U(b,bankBits bits))
        val s_hit  = s(b).valid && isType(s(b).addr) && (ppSel(s(b).addr)===U(pp)) && (bankSel(s(b).addr)===U(b,bankBits bits))

        val (g0,g1,gs) = rrGrant3(d0_hit,d1_hit,s_hit,last(idx))
        val tgtReady = t(idx).ready

        when(g0){ io.dma0_wr.ready := tgtReady }
        when(g1){ io.dma1_wr.ready := tgtReady }
        when(gs){ s(b).ready       := tgtReady }

        when(g0){
          t(idx).valid := True
          t(idx).addr  := localAddr(d0.addr).resized
          t(idx).data  := d0.data
          t(idx).strb  := d0.strb
        } elsewhen(g1){
          t(idx).valid := True
          t(idx).addr  := localAddr(d1.addr).resized
          t(idx).data  := d1.data
          t(idx).strb  := d1.strb
        } elsewhen(gs){
          t(idx).valid := True
          t(idx).addr  := localAddr(s(b).addr).resized
          t(idx).data  := s(b).data
          t(idx).strb  := s(b).strb
        }

        when(t(idx).valid && tgtReady){
          last(idx) := Mux(g0,U(0,2 bits),Mux(g1,U(1,2 bits),U(2,2 bits)))
        }
      }
    }
  }

  // -------------------------
  // arb4RdReq: DMA0/DMA1/LOAD_IFM/LOAD_KER
  // Access restriction:
  //  - LOAD_IFM only allowed when allowLifm = True
  //  - LOAD_KER only allowed when allowLker = True
  // -------------------------
  def arb4RdReq(
                 d0: sram_rport, d1: sram_rport,
                 lifm: Vec[sram_rport], lker: Vec[sram_rport],
                 t: Vec[sram_rport],
                 last: Vec[UInt],
                 isType: UInt => Bool,
                 localAddr: UInt => UInt,
                 allowLifm: Boolean,
                 allowLker: Boolean
               ): Unit = {
    for(pp <- 0 until 2){
      for(b <- 0 until cfg.L1_BANK){
        val idx = ppBankIdx(U(pp,1 bits), U(b,bankBits bits))

        val d0_hit = d0.valid && isType(d0.addr) && (ppSel(d0.addr)===U(pp)) && (bankSel(d0.addr)===U(b,bankBits bits))
        val d1_hit = d1.valid && isType(d1.addr) && (ppSel(d1.addr)===U(pp)) && (bankSel(d1.addr)===U(b,bankBits bits))

        val lifm_hit = if(allowLifm) {
          lifm(b).valid && isType(lifm(b).addr) && (ppSel(lifm(b).addr)===U(pp)) && (bankSel(lifm(b).addr)===U(b,bankBits bits))
        } else False

        val lker_hit = if(allowLker) {
          lker(b).valid && isType(lker(b).addr) && (ppSel(lker(b).addr)===U(pp)) && (bankSel(lker(b).addr)===U(b,bankBits bits))
        } else False

        val (g0,g1,g2,g3) = rrGrant4(d0_hit,d1_hit,lifm_hit,lker_hit,last(idx))
        val tgtReady = t(idx).ready

        when(g0){ io.dma0_rd.ready := tgtReady }
        when(g1){ io.dma1_rd.ready := tgtReady }
        when(g2){ lifm(b).ready    := tgtReady }
        when(g3){ lker(b).ready    := tgtReady }

        when(g0){
          t(idx).valid := True
          t(idx).addr  := localAddr(d0.addr).resized
        } elsewhen(g1){
          t(idx).valid := True
          t(idx).addr  := localAddr(d1.addr).resized
        } elsewhen(g2){
          t(idx).valid := True
          t(idx).addr  := localAddr(lifm(b).addr).resized
        } elsewhen(g3){
          t(idx).valid := True
          t(idx).addr  := localAddr(lker(b).addr).resized
        }

        when(t(idx).valid && tgtReady){
          last(idx) := Mux(g0,U(0,2 bits),
            Mux(g1,U(1,2 bits),
              Mux(g2,U(2,2 bits),U(3,2 bits))))
        }
        // todo
        val g0_d1 = RegNext(g0) init(False)
        val g1_d1 = RegNext(g1) init(False)
        val g2_d1 = RegNext(g2) init(False)
        val g3_d1 = RegNext(g3) init(False)

        when(/*g0*/g0_d1 && t(idx).rvalid){ io.dma0_rd.rvalid := True; io.dma0_rd.rdata := t(idx).rdata }
        when(/*g1*/g1_d1 && t(idx).rvalid){ io.dma1_rd.rvalid := True; io.dma1_rd.rdata := t(idx).rdata }
        when(/*g2*/g2_d1 && t(idx).rvalid){ lifm(b).rvalid    := True; lifm(b).rdata    := t(idx).rdata }
        when(/*g3*/g3_d1 && t(idx).rvalid){ lker(b).rvalid    := True; lker(b).rdata    := t(idx).rdata }
      }
    }
  }

  // -------------------------
  // Instantiate WR arbitration
  // -------------------------
  // IFM/KER writes: DMA0/DMA1 only
  arb2Wr(io.dma0_wr, io.dma1_wr, io.ifm_wr, last_ifm_wr, isIFM, localAddrIFM)
  arb2Wr(io.dma0_wr, io.dma1_wr, io.ker_wr, last_ker_wr, isKER, localAddrKER)

  // OFM writes: DMA0/DMA1 + STORE_ENGINE (store_engine_wr is banked)
  arb3Wr(io.dma0_wr, io.dma1_wr, io.store_engine_wr, io.ofm_wr, last_ofm_wr, isOFM, localAddrOFM)

  // -------------------------
  // Instantiate RD arbitration with access restriction
  // -------------------------
  // IFM reads: allow load_ifm only
  arb4RdReq(io.dma0_rd, io.dma1_rd, io.load_ifm_rd, io.load_ker_rd,
    io.ifm_rd, last_ifm_rd, isIFM, localAddrIFM,
    allowLifm = true, allowLker = false
  )

  // KER reads: allow load_ker only
  arb4RdReq(io.dma0_rd, io.dma1_rd, io.load_ifm_rd, io.load_ker_rd,
    io.ker_rd, last_ker_rd, isKER, localAddrKER,
    allowLifm = false, allowLker = true
  )

  // OFM reads: disallow both load engines (DMA only)
  arb4RdReq(io.dma0_rd, io.dma1_rd, io.load_ifm_rd, io.load_ker_rd,
    io.ofm_rd, last_ofm_rd, isOFM, localAddrOFM,
    allowLifm = false, allowLker = false
  )
}