package alu

import spinal.core._
import spinal.lib._
import config.TTConfig
import config._

/**
 * load_engine - Scheme S + next prefetch buffer (S+prefetch)
 *
 * Fixes in this version:
 *  1) loadActiveFrom() primes lane_done/data on SAME-CYCLE cache hit to remove the "stable 1-cycle bubble".
 *  2) next buffer becomes "elastic 1-deep": promote(NEXT->ACTIVE) can SAME-CYCLE refill NEXT from reqGrpFifo pop,
 *     so next_valid won't drop and create bubble.
 *  3) **IDLE scheduling fixed**: when grp_busy==0 and next_valid==1, ALWAYS promote NEXT->ACTIVE first.
 *     FIFO pop is only used to refill NEXT in that cycle, never to also feed ACTIVE -> prevents double loadActiveFrom().
 */
case class load_engine_old(
                        cfg: TTConfig,
                        aw: Int,
                        lw: Int,
                        pw: Int,
                        dw: Int,
                        lane_num: Int,
                        elem_bytes: Int
                      ) extends Component {

  val io = new Bundle {
    val load_reqs      = Vec(slave Stream(load_req(aw, lw, pw)), lane_num) // from AGU_COMPUTE
    val load_engine_rd = Vec(master(sram_rport(aw, dw)), cfg.L1_BANK)      // to/from L1 fabric
    val packet_st      = master(Stream(tpu_packet(pw, lane_num * elem_bytes * 8))) // to tensor core
  }

  // ------------------------------------------------------------
  // 0) helpers / asserts
  // ------------------------------------------------------------
  assert(elem_bytes * 8 == cfg.TPU_IDWMAX, "packet -> tensor core width mismatch!!!")

  val elemBits    = elem_bytes * 8
  val bankBits    = log2Up(cfg.L1_BANK)
  val lineOffBits = log2Up(dw / 8) // bytes per line

  def bankSel(addr: UInt): UInt =
    addr(lineOffBits + bankBits - 1 downto lineOffBits)

  def lineAddr(addr: UInt): UInt =
    (addr >> lineOffBits).resized

  def byteOff(addr: UInt): UInt =
    addr(lineOffBits - 1 downto 0)

  def extractElem(lineBits: Bits, offBytes: UInt): Bits = {
    val bytes = lineBits.subdivideIn(8 bits)
    val parts = (0 until /*elem_bytes*/1).map { i =>
      bytes((offBytes + U(i, offBytes.getWidth bits)).resized)
    }
    Cat(parts.reverse)
  }

  // ------------------------------------------------------------
  // 1) Output fifo
  // ------------------------------------------------------------
  val outFifo = StreamFifo(tpu_packet(pw, lane_num * elemBits), cfg.PACKET_DEPTH)
  io.packet_st << outFifo.io.pop

  outFifo.io.push.valid        := False
  outFifo.io.push.payload.pid  := 0
  outFifo.io.push.payload.data := B(0, lane_num * elemBits bits)

  // ------------------------------------------------------------
  // 2) LOAD_REQ FIFO: store one GROUP = lane_num reqs
  // ------------------------------------------------------------
  case class load_req_group() extends Bundle {
    val pid  = UInt(pw bits)
    val addr = Vec(UInt(aw bits), lane_num)
    val pad  = Vec(Bool(), lane_num)
  }

  val reqGrpFifo = StreamFifo(load_req_group(), cfg.LD_REQ_DEPTH)

  // push group from AGU (all-or-nothing)
  val inAllValid = io.load_reqs.map(_.valid).andR
  val canPushGrp = inAllValid && reqGrpFifo.io.push.ready

  for (l <- 0 until lane_num) {
    io.load_reqs(l).ready := canPushGrp
  }

  reqGrpFifo.io.push.valid := canPushGrp
  reqGrpFifo.io.push.payload.pid := io.load_reqs(0).payload.req_packetid.resize(pw)
  for (l <- 0 until lane_num) {
    reqGrpFifo.io.push.payload.addr(l) := io.load_reqs(l).payload.req_addr
    reqGrpFifo.io.push.payload.pad(l)  := io.load_reqs(l).payload.req_ispad
  }

  // ------------------------------------------------------------
  // 3) Per-bank request state: IDLE / PENDING / INFLIGHT
  // ------------------------------------------------------------
  val bank_pending  = Vec.fill(cfg.L1_BANK)(Reg(Bool()) init(False))
  val bank_inflight = Vec.fill(cfg.L1_BANK)(Reg(Bool()) init(False))

  val bank_pend_line = Vec(Reg(UInt(aw bits)) init(0), cfg.L1_BANK) // lineAddr
  val bank_req_addr  = Vec(Reg(UInt(aw bits)) init(0), cfg.L1_BANK) // BYTE addr(line base) to fabric

  // ------------------------------------------------------------
  // 4) Active group working regs (one group serviced at a time)
  // ------------------------------------------------------------
  val grp_busy   = Reg(Bool()) init(False)
  val req_pid_r  = Reg(UInt(pw bits)) init(0)
  val req_addr_r = Vec(Reg(UInt(aw bits)) init(0), lane_num)
  val req_pad_r  = Vec(Reg(Bool()) init(False), lane_num)

  val lane_done_r = Vec(Reg(Bool()) init(False), lane_num)
  val lane_data_r = Vec(Reg(Bits(elemBits bits)) init(0), lane_num)

  // ------------------------------------------------------------
  // 5) Next prefetch buffer (1-deep) - elastic
  // ------------------------------------------------------------
  val next_valid = Reg(Bool()) init(False)
  val next_pid   = Reg(UInt(pw bits)) init(0)
  val next_addr  = Vec(Reg(UInt(aw bits)) init(0), lane_num)
  val next_pad   = Vec(Reg(Bool()) init(False), lane_num)

  // ------------------------------------------------------------
  // 6) Per-bank 2-line cache (kept across groups)
  // ------------------------------------------------------------
  val cache_valid = Vec.fill(cfg.L1_BANK)(Vec(Reg(Bool()) init(False), 2))
  val cache_line  = Vec.fill(cfg.L1_BANK)(Vec(Reg(UInt(aw bits)) init(0), 2))
  val cache_data  = Vec.fill(cfg.L1_BANK)(Vec(Reg(Bits(dw bits)) init(0), 2))
  val repl_ptr    = Vec.fill(cfg.L1_BANK)(Reg(UInt(1 bits)) init(0)) // toggles 0/1

  def isCached2way(b: Int, lA: UInt): Bool = {
    val h0 = cache_valid(b)(0) && (cache_line(b)(0) === lA)
    val h1 = cache_valid(b)(1) && (cache_line(b)(1) === lA)
    h0 || h1
  }

  // ------------------------------------------------------------
  // 7) loadActiveFrom()  [FIX#1] prime lane_done/data on same-cycle cache hit
  // ------------------------------------------------------------

  def loadActiveFrom(payload: load_req_group): Unit = {
    grp_busy  := True
    req_pid_r := payload.pid

    for (l <- 0 until lane_num) {
      req_addr_r(l) := payload.addr(l)
      req_pad_r(l)  := payload.pad(l)

      when(payload.pad(l)) {
        lane_done_r(l) := True
        lane_data_r(l) := B(0, elemBits bits)
      } otherwise {
        val bSel = bankSel(payload.addr(l)).resize(bankBits)
        val lA   = lineAddr(payload.addr(l)).resize(aw)
        val off  = byteOff(payload.addr(l))

        val hit0 = cache_valid(bSel)(0) && (cache_line(bSel)(0) === lA)
        val hit1 = cache_valid(bSel)(1) && (cache_line(bSel)(1) === lA)

        when(hit0) {
          lane_done_r(l) := True
          lane_data_r(l) := extractElem(cache_data(bSel)(0), off).resize(elemBits)
        } elsewhen(hit1) {
          lane_done_r(l) := True
          lane_data_r(l) := extractElem(cache_data(bSel)(1), off).resize(elemBits)
        } otherwise {
          lane_done_r(l) := False
          // lane_data_r keep old (don't care until done)
        }
      }
    }

    // reset per-bank request states for this active group (cache kept!)
    for (b <- 0 until cfg.L1_BANK) {
      bank_pending(b)   := False
      bank_inflight(b)  := False
      bank_pend_line(b) := 0
      bank_req_addr(b)  := 0
    }
  }

  // ------------------------------------------------------------
  // 8) Push / promote signals
  // ------------------------------------------------------------
  val allDone   = lane_done_r.map(_ === True).andR
  val push_fire = grp_busy && allDone && outFifo.io.push.ready

  // promote after push (busy case)
  val willPromote = push_fire && next_valid

  // promote in IDLE (critical fix)
  val idlePromote = (!grp_busy) && next_valid

  // Snapshot old NEXT for any promote (avoid ordering issues with next_take)
  val old_next_pid  = next_pid
  val old_next_addr = next_addr
  val old_next_pad  = next_pad

  // ------------------------------------------------------------
  // 9) Group pop policy (FIX#3: IDLE priority to NEXT)
  // ------------------------------------------------------------
  // Pop to ACTIVE only if: not busy AND NEXT empty
  val pop_to_active_cond = (!grp_busy) && (!next_valid)

  // Pop to NEXT if:
  //  - busy and next empty, or busy and next being promoted (refill), OR
  //  - idlePromote (we promote next->active this cycle, allow refill next)
  val pop_to_next_cond =
    (grp_busy && (!next_valid || willPromote)) || idlePromote

  reqGrpFifo.io.pop.ready := pop_to_active_cond || pop_to_next_cond
  val pop_fire = reqGrpFifo.io.pop.fire

  // ONLY ONE place can call loadActiveFrom() per cycle:
  when(idlePromote) {
    val tmp = load_req_group()
    tmp.pid := old_next_pid
    for (l <- 0 until lane_num) {
      tmp.addr(l) := old_next_addr(l)
      tmp.pad(l)  := old_next_pad(l)
    }
    loadActiveFrom(tmp)
  } elsewhen(pop_fire && pop_to_active_cond) {
    loadActiveFrom(reqGrpFifo.io.pop.payload)
  }

  // ------------------------------------------------------------
  // 10) NEXT buffer update (single writer) - elastic refill
  // ------------------------------------------------------------
  val next_consume = idlePromote || willPromote
  val next_take    = pop_fire && pop_to_next_cond

  when(next_take) {
    next_pid := reqGrpFifo.io.pop.payload.pid
    for (l <- 0 until lane_num) {
      next_addr(l) := reqGrpFifo.io.pop.payload.addr(l)
      next_pad(l)  := reqGrpFifo.io.pop.payload.pad(l)
    }
  }

  next_valid := (next_valid && !next_consume) || next_take

  // ------------------------------------------------------------
  // 11) Drive fabric rports ONLY from per-bank regs (stable)
  // ------------------------------------------------------------
  for (b <- 0 until cfg.L1_BANK) {
    io.load_engine_rd(b).valid := bank_pending(b) || bank_inflight(b)
    io.load_engine_rd(b).addr  := bank_req_addr(b)
  }

  // ------------------------------------------------------------
  // 12) Capture responses -> fill 2-way cache; clear inflight
  // ------------------------------------------------------------
  for (b <- 0 until cfg.L1_BANK) {
    when(io.load_engine_rd(b).rvalid) {
      val w = repl_ptr(b)
      cache_valid(b)(w) := True
      cache_data(b)(w)  := io.load_engine_rd(b).rdata
      cache_line(b)(w)  := bank_pend_line(b)

      repl_ptr(b) := ~repl_ptr(b)

      bank_inflight(b) := False
      bank_pending(b)  := False
    }
  }

  // ------------------------------------------------------------
  // 13) Lane completion via cache hit (sticky set-only within active group)
  // ------------------------------------------------------------
  when(grp_busy) {
    for (l <- 0 until lane_num) {
      when(!lane_done_r(l) && !req_pad_r(l)) {
        val bSel = bankSel(req_addr_r(l)).resize(bankBits)
        val lA   = lineAddr(req_addr_r(l)).resize(aw)
        val off  = byteOff(req_addr_r(l))

        val hit0 = cache_valid(bSel)(0) && (cache_line(bSel)(0) === lA)
        val hit1 = cache_valid(bSel)(1) && (cache_line(bSel)(1) === lA)

        when(hit0) {
          lane_data_r(l) := extractElem(cache_data(bSel)(0), off).resize(elemBits)
          lane_done_r(l) := True
        } elsewhen(hit1) {
          lane_data_r(l) := extractElem(cache_data(bSel)(1), off).resize(elemBits)
          lane_done_r(l) := True
        }
      }
    }
  }

  // ------------------------------------------------------------
  // 14) Per-bank issue miss line: PENDING -> (ready) -> INFLIGHT
  // ------------------------------------------------------------
  when(grp_busy) {
    for (b <- 0 until cfg.L1_BANK) {

      // (A) pending -> inflight on ready
      when(bank_pending(b) && !bank_inflight(b)) {
        when(io.load_engine_rd(b).ready) {
          bank_inflight(b) := True
          bank_pending(b)  := False
        }
      }

      // (B) idle -> select one miss from lanes and latch as pending
      when(!bank_pending(b) && !bank_inflight(b)) {
        val cond   = Vec(Bool(), lane_num)
        val lAddrV = Vec(UInt(aw bits), lane_num)

        for (l <- 0 until lane_num) {
          val need = !lane_done_r(l) && !req_pad_r(l) &&
            (bankSel(req_addr_r(l)) === U(b, bankBits bits))
          val lA   = lineAddr(req_addr_r(l)).resize(aw)
          val miss = !isCached2way(b, lA)

          cond(l)   := need && miss
          lAddrV(l) := lA
        }

        val (found, idx) = cond.sFindFirst(_ === True)
        val selLine = UInt(aw bits)
        selLine := 0
        when(found) { selLine := lAddrV(idx) }

        val byteAddr = (selLine << lineOffBits).resized

        when(found) {
          bank_pending(b)   := True
          bank_pend_line(b) := selLine
          bank_req_addr(b)  := byteAddr
        }
      }
    }
  }

  // ------------------------------------------------------------
  // 15) When all lanes done, push packet; then promote NEXT->ACTIVE (busy case)
  // ------------------------------------------------------------
  when(push_fire) {
    outFifo.io.push.valid        := True
    outFifo.io.push.payload.pid  := req_pid_r
    outFifo.io.push.payload.data := Cat((0 until lane_num).map(l => lane_data_r(l)))

    when(next_valid) {
      val tmp = load_req_group()
      tmp.pid := old_next_pid
      for (l <- 0 until lane_num) {
        tmp.addr(l) := old_next_addr(l)
        tmp.pad(l)  := old_next_pad(l)
      }
      loadActiveFrom(tmp)
      // grp_busy stays True
    } otherwise {
      grp_busy := False
    }
  }
}