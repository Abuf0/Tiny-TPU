package alu

import spinal.core._
import spinal.lib._
import config.TTConfig
import config._

/**
 * load_engine - Windowed issue (multi-group in-flight) + per-bank configurable multi-outstanding
 *
 * Changes vs previous (1 outstanding/bank):
 *  - Each bank has an outstanding TAG FIFO with depth BANK_OSD (configurable).
 *  - Bank can keep issuing as long as (tagFifoCount < BANK_OSD) AND rport.ready.
 *  - rvalid pops one tag (in-order per bank) and completes the matching lane.
 *
 * Key behavior:
 *  - NEVER wait for rvalid to start issuing next group.
 *  - Maintain a small in-flight WINDOW of groups (WIN_DEPTH).
 *  - Still respects bank conflicts: a bank issues one request per cycle max (because one rport),
 *    but can pipeline multiple outstanding to hide latency.
 *
 * Assumptions:
 *  - sram_rport responses are IN-ORDER per bank (typical SRAM read pipeline).
 *  - One read request per bank per cycle max (single rport).
 */
case class load_engine(
                        cfg: TTConfig,
                        aw: Int,
                        lw: Int,
                        pw: Int,
                        dw: Int,
                        lane_num: Int,
                        elem_bytes: Int
                      ) extends Component {

  val io = new Bundle {
    val load_reqs      = Vec(slave Stream(load_req(aw, lw, pw)), lane_num)
    val load_engine_rd = Vec(master(sram_rport(aw, dw)), cfg.L1_BANK)
    val packet_st      = master(Stream(tpu_packet(pw, lane_num * elem_bytes * 8)))
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
    // todo parts cat
    val parts = (0 until 1/*elem_bytes*/).map { i =>
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
  outFifo.io.push.payload.oh  := 0
  outFifo.io.push.payload.ow  := 0
  outFifo.io.push.payload.oc  := 0
  outFifo.io.push.payload.data := B(0, lane_num * elemBits bits)

  // ------------------------------------------------------------
  // 2) LOAD_REQ FIFO: store one GROUP = lane_num reqs
  // ------------------------------------------------------------
  case class load_req_group() extends Bundle {
    val pid  = UInt(pw bits)
    val addr = Vec(UInt(aw bits), lane_num)
    val pad  = Vec(Bool(), lane_num)
    val oh  = UInt(pw bits)
    val ow  = UInt(pw bits)
    val oc  = UInt(pw bits)
  }

  val reqGrpFifo = StreamFifo(load_req_group(), cfg.LD_REQ_DEPTH)

  val inAllValid = io.load_reqs.map(_.valid).andR
  val canPushGrp = inAllValid && reqGrpFifo.io.push.ready
  for (l <- 0 until lane_num) io.load_reqs(l).ready := canPushGrp

  reqGrpFifo.io.push.valid := canPushGrp
  reqGrpFifo.io.push.payload.pid := io.load_reqs(0).payload.req_packetid.resize(pw)
  reqGrpFifo.io.push.payload.oh := io.load_reqs(0).payload.req_oh.resize(pw)
  reqGrpFifo.io.push.payload.ow := io.load_reqs(0).payload.req_ow.resize(pw)
  reqGrpFifo.io.push.payload.oc := io.load_reqs(0).payload.req_oc.resize(pw)
  for (l <- 0 until lane_num) {
    reqGrpFifo.io.push.payload.addr(l) := io.load_reqs(l).payload.req_addr
    reqGrpFifo.io.push.payload.pad(l)  := io.load_reqs(l).payload.req_ispad
  }

  // ------------------------------------------------------------
  // 3) Per-bank 2-way cache (kept across groups)
  // ------------------------------------------------------------
  val cache_valid = Vec.fill(cfg.L1_BANK)(Vec(Reg(Bool()) init(False), 2))
  val cache_line  = Vec.fill(cfg.L1_BANK)(Vec(Reg(UInt(aw bits)) init(0), 2))
  val cache_data  = Vec.fill(cfg.L1_BANK)(Vec(Reg(Bits(dw bits)) init(0), 2))
  val repl_ptr    = Vec.fill(cfg.L1_BANK)(Reg(UInt(1 bits)) init(0))

  def isCached2way(b: Int, lA: UInt): Bool = {
    (cache_valid(b)(0) && (cache_line(b)(0) === lA)) ||
      (cache_valid(b)(1) && (cache_line(b)(1) === lA))
  }

  // ------------------------------------------------------------
  // 4) WINDOW of in-flight groups (ring buffer)
  // ------------------------------------------------------------
  val WIN_DEPTH = 8
  val winPtrW   = log2Up(WIN_DEPTH)

  val win_valid = Vec.fill(WIN_DEPTH)(Reg(Bool()) init(False))
  val win_pid   = Vec.fill(WIN_DEPTH)(Reg(UInt(pw bits)) init(0))
  val win_oh    = Vec.fill(WIN_DEPTH)(Reg(UInt(pw bits)) init(0))
  val win_ow    = Vec.fill(WIN_DEPTH)(Reg(UInt(pw bits)) init(0))
  val win_oc    = Vec.fill(WIN_DEPTH)(Reg(UInt(pw bits)) init(0))
  val win_addr  = Vec.fill(WIN_DEPTH)(Vec(Reg(UInt(aw bits)) init(0), lane_num))
  val win_pad   = Vec.fill(WIN_DEPTH)(Vec(Reg(Bool()) init(False), lane_num))

  val lane_done = Vec.fill(WIN_DEPTH)(Vec(Reg(Bool()) init(False), lane_num))
  val lane_wait = Vec.fill(WIN_DEPTH)(Vec(Reg(Bool()) init(False), lane_num)) // issued, waiting any of outstanding returns
  val lane_data = Vec.fill(WIN_DEPTH)(Vec(Reg(Bits(elemBits bits)) init(0), lane_num))

  val lane_bank = Vec.fill(WIN_DEPTH)(Vec(Reg(UInt(bankBits bits)) init(0), lane_num))
  val lane_line = Vec.fill(WIN_DEPTH)(Vec(Reg(UInt(aw bits)) init(0), lane_num))
  val lane_off  = Vec.fill(WIN_DEPTH)(Vec(Reg(UInt(lineOffBits bits)) init(0), lane_num))

  val q_head  = Reg(UInt(winPtrW bits)) init(0)
  val q_tail  = Reg(UInt(winPtrW bits)) init(0)
  val q_count = UInt(log2Up(WIN_DEPTH + 1) bits) //Reg(UInt(log2Up(WIN_DEPTH + 1) bits)) init(0)

  when(q_head > q_tail){
    q_count := WIN_DEPTH + q_tail - q_head
  } .otherwise{
    q_count := (q_tail - q_head).resized
  }

  def qFull  = q_count === WIN_DEPTH-1
  def qEmpty = q_count === 0

  // ------------------------------------------------------------
  // 5) Allocate new group into window whenever there is space
  // ------------------------------------------------------------
  reqGrpFifo.io.pop.ready := !qFull

  when(reqGrpFifo.io.pop.fire) {
    val slot = q_tail

    win_valid(slot) := True
    win_pid(slot)   := reqGrpFifo.io.pop.payload.pid
    win_oh(slot)   := reqGrpFifo.io.pop.payload.oh
    win_ow(slot)   := reqGrpFifo.io.pop.payload.ow
    win_oc(slot)   := reqGrpFifo.io.pop.payload.oc

    for (l <- 0 until lane_num) {
      val a   = reqGrpFifo.io.pop.payload.addr(l)
      val p   = reqGrpFifo.io.pop.payload.pad(l)
      val bS  = bankSel(a).resize(bankBits)
      val lA  = lineAddr(a).resize(aw)
      val off = byteOff(a).resize(lineOffBits)

      win_addr(slot)(l) := a
      win_pad(slot)(l)  := p

      lane_bank(slot)(l) := bS
      lane_line(slot)(l) := lA
      lane_off(slot)(l)  := off

      when(p) {
        lane_done(slot)(l) := True
        lane_wait(slot)(l) := False
        lane_data(slot)(l) := B(0, elemBits bits)
      } otherwise {
        // prime on same-cycle cache hit
        val h0 = cache_valid(bS)(0) && (cache_line(bS)(0) === lA)
        val h1 = cache_valid(bS)(1) && (cache_line(bS)(1) === lA)
        when(h0) {
          lane_done(slot)(l) := True
          lane_wait(slot)(l) := False
          lane_data(slot)(l) := extractElem(cache_data(bS)(0), off).resize(elemBits)
        } elsewhen (h1) {
          lane_done(slot)(l) := True
          lane_wait(slot)(l) := False
          lane_data(slot)(l) := extractElem(cache_data(bS)(1), off).resize(elemBits)
        } otherwise {
          lane_done(slot)(l) := False
          lane_wait(slot)(l) := False
        }
      }
    }

    q_tail  := q_tail + 1
    //q_count := q_count + 1
  }

  // ------------------------------------------------------------
  // 6) Per-bank outstanding TAG FIFO (configurable)
  // ------------------------------------------------------------
  // You can tune this. Recommend >= (fabric read latency) to fully hide bubbles.
  val BANK_OSD = 4
  require(BANK_OSD >= 1, "BANK_OSD must be >= 1")
  val osPtrW = log2Up(BANK_OSD)

  val bank_os_count = Vec.fill(cfg.L1_BANK)(Reg(UInt(log2Up(BANK_OSD + 1) bits)) init(0))
  val bank_os_head  = Vec.fill(cfg.L1_BANK)(Reg(UInt(osPtrW bits)) init(0))
  val bank_os_tail  = Vec.fill(cfg.L1_BANK)(Reg(UInt(osPtrW bits)) init(0))

  val bank_os_slot  = Vec.fill(cfg.L1_BANK)(Vec(Reg(UInt(winPtrW bits)) init(0), BANK_OSD))
  val bank_os_lane  = Vec.fill(cfg.L1_BANK)(Vec(Reg(UInt(log2Up(lane_num) bits)) init(0), BANK_OSD))
  val bank_os_line  = Vec.fill(cfg.L1_BANK)(Vec(Reg(UInt(aw bits)) init(0), BANK_OSD))

  def bankOsFull(b: Int): Bool  = bank_os_count(b) === BANK_OSD
  def bankOsEmpty(b: Int): Bool = bank_os_count(b) === 0

  // ------------------------------------------------------------
  // 7) Default L1 rport drive
  // ------------------------------------------------------------
  // helper: slot at age k
  def slotAtAge(k: Int): UInt = (q_head + U(k, winPtrW bits)).resized

  // ------------------------------------------------------------
  // 8) Issue logic: per bank can issue every cycle if ready and outstanding not full
  // ------------------------------------------------------------
  // -----------------------------------------------
  // Per-bank issue: build candidates then sFindFirst
  // -----------------------------------------------

  //  for (b <- 0 until cfg.L1_BANK) {
  //    io.load_engine_rd(b).valid := False
  //    io.load_engine_rd(b).addr  := 0
  //  }
//  for (b <- 0 until cfg.L1_BANK) {
//    when(!bankOsFull(b)) {
//
//      val N = WIN_DEPTH * lane_num
//
//      val cand      = Vec(Bool(), N)
//      val candSlot  = Vec(UInt(winPtrW bits), N)
//      val candLane  = Vec(UInt(log2Up(lane_num) bits), N)
//      val candLine  = Vec(UInt(aw bits), N)
//
//      // defaults
//      for (i <- 0 until N) {
//        cand(i)     := False
//        candSlot(i) := 0
//        candLane(i) := 0
//        candLine(i) := 0
//      }
//
//      // oldest-first across window, lane order 0..lane_num-1
//      for (k <- 0 until WIN_DEPTH) {
//        val s = slotAtAge(k) // UInt(winPtrW bits)
//        when(k < q_count && win_valid(s)) {
//          for (l <- 0 until lane_num) {
//            val idx  = k * lane_num + l
//            val need = !win_pad(s)(l) && !lane_done(s)(l) //&& !lane_wait(s)(l)
//            val inBk = lane_bank(s)(l) === U(b, bankBits bits)
//            val miss = !isCached2way(b, lane_line(s)(l))
//
//            cand(idx)     := need && inBk && miss
//            candSlot(idx) := s
//            candLane(idx) := U(l, log2Up(lane_num) bits)
//            candLine(idx) := lane_line(s)(l)
//          }
//        }
//      }
//
//      val (found, fidx) = cand.sFindFirst(_ === True)
//
//      when(found) {
//        val selSlot = candSlot(fidx)
//        val selLane = candLane(fidx)
//        val selLine = candLine(fidx)
//
//        io.load_engine_rd(b).valid := True
//        io.load_engine_rd(b).addr  := (selLine << lineOffBits).resized
//
//        when(io.load_engine_rd(b).ready) {
//          val t = bank_os_tail(b)
//
//          bank_os_slot(b)(t) := selSlot
//          bank_os_lane(b)(t) := selLane
//          bank_os_line(b)(t) := selLine
//
//          bank_os_tail(b)  := bank_os_tail(b) + 1
//          bank_os_count(b) := bank_os_count(b) + 1
//
//          lane_wait(selSlot)(selLane) := True
//        }
//      }
//    }
//  }

  // 每 bank 一个 pending latch（1-deep），用于 hold 住一次发射，直到 ready
  val pendV    = Vec.fill(cfg.L1_BANK)(Reg(Bool()) init(False))
  val pendSlot = Vec(Reg(UInt(winPtrW bits)) init(0), cfg.L1_BANK)
  val pendLane = Vec(Reg(UInt(log2Up(lane_num) bits)) init(0), cfg.L1_BANK)
  val pendLine = Vec(Reg(UInt(aw bits)) init(0), cfg.L1_BANK)

  // 默认输出（组合）
  for(b <- 0 until cfg.L1_BANK){
    io.load_engine_rd(b).valid := pendV(b)
    io.load_engine_rd(b).addr  := (pendLine(b) << lineOffBits).resized
  }

  for (b <- 0 until cfg.L1_BANK) {

    // -------------------------
    // (1) 如果 pending 已经有了：就等 ready，fire 时入 outstanding
    // -------------------------
    when(pendV(b)) {
      when(io.load_engine_rd(b).ready) {
        val t = bank_os_tail(b)

        bank_os_slot(b)(t) := pendSlot(b)
        bank_os_lane(b)(t) := pendLane(b)
        bank_os_line(b)(t) := pendLine(b)

        bank_os_tail(b)  := bank_os_tail(b) + 1
        bank_os_count(b) := bank_os_count(b) + 1

        // 锁住这个 lane，防止重复 miss
        lane_wait(pendSlot(b))(pendLane(b)) := True

        // 清 pending
        pendV(b) := False
      }
    } otherwise {

      // -------------------------
      // (2) pending 空且 outstanding 未满：当拍挑一个 miss 候选，锁存成 pending
      // -------------------------
      when(!bankOsFull(b)) {

        val N = WIN_DEPTH * lane_num

        val cand      = Vec(Bool(), N)
        val candSlot  = Vec(UInt(winPtrW bits), N)
        val candLane  = Vec(UInt(log2Up(lane_num) bits), N)
        val candLine  = Vec(UInt(aw bits), N)

        for (i <- 0 until N) {
          cand(i)     := False
          candSlot(i) := 0
          candLane(i) := 0
          candLine(i) := 0
        }

        for (k <- 0 until WIN_DEPTH) {
          val s = slotAtAge(k)
          when(k < q_count && win_valid(s)) {
            for (l <- 0 until lane_num) {
              val idx  = k * lane_num + l
              val need = !win_pad(s)(l) && !lane_done(s)(l)
              val inBk = lane_bank(s)(l) === U(b, bankBits bits)
              val miss = !isCached2way(b, lane_line(s)(l))
              val missAllowed = !lane_wait(s)(l) && miss

              cand(idx)     := need && inBk && missAllowed
              candSlot(idx) := s
              candLane(idx) := U(l, log2Up(lane_num) bits)
              candLine(idx) := lane_line(s)(l)
            }
          }
        }

        val (found, fidx) = cand.sFindFirst(_ === True)

        when(found) {
          // 锁存成 pending：下一拍开始 valid 会被 pendV 驱动，直到 ready
          pendV(b)    := True
          pendSlot(b) := candSlot(fidx)
          pendLane(b) := candLane(fidx)
          pendLine(b) := candLine(fidx)
        }
      }
    }
  }

  // ------------------------------------------------------------
  // 9) Cache-hit completion scan (for lanes not waiting)
  // ------------------------------------------------------------
  val bs_00_dbg = lane_bank(0)(0)
  val la_00_dbg = lane_line(0)(0)
  val hit_00_dbg = cache_valid(bs_00_dbg)(0) && (cache_line(bs_00_dbg)(0) === la_00_dbg)
  val hit_01_dbg = cache_valid(bs_00_dbg)(1) && (cache_line(bs_00_dbg)(1) === la_00_dbg)
  val dbg_00 = win_valid(0) && (!win_pad(0)(0) && !lane_done(0)(0))
  for (s <- 0 until WIN_DEPTH) {
    when(win_valid(s)) {
      for (l <- 0 until lane_num) {

        when(!win_pad(s)(l) && !lane_done(s)(l)) {

          val bS  = lane_bank(s)(l)
          val lA  = lane_line(s)(l)
          val off = lane_off(s)(l)

          val h0 = cache_valid(bS)(0) && (cache_line(bS)(0) === lA)
          val h1 = cache_valid(bS)(1) && (cache_line(bS)(1) === lA)

          when(h0) {
            lane_data(s)(l) := extractElem(cache_data(bS)(0), off).resize(elemBits)
            lane_done(s)(l) := True
          } elsewhen(h1) {
            lane_data(s)(l) := extractElem(cache_data(bS)(1), off).resize(elemBits)
            lane_done(s)(l) := True
          }
        }

      }
    }
  }

  // ------------------------------------------------------------
  // 10) rvalid handling: pop one outstanding tag (in-order), fill cache, complete lane
  // ------------------------------------------------------------
  for (b <- 0 until cfg.L1_BANK) {
    when(io.load_engine_rd(b).rvalid && !bankOsEmpty(b)) {
      val h = bank_os_head(b)

      val s   = bank_os_slot(b)(h)
      val ln  = bank_os_lane(b)(h)
      val lA  = bank_os_line(b)(h)
      val off = lane_off(s)(ln)

      // (A) fill 2-way cache
      val w = repl_ptr(b)
      cache_valid(b)(w) := True
      cache_data(b)(w)  := io.load_engine_rd(b).rdata
      cache_line(b)(w)  := lA
      repl_ptr(b)       := ~repl_ptr(b)

      // (B) complete the lane
      lane_data(s)(ln) := extractElem(io.load_engine_rd(b).rdata, off).resize(elemBits)
      lane_done(s)(ln) := True
      lane_wait(s)(ln) := False

      // (C) pop outstanding tag
      bank_os_head(b)  := bank_os_head(b) + 1
      bank_os_count(b) := bank_os_count(b) - 1
    }
  }

  // ------------------------------------------------------------
  // 11) Retire in-order: only retire HEAD group when it is fully done
  // ------------------------------------------------------------
  val headSlot = q_head
  val headAllDone = Bool()
  headAllDone := False

  when(!qEmpty && win_valid(headSlot)) {
    headAllDone := lane_done(headSlot).map(_ === True).andR
  }

  when(!qEmpty && win_valid(headSlot) && headAllDone) {
    when(outFifo.io.push.ready) {
      outFifo.io.push.valid        := True
      outFifo.io.push.payload.pid  := win_pid(headSlot)
      outFifo.io.push.payload.oh  := win_oh(headSlot)
      outFifo.io.push.payload.ow  := win_ow(headSlot)
      outFifo.io.push.payload.oc  := win_oc(headSlot)
      outFifo.io.push.payload.data := Cat((0 until lane_num).map(l => lane_data(headSlot)(l)))

      win_valid(headSlot) := False
      q_head  := q_head + 1
      //q_count := q_count - 1
    }
  }
}