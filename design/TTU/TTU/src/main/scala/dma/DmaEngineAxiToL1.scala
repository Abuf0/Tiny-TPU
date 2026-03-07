package dma
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.amba4.axi._
import config.TTConfig
import config._

class DmaEngineAxiToL1(cfg: TTConfig) extends Component {
  val io = new Bundle {
    val axi = master(Axi4(cfg.memAxi))
    val wr  = master(sram_wport(cfg.L1_AW, cfg.L1_DW))
    val rd  = master(sram_rport(cfg.L1_AW, cfg.L1_DW))

    val cmd = slave(Stream(DmaCmd(cfg.memAxi.addressWidth)))
    val busy= out Bool()
    val done= out Bool()
  }

  // todo : ar_len... need to be config by cmd in

  val beatBytes = cfg.memAxi.dataWidth / 8
  require(cfg.L1_DW == cfg.memAxi.dataWidth, "简化版本要求 L1_DW == AXI dataWidth（否则需加 width converter）")

  // defaults
  io.wr.valid := False
  io.wr.addr  := 0
  io.wr.data  := 0
  io.wr.strb  := B((cfg.L1_DW/8) bits, default -> True)

  io.rd.valid := False
  io.rd.addr  := 0

  io.cmd.ready := False
  io.done := False

  // AXI default idle
  io.axi.ar.valid := False
  io.axi.ar.payload.assignDontCare()
  io.axi.aw.valid := False
  io.axi.aw.payload.assignDontCare()
  io.axi.w.valid  := False
  io.axi.w.payload.assignDontCare()
  io.axi.r.ready  := True
  io.axi.b.ready  := True

  // state regs
  val busy   = RegInit(False)
  val isLoad = Reg(Bool()) init(True)
  val extPtr = Reg(UInt(cfg.memAxi.addressWidth bits)) init(0)
  val l1Ptr  = Reg(UInt(cfg.memAxi.addressWidth bits)) init(0)
  val remain = Reg(UInt(32 bits)) init(0)
  val trans_len = Reg(UInt(8 bits)) init(0)
  val wrFire = io.wr.valid && io.wr.ready
  val rdFire = io.rd.valid && io.rd.ready

  io.busy := busy

  // FIFOs
  val rFifo = StreamFifo(Bits(cfg.L1_DW bits), 16) // AXI R -> L1 WR
  rFifo.io.flush := False
  val wFifo = StreamFifo(Bits(cfg.L1_DW bits), 16) // L1 RD -> AXI W
  wFifo.io.flush := False

  wFifo.io.push.valid := False
  wFifo.io.push.payload := 0
  rFifo.io.push.valid := False
  rFifo.io.push.payload := 0

  val done_d1 = RegNext(io.done) init(False)

  // accept cmd
  when(!busy) {
    io.cmd.ready := True
    when(io.cmd.fire && !done_d1) {
      busy   := True
      isLoad := io.cmd.payload.isLoad
      extPtr := io.cmd.payload.extAddr
      l1Ptr  := io.cmd.payload.l1Addr
      remain := io.cmd.payload.bytes
      trans_len := io.cmd.payload.trans_len
    }
  }

  // -----------------------
  // LOAD: ext -> L1
  // -----------------------
  when(busy && isLoad) {
    // issue one-beat AXI read when need data and fifo has space
    when(remain =/= 0 && rFifo.io.availability > 4) {
      io.axi.ar.valid := True
      io.axi.ar.addr  := extPtr
      io.axi.ar.len   := trans_len-1
      io.axi.ar.size  := log2Up(beatBytes)
      io.axi.ar.burst := B"01"
      io.axi.ar.id    := 0

      when(io.axi.ar.fire) {
        extPtr := extPtr + U(beatBytes).resized
      }
    }

    // capture AXI R into fifo
    rFifo.io.push.valid   := io.axi.r.valid
    rFifo.io.push.payload := io.axi.r.data
    io.axi.r.ready        := rFifo.io.push.ready

    // pop fifo -> L1 write
    io.wr.valid := rFifo.io.pop.valid
    io.wr.addr  := l1Ptr.resized
    io.wr.data  := rFifo.io.pop.payload
    io.wr.strb  := B((cfg.L1_DW/8) bits, default -> True)
    rFifo.io.pop.ready := io.wr.ready

    when(wrFire) {
      l1Ptr  := l1Ptr + U(beatBytes).resized
      when(remain >= U(beatBytes, 32 bits)) {
        remain := remain - U(beatBytes, 32 bits)
      } .otherwise{
        remain := 0
        io.wr.strb := (B((cfg.L1_DW/8) bits, default -> True) >> ((cfg.L1_DW/8) - remain)).resize(cfg.L1_DW/8)
      }
    }

    when(remain === 0 && !rFifo.io.pop.valid) {
      busy := False
      io.done := True
    }
  } otherwise {
    rFifo.io.push.valid := False
    rFifo.io.pop.ready  := False
  }

  // -----------------------
  // STORE: L1 -> ext
  // -----------------------
  when(busy && !isLoad) {
    // 1) issue L1 read requests into rd port when fifo has space
    // 注意：你的 rd 是 request/response 分离；这里用“发请求→等 rvalid”模型
    when(remain =/= 0 && wFifo.io.availability > 4) {
      io.rd.valid := True
      io.rd.addr  := l1Ptr.resized
      when(rdFire) { // fire = valid && ready
        l1Ptr := l1Ptr + U(beatBytes).resized
      }
    }

    // 2) push L1 read response into fifo
    wFifo.io.push.valid   := io.rd.rvalid
    wFifo.io.push.payload := io.rd.rdata
    // 如果你希望对返回反压，需要在 rport 里增加 respReady；目前没有，只能吞

    // 3) issue AXI write when fifo has data
    io.axi.aw.valid := wFifo.io.pop.valid
    io.axi.aw.addr  := extPtr
    io.axi.aw.len   := trans_len-1
    io.axi.aw.size  := log2Up(beatBytes)
    io.axi.aw.burst := B"01"
    io.axi.aw.id    := 0

    // W：简单耦合到 AW（实际可解耦）
    io.axi.w.valid := wFifo.io.pop.valid && io.axi.aw.ready
    io.axi.w.data  := wFifo.io.pop.payload
    io.axi.w.strb  := B((cfg.memAxi.dataWidth/8) bits, default -> True)
    io.axi.w.last  := True

    wFifo.io.pop.ready := io.axi.w.fire

    when(io.axi.w.fire) {
      extPtr  := extPtr + U(beatBytes).resized
      when(remain > U(beatBytes, 32 bits)) {
        remain := remain - U(beatBytes, 32 bits)
      } .otherwise{
        remain := 0
        io.axi.w.strb := (B((cfg.L1_DW/8) bits, default -> True) >> ((cfg.L1_DW/8) - remain)).resize(cfg.L1_DW/8)
      }
    }

    when(remain === 0 && !wFifo.io.pop.valid) {
      busy := False
      io.done := True
    }
  } otherwise {
    wFifo.io.push.valid := False
    wFifo.io.pop.ready  := False
  }
}
