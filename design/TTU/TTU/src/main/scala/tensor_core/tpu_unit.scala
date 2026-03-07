package tensor_core
import spinal.core._
import spinal.lib._
import config.TTConfig
import scala.collection.mutable.ArrayBuffer
import config._

/**
 include TPU ARRAY, sync fifo, drain pool, write buffer
 **/

case class tpu_unit(cfg: TTConfig) extends Component {
  val io = new Bundle {
    val flush_in = in Bool()
    val gemm_k = in UInt(12 bits)
    val ifm_in = Vec.fill(cfg.TPU_H)(slave Stream(Bits(cfg.TPU_IDWMAX bits)))
    val ke_in = Vec.fill(cfg.TPU_W)(slave Stream(Bits(cfg.TPU_IDWMAX bits)))
    val ofm_out = Vec.fill(cfg.TPU_O)(master Stream(ofm_packet(12,cfg.TPU_ODWMAX)))
    val tile_done = out Bool()
    val otlie_config = slave Flow(otile_cfg(cfg))  // to tensor
    val tensor_cfg = in (tensor_dp(cfg))
  }

  val flush = io.flush_in || io.tile_done
  val otile_cfg_r = RegNextWhen(io.otlie_config.payload, io.otlie_config.valid)
  // if/kernel sync fifo
  //val ifm_fifo = Vec.fill(cfg.TPU_H)(StreamFifo(Bits(cfg.TPU_IDWMAX bits), cfg.TPU_H))
  //val ke_fifo = Vec.fill(cfg.TPU_W)(StreamFifo(Bits(cfg.TPU_IDWMAX bits), cfg.TPU_W))
  // 初始化空的ArrayBuffer，显式指定元素类型为StreamFifo[Bits]
  val ifm_fifo = ArrayBuffer[StreamFifo[Bits]]()
  // 遍历索引，生成深度递增的StreamFifo并加入ArrayBuffer
  for(i <- 0 until cfg.TPU_H) {
    ifm_fifo += StreamFifo(Bits(cfg.TPU_IDWMAX bits), i + 3)
  }
  val ke_fifo = ArrayBuffer[StreamFifo[Bits]]()
  for(i <- 0 until cfg.TPU_W) {
    ke_fifo += StreamFifo(Bits(cfg.TPU_IDWMAX bits), i + 3)
  }

  val ofm_fifo = ArrayBuffer[StreamFifo[ofm_packet]]()
  for(i <- 0 until cfg.TPU_O) {
    ofm_fifo += StreamFifo(ofm_packet(12,cfg.TPU_ODWMAX), cfg.TPU_O)
  }

  // if/kernel -> MAC
  val ifm_mac = Vec.fill(cfg.TPU_H)(Stream(Bits(cfg.TPU_IDWMAX bits)))
  val ke_mac = Vec.fill(cfg.TPU_W)(Stream(Bits(cfg.TPU_IDWMAX bits)))

  val gemm_k_pad = UInt(io.gemm_k.getWidth bits)
  def TPU_MAX = if(cfg.TPU_W > cfg.TPU_H) cfg.TPU_W else cfg.TPU_H
  when(io.gemm_k < TPU_MAX ){
    gemm_k_pad := TPU_MAX
  } .otherwise{
    gemm_k_pad := io.gemm_k
  }

  val flow_init = Flow(Bits(cfg.TPU_IDWMAX bits))
  flow_init.valid := False
  flow_init.payload := B(0)

  val stream_init = Stream(Bits(cfg.TPU_IDWMAX bits))
  stream_init.valid := True
  stream_init.payload := B(0)
  stream_init.ready := True

  val tpu_on = Reg(Bool()) init(False)

  val icnt = Reg(UInt((gemm_k_pad.getWidth) bits)) init(0)
  val tcnt = Reg(UInt((gemm_k_pad.getWidth+cfg.TPU_W+cfg.TPU_W) bits)) init(0)
  val ohcnt = Reg(UInt(log2Up(cfg.TPU_H) bits)) init(0)
  val owcnt = Reg(UInt(log2Up(cfg.TPU_W) bits)) init(0)
  val ocnt = Reg(UInt(log2Up(cfg.TPU_O) bits)) init(0)
  val pad_in = (icnt > (io.gemm_k -1)) && (tcnt < (gemm_k_pad -1) && (io.gemm_k < TPU_MAX)) && tpu_on
  val pipe_out = (tcnt >= (gemm_k_pad -1) && (tcnt < (gemm_k_pad + cfg.TPU_O + cfg.TPU_O))) && tpu_on
  val psum_out = (tcnt > (gemm_k_pad + cfg.TPU_O -1) && (tcnt <= (gemm_k_pad + cfg.TPU_O + cfg.TPU_O -1))) && tpu_on  // todo stall + mult pipe


  for (i <- 0 until cfg.TPU_H){
    ifm_fifo(i).io.flush := flush //io.flush_in
    io.ifm_in(i).ready := True
    when(pad_in){
      ifm_fifo(i).io.push << stream_init
    } .otherwise {
      ifm_fifo(i).io.push << io.ifm_in(i)
    }
    when(pipe_out){
      ifm_fifo(i).io.pop >> ifm_mac(i)
    } .elsewhen(ifm_fifo(i).io.occupancy >= i+2) {
      ifm_fifo(i).io.pop >> ifm_mac(i)
    } .otherwise{
      ifm_fifo(i).io.pop.ready := False
      ifm_mac(i).valid := False
      ifm_mac(i).payload := B(0)
    }
    ifm_mac(i).ready := True // todo stall
  }

  for (i <- 0 until cfg.TPU_W){
    io.ke_in(i).ready := True
    ke_fifo(i).io.flush := flush  //io.flush_in
    when(pad_in){
      ke_fifo(i).io.push << stream_init
    } .otherwise {
      ke_fifo(i).io.push << io.ke_in(i)
    }
    when(pipe_out) {
      ke_fifo(i).io.pop >> ke_mac(i)
    } .elsewhen(ke_fifo(i).io.occupancy >= i+2) {
      ke_fifo(i).io.pop >> ke_mac(i)
    } .otherwise{
      ke_fifo(i).io.pop.ready := False
      ke_mac(i).valid := False
      ke_mac(i).payload := B(0)
    }
    ke_mac(i).ready := True // todo stall
  }

  // Shift  // todo delete shift_reg_x.valid
  val shift_reg_h = Vec.fill(cfg.TPU_H)(Vec.fill(cfg.TPU_W)(Reg(Flow(Bits(cfg.TPU_IDWMAX bits))) init(flow_init) ))
  val shift_reg_w = Vec.fill(cfg.TPU_W)(Vec.fill(cfg.TPU_H)(Reg(Flow(Bits(cfg.TPU_IDWMAX bits))) init(flow_init)))

  val flow_pipe_out = Flow(Bits(cfg.TPU_IDWMAX bits))
  flow_pipe_out.valid := True
  flow_pipe_out.payload := B(0)

  val shift_vld = ifm_mac.map(_.valid).asBits.orR || ke_mac.map(_.valid).asBits.orR
  val shift_reg_vld = RegNext(shift_vld) init(False)

  for(i <- 0 until cfg.TPU_H) {
    when(tcnt >= gemm_k_pad + i) {
      shift_reg_h(i)(0) := flow_pipe_out
    } .otherwise{
      //shift_reg_h(i)(0) := ifm_mac(i).asFlow
      when(ifm_mac(i).valid){
        shift_reg_h(i)(0) := ifm_mac(i).asFlow
      }
    }
  }
  for(i <- 0 until cfg.TPU_W) {
    when(tcnt >= gemm_k_pad + i) {
      shift_reg_w(0)(i) := flow_pipe_out
    } .otherwise{
      //shift_reg_w(0)(i) := ke_mac(i).asFlow
      when(ke_mac(i).valid){
        shift_reg_w(0)(i) := ke_mac(i).asFlow
      }
    }
  }

  for(i <- 0 until cfg.TPU_H) {
    for(j <- 1 until cfg.TPU_W) {
      //shift_reg_h(i)(j) := shift_reg_h(i)(j - 1)

      when(tcnt >= gemm_k_pad + i) {
        shift_reg_h(i)(j) := shift_reg_h(i)(j - 1)
      }.otherwise {
        //shift_reg_h(i)(0) := ifm_mac(i).asFlow
        when(ifm_mac(i).valid) {
          shift_reg_h(i)(j) := shift_reg_h(i)(j - 1)
        }
      }
    }
  }

  for(i <- 1 until cfg.TPU_H) {
    for(j <- 0 until cfg.TPU_W) {
      //shift_reg_w(i)(j) := shift_reg_w(i - 1)(j)
      when(tcnt >= gemm_k_pad + i) {
        shift_reg_w(i)(j) := shift_reg_w(i - 1)(j)
      } .otherwise{
        when(ke_mac(i).valid){
          shift_reg_w(i)(j) := shift_reg_w(i - 1)(j)
        }
      }
    }
  }

  // MAC array
  val psum = Vec.fill(cfg.TPU_H)(Vec.fill(cfg.TPU_W)(Reg(SInt(cfg.TPU_PSDWMAX bits)) init(0)))
  val psum_done = Vec.fill(cfg.TPU_H)(Vec.fill(cfg.TPU_W)(Reg(Bool()) init(False))) // todo
  for(i <- 0 until cfg.TPU_H) {
    for(j <- 0 until cfg.TPU_W) {
      //when(psum_done(i)(j)){
      when(flush) {
        psum(i)(j) := S(0)
      } .elsewhen(/*shift_reg_h(i)(j).valid && shift_reg_w(i)(j).valid*/shift_reg_vld || tcnt >= gemm_k_pad + i){
        if(cfg.TPU_IDTYPE == "UINT8" && cfg.TPU_KDTYPE == "INT8"){
          psum(i)(j) := psum(i)(j) + (shift_reg_h(i)(j).payload.resize(8).asUInt.resize(9)).asSInt/*consider*/ * shift_reg_w(i)(j).payload.resize(8).asSInt
        }
        // todo
      }
    }
  }

  // todo output
  when(flush){
    tcnt := 0
  } .elsewhen(ifm_mac.map(_.valid).asBits.orR || ke_mac.map(_.valid).asBits.orR) {
    tcnt := tcnt + 1
  } .elsewhen(pipe_out) {
    tcnt := tcnt + 1
  }

  when(flush){
    ohcnt := 0
    owcnt := 0
    ocnt := 0
  } .elsewhen(psum_out){
    ocnt := ocnt + 1
    when(otile_cfg_r.ORDER === 0){
      when(owcnt === otile_cfg_r.OW_TILE.asUInt-1){
        owcnt := 0
        when(ohcnt === otile_cfg_r.OH_TILE.asUInt-1) {
          ohcnt := 0
        } .otherwise{
          ohcnt := ohcnt + 1
        }
      } .otherwise{
        owcnt := owcnt + 1
      }
    }
  }

  when(flush){
    icnt := 0
  } .elsewhen(io.ifm_in.map(_.valid).asBits.orR || io.ke_in.map(_.valid).asBits.orR) {
    icnt := icnt + 1
  }

  when(io.flush_in){
    tpu_on := False
  } .elsewhen(io.ifm_in.map(_.valid).asBits.orR || io.ke_in.map(_.valid).asBits.orR){
    tpu_on := True
  }

  val ofm_mac = Vec.fill(cfg.TPU_O)(Stream(ofm_packet(12,cfg.TPU_ODWMAX)))
  val ofm_packet_init = ofm_packet(12,cfg.TPU_ODWMAX)
  ofm_packet_init.assignFromBits(B(0, ofm_packet_init.getBitsWidth bits))

  for (i <- 0 until cfg.TPU_O) {
    ofm_mac(i).valid.setAsReg() init (False)
    ofm_mac(i).payload.setAsReg() //init(ofm_packet_init)
  }

  when(psum_out) {
    for (i <- 0 until cfg.TPU_O) {
      ofm_mac(i).valid := True
      ofm_mac(i).payload.data := psum(ocnt)(i).resize(cfg.TPU_ODWMAX).asBits  // oc-first //psum(i)(ocnt).resize(cfg.TPU_ODWMAX).asBits // sp_len first
      ofm_mac(i).payload.oh := (io.otlie_config.TILE_OH0.asUInt + ohcnt).resize(12 bits)
      ofm_mac(i).payload.ow := (io.otlie_config.TILE_OW0.asUInt + owcnt).resize(12 bits)
      ofm_mac(i).payload.oc := i + io.otlie_config.TILE_OC0.asUInt
    }
  } .otherwise{
    for (i <- 0 until cfg.TPU_O) {
      ofm_mac(i).valid.setAsReg() init (False)
      ofm_mac(i).valid := False
    }
  }

  for (i <- 0 until cfg.TPU_O){
    ofm_fifo(i).io.flush := io.flush_in
    ofm_fifo(i).io.push << ofm_mac(i)
    ofm_fifo(i).io.pop >> io.ofm_out(i)

  }

  io.tile_done.setAsReg() init(False)
  when( (tcnt >= (gemm_k_pad + cfg.TPU_O + cfg.TPU_O -1)) && ofm_fifo.map(_.io.occupancy === 0).andR) {
    io.tile_done := True
  }.otherwise{
    io.tile_done := False
  }


}
