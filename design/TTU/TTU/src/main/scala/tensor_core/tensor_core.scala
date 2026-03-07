package tensor_core
import spinal.core._
import spinal.lib._
import config.TTConfig
import config._
import spinal.core.sim.SimBitVectorPimper

case class tensor_core(cfg: TTConfig,pw: Int) extends Component {
  val io = new Bundle {
    val tensor_dispatch = slave Stream (tensor_dp(cfg)) // from scb, global cfg
    val packet_ifm = slave(Stream(tpu_packet(pw, cfg.TPU_IDWMAX*cfg.TPU_H)))  // from LOAD_ENGINE @IFM
    val packet_ker = slave(Stream(tpu_packet(pw,cfg.TPU_IDWMAX*cfg.TPU_W)))  // from LOAD_ENGINE @KER
    val packet_ofm = master(Stream(tpu_packet(pw, cfg.TPU_ODWMAX*cfg.TPU_O)))  // to STORE_ENGINE @OFM
    val tensor_back = master Stream (alu_back(cfg)) // to scb
    val OTILE_CFG = slave Flow(otile_cfg(cfg))  // from loop controller
  }
  val ifm_in = Vec.fill(cfg.TPU_H)(Stream(Bits(cfg.TPU_IDWMAX bits)))
  val ke_in = Vec.fill(cfg.TPU_W)(Stream(Bits(cfg.TPU_IDWMAX bits)))
  val ofm_out = Vec.fill(cfg.TPU_O)(Stream(ofm_packet(12,cfg.TPU_ODWMAX)))
  val tensor_cfg_init = tensor_dp(cfg)
  tensor_cfg_init.assignFromBits(B(0, tensor_cfg_init.getBitsWidth bits))
  val tensor_cfg = Reg(tensor_dp(cfg)) init(tensor_cfg_init)
  val tensor_unit = tpu_unit(cfg)
  val tensor_work = Reg(Bool()) init(False)
  val tile_done = Bool()
  val tile_real_done = Bool()
  val tile_done_latch = Reg(Bool()) init(False)
  val tile_token_id = Reg(UInt(pw bits)) init(0)

  io.tensor_dispatch.ready := !tensor_work
  when(io.tensor_dispatch.valid){
    tensor_cfg := io.tensor_dispatch.payload
    tensor_work := True
    // todo
    //assert(io.tensor_dispatch.payload.M_TILE.asUInt == cfg.TPU_H, "CONV M_TILE mismatch!!")
    //assert(io.tensor_dispatch.payload.N_TILE.asUInt == cfg.TPU_W, "CONV N_TILE mismatch!!")
  } .elsewhen(tile_done){
    tensor_work := False
  }

  tensor_unit.io.flush_in := io.tensor_dispatch.valid
  tensor_unit.io.gemm_k := tensor_cfg.K_TILE.asUInt
  tensor_unit.io.otlie_config := io.OTILE_CFG
  tensor_unit.io.ifm_in <> ifm_in
  tensor_unit.io.ke_in <> ke_in
  tensor_unit.io.ofm_out <> ofm_out
  tile_done := tensor_unit.io.tile_done

  val canFire = io.packet_ifm.valid && io.packet_ker.valid && (io.packet_ifm.payload.pid === io.packet_ker.payload.pid) && ifm_in.map(_.ready).andR && ke_in.map(_.ready).andR


  when(canFire){
    tile_token_id := io.packet_ifm.payload.pid
  }

  val ifmHoldV = Reg(Bool()) init(False)
  val kerHoldV = Reg(Bool()) init(False)
  val ifmHold  = Reg(tpu_packet(pw, cfg.TPU_IDWMAX*cfg.TPU_H))
  val kerHold  = Reg(tpu_packet(pw, cfg.TPU_IDWMAX*cfg.TPU_W))
  val pidMatch = ifmHoldV && kerHoldV && (ifmHold.pid === kerHold.pid)

  val downReady = ifm_in.map(_.ready).andR && ke_in.map(_.ready).andR
  val doSend = pidMatch && downReady

  io.packet_ifm.ready := !ifmHoldV || doSend
  io.packet_ker.ready := !kerHoldV || doSend

  when(io.packet_ifm.fire){
    ifmHoldV := True
    ifmHold  := io.packet_ifm.payload
  } .elsewhen(doSend){
    ifmHoldV := False
  }

  when(io.packet_ker.fire){
    kerHoldV := True
    kerHold  := io.packet_ker.payload
  } .elsewhen(doSend){
    kerHoldV := False
  }

//  when(doSend){
//    ifmHoldV := False
//    kerHoldV := False
//  }

  val ifmVecBits = ifmHold.data.subdivideIn(cfg.TPU_IDWMAX bits)

  // 每个 lane
  for(i <- 0 until cfg.TPU_H){
    ifm_in(i).valid   := doSend  //io.packet_ifm.valid
    ifm_in(i).payload := ifmVecBits(i)
  }

  // 只有所有 lane ready 才 ready
  //io.packet_ifm.ready := ifm_in.map(_.ready).andR

  val kerVecBits = kerHold.data.subdivideIn(cfg.TPU_IDWMAX bits)

  for(i <- 0 until cfg.TPU_W){
    ke_in(i).valid   := doSend //io.packet_ker.valid
    ke_in(i).payload := kerVecBits(i)
  }

 // io.packet_ker.ready := ke_in.map(_.ready).andR

  io.packet_ofm.valid := ofm_out.map(_.valid).andR
  io.packet_ofm.payload.data := Cat(ofm_out.reverse.map(_.payload.data))
  io.packet_ofm.payload.pid := tile_token_id
  io.packet_ofm.payload.oh := ofm_out(0).payload.oh  // todo OS下可以直接用(0)
  io.packet_ofm.payload.ow := ofm_out(0).payload.ow
  io.packet_ofm.payload.oc := ofm_out(0).payload.oc

  for(i <- 0 until cfg.TPU_O){
    ofm_out(i).ready := io.packet_ofm.ready
  }


  io.tensor_back.valid.setAsReg() init(False)
  io.tensor_back.payload.CMD_ID.setAsReg() init(0)
  when(io.tensor_back.ready){
    when(tile_done || tile_done_latch){ // todo : other ALUs need add this ack
      io.tensor_back.valid := True
      io.tensor_back.payload.CMD_ID := tensor_cfg.CMD_ID
    } .elsewhen(io.tensor_back.valid){
      io.tensor_back.valid := False
    }
  } .otherwise{
    io.tensor_back.valid := False
  }

  when(tile_done && !io.tensor_back.ready){
    tile_done_latch := True
  } .elsewhen(io.tensor_back.ready && tile_done_latch){
    tile_done_latch := False
  }

}

