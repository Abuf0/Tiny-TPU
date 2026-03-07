package alu
import spinal.core._
import spinal.lib._
import config.TTConfig
import config._

case class loop_controller(cfg: TTConfig, lane_num: Int) extends Component {
  val io = new Bundle {
    val loop_ctrl_dispatch = slave Stream (loop_ctrl_dp(cfg)) // from scb, decode info, CONV_TILE
    val loop_ctrl = master Stream(loop_ctrl_scan(cfg,lane_num)) // to trig AGU, this ready must be AGU_IFM & AGU_KER
    val OTILE_cfg = master Flow(otile_cfg(cfg))  // to tensor
  }

  val loop_work = Reg(Bool()) init(False)
  val loop_ctrl_dp_init = loop_ctrl_dp(cfg)
  loop_ctrl_dp_init.assignFromBits(B(0,loop_ctrl_dp_init.asBits.getBitsWidth bits))
  val loop_cfg = Reg(loop_ctrl_dp(cfg)) init(loop_ctrl_dp_init)

  val loop_fifo = StreamFifo(loop_ctrl_scan(cfg,lane_num),cfg.LOOP_DEPTH)

  val oh_idx = Reg(UInt(loop_ctrl_scan(cfg,lane_num).oh(0).getBitsWidth bits)) init(0)
  val ow_idx = Reg(UInt(loop_ctrl_scan(cfg,lane_num).ow(0).getBitsWidth bits)) init(0)
  val token_id = Reg(UInt(loop_ctrl_scan(cfg,lane_num).token_id(0).getBitsWidth bits)) init(0)
  val lane_idx = Reg(UInt(log2Up(lane_num) bits)) init(0)
  val loop_valid = Reg(Bool()) init(False)
  val row_done = (ow_idx === loop_cfg.OW_TILE.asUInt-1)
  val col_done = (oh_idx === loop_cfg.OH_TILE.asUInt-1)
  val tile_done = row_done && col_done
  val loop_done = loop_work && loop_fifo.io.push.ready && tile_done
  val lane_done = loop_work && loop_fifo.io.push.ready && (lane_idx === lane_num-1)

  io.loop_ctrl_dispatch.ready := !loop_work
  io.OTILE_cfg.valid.setAsReg() init(False)
  io.OTILE_cfg.payload.ORDER.setAsReg() init(0)
  io.OTILE_cfg.payload.OH_TILE.setAsReg() init(0)
  io.OTILE_cfg.payload.OW_TILE.setAsReg() init(0)
  io.OTILE_cfg.payload.OC_TILE.setAsReg() init(0)
  io.OTILE_cfg.payload.TILE_OH0.setAsReg() init(0)
  io.OTILE_cfg.payload.TILE_OW0.setAsReg() init(0)
  io.OTILE_cfg.payload.TILE_OC0.setAsReg() init(0)

  io.OTILE_cfg.valid := io.loop_ctrl_dispatch.valid
  when(io.loop_ctrl_dispatch.valid){
    loop_work := True
    loop_cfg := io.loop_ctrl_dispatch.payload
    io.OTILE_cfg.OH_TILE := io.loop_ctrl_dispatch.payload.OH_TILE
    io.OTILE_cfg.OW_TILE := io.loop_ctrl_dispatch.payload.OW_TILE
    io.OTILE_cfg.OC_TILE := io.loop_ctrl_dispatch.payload.OC_TILE
    io.OTILE_cfg.ORDER := io.loop_ctrl_dispatch.payload.ORDER
    io.OTILE_cfg.TILE_OH0 := io.loop_ctrl_dispatch.payload.TILE_OH0
    io.OTILE_cfg.TILE_OW0 := io.loop_ctrl_dispatch.payload.TILE_OW0
    io.OTILE_cfg.TILE_OC0 := io.loop_ctrl_dispatch.payload.TILE_OC0
  } .elsewhen(loop_done){
    loop_work := False
    oh_idx := 0
    ow_idx := 0
    token_id := 0
  }
  when(loop_done){
    lane_idx := 0
  } .elsewhen(loop_valid){
    when(lane_done){
      lane_idx := 0
    } .otherwise{
      lane_idx := lane_idx + 1
    }
  }

  when(loop_work && loop_fifo.io.push.ready && !loop_done){
    loop_valid := True
    when(loop_valid){
      token_id := token_id + 1
    }
    when(loop_cfg.ORDER === 0 && loop_valid){ // row first
      when(row_done){
        oh_idx := oh_idx + 1
        ow_idx := 0
      } .otherwise{
        ow_idx := ow_idx + 1
      }
    } .elsewhen(loop_cfg.ORDER === 1 && loop_valid){
      when(col_done){
        ow_idx := ow_idx + 1
        oh_idx := 0
      } .otherwise{
        oh_idx := oh_idx + 1
      }
    }
  } .otherwise{
    loop_valid := False
  }

  val loop_flow_init = Flow(loop_ctrl_scan(cfg, lane_num))
  loop_flow_init.valid := False
  loop_flow_init.payload.assignFromBits(B(0,loop_flow_init.getBitsWidth bits))
  val loop_buffer = Reg(Flow(loop_ctrl_scan(cfg, lane_num))) init(loop_flow_init)
  when(lane_done || loop_done){
    loop_buffer.valid := True
  } .otherwise{
    loop_buffer.valid := False
  }
  when(loop_valid){
    loop_buffer.payload.oh(lane_idx) := (oh_idx + loop_cfg.TILE_OH0.asUInt).asBits //oh_idx.asBits
    loop_buffer.payload.ow(lane_idx) := (ow_idx + loop_cfg.TILE_OW0.asUInt).asBits  //ow_idx.asBits
    loop_buffer.payload.oc := loop_cfg.TILE_OC0 // tile_oc start point /per CONV_TILE -> AGU_KER
    loop_buffer.payload.token_id(lane_idx) := token_id.asBits
    loop_buffer.payload.mask(lane_idx) := True  // todo
  }

  when(loop_done){
    for (l <- 0 until lane_num) {
      when(l > lane_idx){
        loop_buffer.payload.mask(l) := False
      }
    }
  }


  loop_fifo.io.push.valid := loop_buffer.valid
  loop_fifo.io.push.payload := loop_buffer.payload
//  loop_fifo.io.push.payload.oh := oh_idx.asBits
//  loop_fifo.io.push.payload.ow := ow_idx.asBits
//  loop_fifo.io.push.payload.token_id := token_id.asBits
  loop_fifo.io.pop >> io.loop_ctrl


}

