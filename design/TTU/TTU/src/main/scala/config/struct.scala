package config
import spinal.core._
import spinal.lib._

/***
| 偏移 | 字段         | 类型       | 含义                           |
| -: | ---------- | -------- | ---------------------------- |
|  0 | op         | uint8    | opcode（见 Op 枚举）              |
|  1 | dep_count  | uint8    | deps 数量（最多 4）                |
|  2 | succ_count | uint8    | 下游依赖个数                       |
|  3 | rsv        | uint8    | 保留（0）                        |
|  4 | deps[0]    | uint16   | 依赖指令 index（0..n-1），无则 0xFFFF |
|  6 | deps[1]    | uint16   | 同上                           |
|  8 | deps[2]    | uint16   | 同上                           |
| 10 | deps[3]    | uint16   | 同上                           |
| 12 | w0         | uint32   | 指令 payload word0             |
| 16 | w1         | uint32   | payload word1                |
| 20 | w2         | uint32   | payload word2                |
| 24 | w3         | uint32   | payload word3                |
| 28 | pad        | uint8[4] | 填充/保留                        |
***/
case class mem_if(cfg: TTConfig, AW : Int, DW : Int) extends Bundle with IMasterSlave {
  val mem_rd = Bool()
  val mem_wr = Bool()
  val mem_addr = UInt(AW bits)
  val mem_wdata = UInt(DW bits)
  val mem_wmask = UInt(DW/8 bits)
  val mem_rdata = UInt(DW bits)

  override def asMaster(): Unit = {
    out(mem_rd,mem_wr,mem_addr,mem_wdata,mem_wmask)
    in(mem_rdata)
  }
}

case class sram_wport(aw: Int, dw: Int) extends Bundle with IMasterSlave {
  val valid = Bool()
  val addr  = UInt(aw bits)
  val data  = Bits(dw bits)
  val strb  = Bits(dw/8 bits)
  val ready = Bool()
  override def asMaster(): Unit = {
    out(valid,addr,data,strb)
    in(ready)
  }
}

case class sram_rport(aw: Int, dw: Int) extends Bundle with IMasterSlave {
  val valid = Bool()
  val addr  = UInt(aw bits)
  val ready = Bool()
  val rvalid= Bool()
  val rdata = Bits(dw bits)
  override def asMaster(): Unit = {
    out(valid,addr)
    in(ready,rdata,rvalid)
  }
}

case class load_req(aw: Int, lw: Int, pw: Int) extends Bundle {
  val req_valid = Bool()
  val req_addr  = UInt(aw bits)
  val req_laneid = UInt(lw bits)
  val req_packetid = UInt(pw bits)
  val req_ispad= Bool()
  // todo : oc dont need IFM/KER (oh,ow,oc)
  val req_oh = UInt(pw bits)
  val req_ow = UInt(pw bits)
  val req_oc = UInt(pw bits)
}

case class tpu_packet(pw: Int, dataW: Int) extends Bundle {
  val pid  = UInt(pw bits)
  val data = Bits(dataW bits)
  // todo : oc dont need IFM/KER (oh,ow,oc)
  val oh = UInt(pw bits)
  val ow = UInt(pw bits)
  val oc = UInt(pw bits)
}

case class ofm_packet(pw: Int, dataW: Int) extends Bundle {
  val data = Bits(dataW bits)
  val oh = UInt(pw bits)
  val ow = UInt(pw bits)
  val oc = UInt(pw bits)
}

case class DmaCmd(addrW: Int) extends Bundle {
  val isLoad = Bool()                 // 1: ext->L1, 0: L1->ext
  val extAddr= UInt(addrW bits)       // external memory byte address
  val l1Addr = UInt(addrW bits)       // L1 global byte address (送 L1Fabric decode)
  val bytes  = UInt(32 bits)
  val trans_len = UInt(8 bits)
}


case class cmd_cfg(cfg: TTConfig) extends Bundle {
  val cmd_base = Bits(cfg.CMD_AW+cfg.CMD_WB/8 bits)
  val cmd_size = Bits(cfg.CMD_AW bits)
  val cmdcpy_en = Bool()
  val cmdcpy_src = UInt(cfg.memAxi.addressWidth bits)
  val cmdcpy_dst = UInt(cfg.memAxi.addressWidth bits)
  val cmdcpy_bytes = UInt(32 bits)
  val cmdcpy_trans_len = UInt(8 bits)
}

case class dec_map(cfg: TTConfig) extends Bundle {
  val CMD_ID = Bits(cfg.CMD_AW bits)
  val OP_DEC = Bits(8 bits)
  val DEP_CNT = UInt(8 bits)
  val SUCC_CNT = UInt(8 bits)
  val DEP_ID_0 = Bits(16 bits)
  val DEP_ID_1 = Bits(16 bits)
  val DEP_ID_2 = Bits(16 bits)
  val DEP_ID_3 = Bits(16 bits)
  val WORD_0 = Bits(32 bits)
  val WORD_1 = Bits(32 bits)
  val WORD_2 = Bits(32 bits)
  val WORD_3 = Bits(32 bits)
  val WORD_4 = Bits(32 bits)
  val RESERVE_BYTE = Bits(8 bits)
}

// todo : rearrange dispatch bundles
case class dma_dp(cfg: TTConfig) extends Bundle{
  val CMD_ID = Bits(cfg.CMD_AW bits)
  val dma_cfg = DmaCmd(cfg.memAxi.addressWidth)
  val is_last_trans = Bool()
}

case class agu_dma_dp(cfg: TTConfig) extends Bundle{
  val CMD_ID = Bits(cfg.CMD_AW bits)
  val OP_DEC = Bits(8 bits)
  val WORD_0 = Bits(32 bits)
  val WORD_1 = Bits(32 bits)
  val WORD_2 = Bits(32 bits)
  val WORD_3 = Bits(32 bits)
  val PADDING = Bits(8 bits)
}

/***
 * | 参数名             | 含义                 | 典型范围          | 位宽建议       | 必须 | 编译器配置 |
 * | --------------- | ------------------ | ------------- | ---------- | -- | ----- |
 * | **BASE_IFM**    | L1 中 IFM tile 的基地址 | 0 ~ L1_size-1 | `L1_AW`    | ✅  | ✅     |
 * | **ELEM_BYTES**  | 每个 element 字节数     | 1 / 2 / 4 / 8 | 2~3 bits   | ✅  | ✅     |
 * | **LAYOUT_TYPE** | 数据布局（NHWC/NCHW）    | 0~3           | 2 bits     | ✅  | ✅     |
 * | **H_IN**        | 输入高度               | 1 ~ 4096      | 12~13 bits | ✅  | ✅     |
 * | **W_IN**        | 输入宽度               | 1 ~ 4096      | 12~13 bits | ✅  | ✅     |
 * | **C_IN**        | 输入通道数              | 1 ~ 4096      | 11~12 bits | ✅  | ✅     |
 * | **K_H**         | Kernel 高           | 1 ~ 15 (或31)  | 4~5 bits   | ✅  | ✅     |
 * | **K_W**         | Kernel 宽           | 1 ~ 15 (或31)  | 4~5 bits   | ✅  | ✅     |
 * | **STRIDE_H**    | 垂直步长               | 1 ~ 8         | 3 bits     | ✅  | ✅     |
 * | **STRIDE_W**    | 水平步长               | 1 ~ 8         | 3 bits     | ✅  | ✅     |
 * | **DILATION_H**  | 垂直空洞率              | 1 ~ 16        | 4 bits     | ✅  | ✅     |
 * | **DILATION_W**  | 水平空洞率              | 1 ~ 16        | 4 bits     | ✅  | ✅     |
 * | **PAD_TOP**     | 上侧 padding         | 0 ~ 64        | 6 bits     | ✅  | ✅     |
 * | **PAD_LEFT**    | 左侧 padding         | 0 ~ 64        | 6 bits     | ✅  | ✅     |
 * | **TILE_H0**     | Tile 在原图起始行        | 0 ~ H_IN-1    | 与 H_IN 相同  | 推荐 | ✅     |
 * | **TILE_W0**     | Tile 在原图起始列        | 0 ~ W_IN-1    | 与 W_IN 相同  | 推荐 | ✅     |
 * | **TILE_C0**     | Tile 在通道维起始位置      | 0 ~ C_IN-1    | 与 C_IN 相同  | 推荐 | ✅     |
 */

case class agu_compute_dp(cfg: TTConfig) extends Bundle{
  val CMD_ID = Bits(cfg.CMD_AW bits)
  val BASE = Bits(cfg.L1_AW bits)
  val ELEM_BYTES = Bits(3 bits)
  val LAYOUT_TYPE = Bits(2 bits)
  val H_IN = Bits(12 bits)
  val W_IN = Bits(12 bits)
  val C_IN = Bits(12 bits)
  val C_OUT = Bits(12 bits)
  val K_H = Bits(4 bits)
  val K_W = Bits(4 bits)
  val STRIDE_H = Bits(3 bits)
  val STRIDE_W = Bits(3 bits)
  val DILATION_H = Bits(4 bits)
  val DILATION_W = Bits(4 bits)
  val PAD_TOP = Bits(6 bits)
  val PAD_LEFT = Bits(6 bits)
  val TILE_H0 = Bits(12 bits)
  val TILE_W0 = Bits(12 bits)
  val TILE_C0 = Bits(12 bits)
  val OC_TILE = Bits(12 bits)
}

/***
 * 完成一次计算，指令如下：
 * AGU_COMPUTE @ IFM (仅作AGU所需的cfg)
 * AGU_COMPUTE @ KER
 * CONV_TILE  : 完成一块out tile的卷积计算，假设os，输入为MNK，其中MN一般≈TPUsize；
 *            : 【loop controller】拿到OUT TILE的初始(H,W)坐标，根据order按序下发该tile下每个输出点token对应的(oh,ow)以及token_id
 *            : 【AGU_COMPUTE@IFM/@KER】并行拿到loop后，根据之前锁存的cfg产生隐式的im2col地址
 *            : 【LOAD ENGINE@IFM/@KER】并行拿到地址后，访问L1 fabric，得到数据后统一向tensor core下发
 */
case class loop_ctrl_dp(cfg: TTConfig) extends Bundle {
  val CMD_ID = Bits(cfg.CMD_AW bits)  // todo delete
  val TILE_OH0 = Bits(12 bits)
  val TILE_OW0 = Bits(12 bits)
  val TILE_OC0 = Bits(12 bits)  // optional
  val OH_TILE = Bits(12 bits)
  val OW_TILE = Bits(12 bits)
  val OC_TILE = Bits(12 bits)   // optional
  val ORDER = Bits(2 bits)
  // OH_STEP/OW_STEP...

}

case class otile_cfg(cfg: TTConfig) extends Bundle {
  val OH_TILE = Bits(12 bits)
  val OW_TILE = Bits(12 bits)
  val OC_TILE = Bits(12 bits)   // optional
  val ORDER = Bits(2 bits)
  val TILE_OH0 = Bits(12 bits)
  val TILE_OW0 = Bits(12 bits)
  val TILE_OC0 = Bits(12 bits)
  // OH_STEP/OW_STEP...

}

case class loop_ctrl_scan(cfg: TTConfig, lane_num: Int) extends Bundle {
  val oh = Vec.fill(lane_num)(Bits(12 bits))
  val ow = Vec.fill(lane_num)(Bits(12 bits))
  val oc = (Bits(12 bits))
  val token_id = Vec.fill(lane_num)(Bits(12+2*log2Up(cfg.TPU_O) bits))
  val mask = Vec.fill(lane_num)(Bool())
  // OH_STEP/OW_STEP...

}

case class tensor_dp(cfg: TTConfig) extends Bundle{
  val CMD_ID = Bits(cfg.CMD_AW bits)
  val K_TILE = Bits(12 bits)
  val M_TILE = Bits(12 bits)
  val N_TILE = Bits(12 bits)

}

case class alu_int_dp(cfg: TTConfig) extends Bundle{
  val CMD_ID = Bits(cfg.CMD_AW bits)
  val OP_DEC = Bits(8 bits)
  val WORD_0 = Bits(32 bits)
  val WORD_1 = Bits(32 bits)
  val WORD_2 = Bits(32 bits)
  val WORD_3 = Bits(32 bits)
  val PADDING = Bits(8 bits)
}

case class alu_fp_dp(cfg: TTConfig) extends Bundle{
  val CMD_ID = Bits(cfg.CMD_AW bits)
  val OP_DEC = Bits(8 bits)
  val WORD_0 = Bits(32 bits)
  val WORD_1 = Bits(32 bits)
  val WORD_2 = Bits(32 bits)
  val WORD_3 = Bits(32 bits)
  val PADDING = Bits(8 bits)
}

case class store_dp(cfg: TTConfig) extends Bundle{
  val CMD_ID = Bits(cfg.CMD_AW bits)
  val OP_DEC = Bits(8 bits)
  val WORD_0 = Bits(32 bits)
  val WORD_1 = Bits(32 bits)
  val WORD_2 = Bits(32 bits)
  val WORD_3 = Bits(32 bits)
  val PADDING = Bits(8 bits)
}

case class alu_back(cfg: TTConfig) extends Bundle{
  val CMD_ID = Bits(cfg.CMD_AW bits)
}

