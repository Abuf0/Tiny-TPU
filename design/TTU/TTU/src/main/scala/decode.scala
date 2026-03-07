import spinal.core._
import spinal.lib._
import config._

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


case class decode(cfg: TTConfig) extends Component {
  val io = new Bundle {
    val cmd_id_in = in Bits(cfg.CMD_AW bits)
    val cmd_stream_in = slave Stream (Bits(cfg.CMD_DW bits))
    val dec_out = master Stream (dec_map(cfg))
  }
  val flow_init = Flow(dec_map(cfg))
  flow_init.valid := False
  flow_init.payload.assignFromBits(B(0,dec_map(cfg).getBitsWidth bits))
  val dec_flow = Reg(Flow(dec_map(cfg))) init(flow_init)  // buffer
  val dec_flow_buff = Reg(Flow(dec_map(cfg))) init(flow_init)

  io.cmd_stream_in.ready := io.dec_out.ready
  io.dec_out.valid := dec_flow_buff.valid
  io.dec_out.payload := dec_flow_buff.payload

  when(io.dec_out.ready){
    dec_flow_buff := dec_flow
  } .otherwise{
    //dec_flow_buff.valid := False
  }

  when(io.dec_out.ready && io.cmd_stream_in.valid){
    dec_flow.valid := True
    dec_flow.payload.CMD_ID := io.cmd_id_in
    dec_flow.payload.OP_DEC := io.cmd_stream_in.payload(7 downto 0)
    dec_flow.payload.DEP_CNT := io.cmd_stream_in.payload(15 downto 8).asUInt
    dec_flow.payload.SUCC_CNT := io.cmd_stream_in.payload(23 downto 16).asUInt
    dec_flow.payload.DEP_ID_0 := io.cmd_stream_in.payload(47 downto 32)
    dec_flow.payload.DEP_ID_1 := io.cmd_stream_in.payload(63 downto 48)
    dec_flow.payload.DEP_ID_2 := io.cmd_stream_in.payload(79 downto 64)
    dec_flow.payload.DEP_ID_3 := io.cmd_stream_in.payload(95 downto 80)
    dec_flow.payload.WORD_0 := io.cmd_stream_in.payload(127 downto 96)
    dec_flow.payload.WORD_1 := io.cmd_stream_in.payload(159 downto 128)
    dec_flow.payload.WORD_2 := io.cmd_stream_in.payload(191 downto 160)
    dec_flow.payload.WORD_3 := io.cmd_stream_in.payload(223 downto 192)
    dec_flow.payload.WORD_4 := io.cmd_stream_in.payload(255 downto 224)
    dec_flow.payload.RESERVE_BYTE := io.cmd_stream_in.payload(31 downto 24)
  } .elsewhen(dec_flow_buff.valid && io.dec_out.ready) {
    dec_flow.valid := False
  }

}
