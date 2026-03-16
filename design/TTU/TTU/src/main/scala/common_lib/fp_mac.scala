package common_lib
import spinal.core._
import spinal.lib._

case class fp_mac(sig_w: Int, exp_w: Int, comp: Int) extends Component {
  // 校验参数合法性（避免无效配置）
  require(exp_w >= 3 && exp_w <= 15, "exp_w must be 3~15 (standard float range)")
  require(sig_w >= 2 && sig_w <= 63, "sig_w must be 2~63 (standard float range)")
  val io = new Bundle {
    val a = in Bits(sig_w + exp_w + 1 bits)
    val b = in Bits(sig_w + exp_w + 1 bits)
    val c = in Bits(sig_w + exp_w + 1 bits)
    val rnd = in Bits(3 bits)
    val z = out Bits(sig_w + exp_w + 1 bits)
    val status = out Bits(8 bits)
  }
  // 浮点常量（适配非正规数/下溢）
  def BIAS = (1 << (exp_w - 1)) - 1  // 偏置值（FP32=127）
  def LZC_THRESHOLD_SUBNORMAL = 48   // 触发非正规数的LZC阈值
  def LZC_THRESHOLD_ZERO = 72        // 触发置零的LZC阈值

  def MAN_WD = sig_w + 1
  def MAN_TWD = 3*MAN_WD
  def EXP_DIFF_THRESHOLD = 2*MAN_WD
  def LZC_WD = log2Up(MAN_TWD)

  /** stage 1. **/
  // 1a. 预处理
  val a_sign = io.a(sig_w + exp_w)
  val b_sign = io.b(sig_w + exp_w)
  val c_sign = io.c(sig_w + exp_w)
  val a_exp = io.a(sig_w + exp_w - 1 downto sig_w).asUInt
  val b_exp = io.b(sig_w + exp_w - 1 downto sig_w).asUInt
  val c_exp = io.c(sig_w + exp_w - 1 downto sig_w).asUInt
  val a_man = io.a(sig_w - 1 downto 0).asUInt
  val b_man = io.b(sig_w - 1 downto 0).asUInt
  val c_man = io.c(sig_w - 1 downto 0).asUInt

  val a_hidden = (a_exp =/= 0).asUInt.resize(1)
  val b_hidden = (b_exp =/= 0).asUInt.resize(1)
  val c_hidden = (c_exp =/= 0).asUInt.resize(1)

  val a_man_1 = a_hidden @@ a_man
  val b_man_1 = b_hidden @@ b_man
  val c_man_1 = c_hidden @@ c_man

  val product_sign = a_sign ^ b_sign

  // 1b. 检测输入 Inf/NaN（FP32 规则：指数全1=特殊值）
  val exp_all_one = UInt(exp_w bits)
  exp_all_one := (default -> true)
  val a_is_inf = (a_exp === exp_all_one) && (a_man === U(0))
  val b_is_inf = (b_exp === exp_all_one) && (b_man === U(0))
  val c_is_inf = (c_exp === exp_all_one) && (c_man === U(0))
  val a_is_nan = (a_exp === exp_all_one) && (a_man =/= U(0))
  val b_is_nan = (b_exp === exp_all_one) && (b_man =/= U(0))
  val c_is_nan = (c_exp === exp_all_one) && (c_man =/= U(0))

  val output_nan = a_is_nan | b_is_nan | c_is_nan
  val a_b_inf = a_is_inf | b_is_inf

  val a_zero = (a_exp === U(0)) && (a_man === U(0))
  val b_zero = (b_exp === U(0)) && (b_man === U(0))
  val inf_x_zero = (a_is_inf & b_zero) | (b_is_inf & a_zero)
  val prod_is_inf = a_b_inf && !inf_x_zero  // a×b是有效Inf（非Inf×0）
  val inf_cancel = prod_is_inf && c_is_inf && (product_sign =/= c_sign)
  val output_nan_inf = output_nan | inf_x_zero | inf_cancel
  val inf_sign = Mux(a_b_inf, product_sign, c_sign)  // a/b含Inf→取prod_sign，仅c含Inf→取c_sign


  // 1c. exp对齐
  val product_exp = a_exp + b_exp - BIAS

  val exp_diff_dir = product_exp > c_exp
  val exp_diff = UInt(exp_w bits)
  when(exp_diff_dir) {
    exp_diff := product_exp - c_exp
  } .otherwise{
    exp_diff := c_exp - product_exp
  }

  // 1d. product_man计算
  val product_man = ((a_man_1 * b_man_1).resize(MAN_TWD) << MAN_WD).resize(MAN_TWD)
  val product_shift = UInt(MAN_TWD bits)
  val c_man_shift = UInt(MAN_TWD bits)
  val c_man_ext = (c_man_1.resize(MAN_TWD) << MAN_WD).resize(MAN_TWD)

  /** stage 2. **/
  // 2a. man对齐
  when(exp_diff_dir) {
    product_shift := product_man.resize(MAN_TWD)
    when(exp_diff < EXP_DIFF_THRESHOLD) {
      c_man_shift := (c_man_ext >> exp_diff).resize(MAN_TWD)
    } .otherwise{
      c_man_shift := 0
    }
  } .otherwise{
    c_man_shift := c_man_ext.resize(MAN_TWD)
    when(exp_diff < EXP_DIFF_THRESHOLD) {
      product_shift := (product_man >> exp_diff).resize(MAN_TWD)
    } .otherwise{
      product_shift := 0
    }
  }
  val c_man_align = (c_man_shift << sig_w).resize(MAN_TWD)

  val product_exp_shift = exp_diff_dir? product_exp | c_exp
  val product_man_align = product_shift//(71 downto 24) ## U(0, 24 bits)

  // 2b. man相加
  val prod_mag_s = SInt(MAN_TWD+1 bits)
  val c_mag_s    = SInt(MAN_TWD+1 bits)

  prod_mag_s := product_man_align.resize(MAN_TWD+1).asSInt
  c_mag_s    := c_man_align.resize(MAN_TWD+1).asSInt

  val prod_val = SInt(MAN_TWD+1 bits)
  val c_val    = SInt(MAN_TWD+1 bits)

  prod_val := Mux(product_sign, -prod_mag_s, prod_mag_s)
  c_val    := Mux(c_sign,       -c_mag_s,    c_mag_s)

  val sum_man = SInt(MAN_TWD+2 bits)
  sum_man := prod_val.resize(MAN_TWD+2) + c_val.resize(MAN_TWD+2)

  // 2c. sum_man转绝对值
  val sum_man_abs = sum_man.abs(MAN_TWD-1 downto 0)
  val sum_man_overflow = sum_man.abs(MAN_TWD)
  val sum_man_vec = Vec.tabulate(MAN_TWD)(i => sum_man_abs.asBits(MAN_TWD-1 - i))

  /** stage 3. **/
  // 3a. sum_man的lzc
  val (found, lzc) = sum_man_vec.sFindFirst(_ === True)
  // 修正LZC：全零时LZC=72，否则取实际值
  val lzc_real = sum_man_overflow?  U(0,LZC_WD bits) | Mux(!found, U(MAN_TWD, LZC_WD bits), lzc.resize(LZC_WD))


  // 3b. LZC分级判断
  val lzc_large_subnormal = (lzc_real >= LZC_THRESHOLD_SUBNORMAL) && (lzc_real < LZC_THRESHOLD_ZERO)
  val lzc_huge_zero = lzc_real >= LZC_THRESHOLD_ZERO
  val lzc_normal = lzc_real < LZC_THRESHOLD_SUBNORMAL

  /** stage 4. **/
  // 4a. sub_man的归一化
  val norm_man = sum_man_overflow?  (sum_man.abs(MAN_TWD downto 0) >> 1).resize(MAN_TWD) | (sum_man_abs << lzc_real).resize(MAN_TWD)
  val result_exp = product_exp_shift - lzc_real + norm_man(71).asUInt + sum_man_overflow.asUInt
  val result_man = norm_man(MAN_TWD-1 downto MAN_TWD-1-sig_w)//(norm_man >> 24).resize(48)
  val result_sign = Bool()
  result_sign := sum_man < 0
  when(sum_man === 0){
    result_sign := False
  }

  // 4b. 舍入逻辑
  val guard_bit = norm_man(MAN_TWD-1-sig_w-1)
  val round_bit = norm_man(MAN_TWD-1-sig_w-2)
  val sticky_bit = norm_man(MAN_TWD-1-sig_w-3 downto MAN_WD).asBits.orR

  // 根据rnd选择舍入策略
  val round_up = (io.rnd.mux(
    // 000: 就近舍入（偶）
    0 -> (guard_bit & (round_bit | sticky_bit | result_man(0))),
    // 001: 向零舍入
    1 -> False,
    // 010: 向下舍入（-∞）
    2 -> (result_sign & (guard_bit | round_bit | sticky_bit)),
    // 011: 向上舍入（+∞）
    3 -> (!result_sign & (guard_bit | round_bit | sticky_bit)),
    // 100: 就近舍入（大）
    4 -> (guard_bit & (round_bit | sticky_bit)),
    // 101~111: 默认按000处理
    default -> (guard_bit & (round_bit | sticky_bit | result_man(0)))
  )).asBits.asBool

  val man_rounded = result_man(sig_w-1 downto 0) + round_up.asUInt  // 取高23位+进位

  // 4c. 溢出处理（舍入导致进位）
  val man_overflow = result_man(sig_w-1 downto 0).asBits.andR & round_up
  val exp_final = result_exp(exp_w-1 downto 0) + man_overflow.asUInt
  val man_final = exp_final.asBits.andR?  U(0, sig_w bits) | man_rounded

  // 4d. 特殊数处理
  val norm_num = (result_sign.asUInt @@ exp_final @@ man_final).resize(sig_w + exp_w + 1)
  val sub_sign = result_sign && man_final.asBits.orR
  val subnorm_num = lzc_huge_zero?  U(0, 1+exp_w+sig_w bits) | (sub_sign.asUInt @@ U(0,exp_w bits) @@ man_final).resize(sig_w + exp_w + 1)
  val z = lzc_normal? norm_num | subnorm_num

  /** stage 5. **/
  // 输出
  when(output_nan_inf) {
    // 输出 NaN：0 11111111 10000000000000000000000
    //io.z := B"0_11111111_10000000000000000000000"
    io.z := B(1 bits, default->False) ## B(exp_w bits, default->True) ## B(sig_w bits, (sig_w-1)->True, default->False)
    io.status := B"00000001"  // NaN 标志
  } .elsewhen(a_b_inf | c_is_inf) {
    // 输出 Inf：符号位 + 全1指数 + 全0尾数
    //io.z := (inf_sign ## B"11111111" ## B"00000000000000000000000").asBits
    io.z := inf_sign ## B(exp_w bits, default->True) ## B(sig_w bits, default->False)
    io.status := B"00000010"  // Inf 标志
  } .otherwise {
    io.z := z.asBits
    io.status := B"00000000"
  }


}