package common_lib
import spinal.core._
import spinal.lib._

case class fp_mac(sig_w: Int, exp_w: Int, comp: Int) extends Component {
  val io = new Bundle {
    val a = in Bits(sig_w + exp_w + 1 bits)
    val b = in Bits(sig_w + exp_w + 1 bits)
    val c = in Bits(sig_w + exp_w + 1 bits)
    val rnd = in Bits(3 bits)
    val z = out Bits(sig_w + exp_w + 1 bits)
    val status = out Bits(8 bits)
  }
  // 浮点常量（适配非正规数/下溢）
  val BIAS = (1 << (exp_w - 1)) - 1  // 偏置值（FP32=127）
  val EXP_MIN_NORMAL = 1             // 正规数最小指数（非正规数阈值）
  val LZC_THRESHOLD_SUBNORMAL = 48   // 触发非正规数的LZC阈值
  val LZC_THRESHOLD_ZERO = 72        // 触发置零的LZC阈值

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

  // 2. 检测输入 Inf/NaN（FP32 规则：指数全1=特殊值）
  val a_is_inf = (a_exp === U(255)) && (a_man === U(0))
  val b_is_inf = (b_exp === U(255)) && (b_man === U(0))
  val c_is_inf = (c_exp === U(255)) && (c_man === U(0))
  val a_is_nan = (a_exp === U(255)) && (a_man =/= U(0))
  val b_is_nan = (b_exp === U(255)) && (b_man =/= U(0))
  val c_is_nan = (c_exp === U(255)) && (c_man =/= U(0))

  // 3. 核心判断：NaN/Inf 优先级逻辑
  val output_nan = a_is_nan | b_is_nan | c_is_nan
  val a_b_inf = a_is_inf | b_is_inf

  val product_sign = a_sign ^ b_sign

  // 子判断：Inf × 0 → NaN
  val a_zero = (a_exp === U(0)) && (a_man === U(0))
  val b_zero = (b_exp === U(0)) && (b_man === U(0))
  val inf_x_zero = (a_is_inf & b_zero) | (b_is_inf & a_zero)
  // Inf相消：a×b=Inf 且 c=Inf 且符号相反 → 未定式，返回NaN
  val prod_is_inf = a_b_inf && !inf_x_zero  // a×b是有效Inf（非Inf×0）
  val inf_cancel = prod_is_inf && c_is_inf && (product_sign =/= c_sign)
  val output_nan_inf = output_nan | inf_x_zero | inf_cancel

  // 4. 决定输出 Inf 的符号

  // stage.1
  val product_exp = a_exp + b_exp - 127

  val inf_sign = Mux(a_b_inf, product_sign, c_sign)  // a/b含Inf→取prod_sign，仅c含Inf→取c_sign


  val exp_diff_dir = product_exp > c_exp
  val exp_diff = UInt(exp_w bits)
  when(exp_diff_dir) {
    exp_diff := product_exp - c_exp
  } .otherwise{
    exp_diff := c_exp - product_exp
  }

  val product_man = ((a_man_1 * b_man_1) << 24).resize(72)
  val product_shift = UInt(72 bits)
  val c_man_shift = UInt(72 bits)
  val c_man_ext = c_man_1 << 24
  when(exp_diff_dir) {
    product_shift := product_man.resize(72)
    when(exp_diff < 48) {
      c_man_shift := (c_man_ext >> exp_diff).resize(72)
    } .otherwise{
      c_man_shift := 0
    }
  } .otherwise{
    c_man_shift := c_man_ext.resize(72)
    when(exp_diff < 48) {
      product_shift := (product_man >> exp_diff).resize(72)
    } .otherwise{
      product_shift := 0
    }
  }
  val c_man_align = (c_man_shift << 23).resize(72)

  val product_exp_shift = exp_diff_dir? product_exp | c_exp
  //val sign_extension = product_sign ? S(-1, 24 bits) | S(0, 24 bits)
  val product_man_align = product_shift(71 downto 24) ## U(0, 24 bits)
//  val sum_man = SInt(73 bits)
//  when(c_sign) {
//    sum_man := product_sign?  -(product_man_align.asUInt.resize(73) + c_man_align.resize(73)).asSInt | (product_man_align.resize(73).asSInt - c_man_align.resize(73).asSInt)
//  } .otherwise{
//    sum_man := product_sign?  (-product_man_align.resize(73).asSInt + c_man_align.resize(73).asSInt) | (product_man_align.asUInt.resize(73) + c_man_align.resize(73)).asSInt
//  }

  val prod_mag_s = SInt(73 bits)
  val c_mag_s    = SInt(73 bits)

  prod_mag_s := product_man_align.resize(73).asSInt
  c_mag_s    := c_man_align.resize(73).asSInt

  val prod_val = SInt(73 bits)
  val c_val    = SInt(73 bits)

  prod_val := Mux(product_sign, -prod_mag_s, prod_mag_s)
  c_val    := Mux(c_sign,       -c_mag_s,    c_mag_s)

  val sum_man = SInt(74 bits)
  sum_man := prod_val.resize(74) + c_val.resize(74)

  val sum_man_abs = sum_man.abs(71 downto 0)
  val sum_man_vec = Vec.fill(sum_man_abs.getBitsWidth)(Bits())
  sum_man_vec := (sum_man_abs.reversed.asBits).subdivideIn(1 bits)
  val (found, lzc) = sum_man_vec.sFindFirst(_ === B(1))
  // 修正LZC：全零时LZC=72，否则取实际值
  val lzc_real = Mux(!found, U(72, 7 bits), lzc.resize(7))

  // 6. LZC分级判断
  val lzc_large_subnormal = (lzc_real >= LZC_THRESHOLD_SUBNORMAL) && (lzc_real < LZC_THRESHOLD_ZERO)
  val lzc_huge_zero = lzc_real >= LZC_THRESHOLD_ZERO
  val lzc_normal = lzc_real < LZC_THRESHOLD_SUBNORMAL

  val norm_man = (sum_man_abs << lzc_real).resize(72)
  val result_exp = product_exp_shift - lzc_real + norm_man(71).asUInt
  val result_man = norm_man(71 downto 48)//(norm_man >> 24).resize(48)

  val guard_bit = norm_man(46)
  val round_bit = norm_man(45)
  val sticky_bit = norm_man(44 downto 0).asBits.orR
  // 舍入决策逻辑
  val round_up = guard_bit & (round_bit | sticky_bit | result_man(0)) // 向偶数舍入

  val man_rounded = result_man(22 downto 0) + round_up.asUInt  // 取高23位+进位
  val man_rnd_final = man_rounded(22)?  man_rounded | man_rounded(21 downto 0)@@U(0,1 bits)

  // 溢出处理（舍入导致进位）
  val man_overflow =result_man(22 downto 0).asBits.andR & round_up
  val exp_final = result_exp(7 downto 0) + man_overflow.asUInt
  val man_final = exp_final.asBits.andR?  U(0, sig_w bits) | man_rounded

  val result_sign = Bool()
  result_sign := sum_man < 0
  when(sum_man === 0){
    result_sign := False
  }

  val norm_num = (result_sign.asUInt @@ exp_final @@ man_final).resize(sig_w + exp_w + 1)
  val sub_sign = result_sign && man_final.asBits.orR
  val subnorm_num = lzc_huge_zero?  U(0, 1+exp_w+sig_w bits) | (sub_sign.asUInt @@ U(0,exp_w bits) @@ man_final).resize(sig_w + exp_w + 1)
  val z = lzc_normal? norm_num | subnorm_num
  //io.z := z.asBits

  when(output_nan_inf) {
    // 输出 NaN：0 11111111 10000000000000000000000
    io.z := B"0_11111111_10000000000000000000000"
    io.status := B"00000001"  // NaN 标志
  } .elsewhen(a_b_inf | c_is_inf) {
    // 输出 Inf：符号位 + 全1指数 + 全0尾数
    io.z := (inf_sign ## B"11111111" ## B"00000000000000000000000").asBits
    io.status := B"00000010"  // Inf 标志
  } .otherwise {
    io.z := z.asBits
    io.status := B"00000000"
  }


}