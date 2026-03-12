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
  val EXP_MIN_NORMAL = 1 - BIAS      // 正规数最小指数（非正规数阈值）
  val LZC_THRESHOLD_SUBNORMAL = 47   // 触发非正规数的LZC阈值
  val LZC_THRESHOLD_ZERO = 71        // 触发置零的LZC阈值

  val a_sign = io.a(sig_w + exp_w)
  val b_sign = io.b(sig_w + exp_w)
  val c_sign = io.c(sig_w + exp_w)
  val a_exp = io.a(sig_w + exp_w - 1 downto sig_w).asUInt
  val b_exp = io.b(sig_w + exp_w - 1 downto sig_w).asUInt
  val c_exp = io.c(sig_w + exp_w - 1 downto sig_w).asUInt
  val a_man = U(1,1 bits) @@ io.a(sig_w - 1 downto 0).asUInt
  val b_man = U(1,1 bits) @@ io.b(sig_w - 1 downto 0).asUInt
  val c_man = U(1,1 bits) @@ io.c(sig_w - 1 downto 0).asUInt

  // stage.1
  val product_sign = a_sign ^ b_sign
  val product_exp = a_exp + b_exp - 127

  val exp_diff_dir = product_exp > c_exp
  val exp_diff = UInt(exp_w bits)
  when(exp_diff_dir) {
    exp_diff := product_exp - c_exp
  } .otherwise{
    exp_diff := c_exp - product_exp
  }

  val product_man = (a_man * b_man)
  val product_shift = UInt(72 bits)
  val c_man_shift = UInt(72 bits)
  val c_man_ext = c_man << 23
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
  val c_man_align = c_man_shift

  val product_exp_shift = exp_diff_dir? product_exp | c_exp
  val sign_extension = product_sign ? S(-1, 24 bits) | S(0, 24 bits)
  val product_man_signed = sign_extension @@ product_shift.asSInt
  //val product_man_align = (product_sign ## product_man_signed(70 downto 24) ## U(0, 24 bits)).asSInt //(product_man_signed << 24).resize(72)
  val product_man_align = product_shift(71 downto 24) ## U(0, 24 bits)
  val sum_man = SInt(72 bits)
  when(c_sign) {
    sum_man := product_sign?  -(product_man_align.asUInt + c_man_align).asSInt.resize(72) | (product_man_align.asSInt.resize(72) - c_man_align.asSInt.resize(72))
  } .otherwise{
    sum_man := product_sign?  (-product_man_align.asSInt.resize(72) + c_man_align.asSInt.resize(72)) | (product_man_align.asSInt.resize(72) + c_man_align.asSInt.resize(72))
  }

  val sum_man_vec = Vec.fill(sum_man.getBitsWidth)(Bits())
  sum_man_vec := (sum_man.abs.reversed.asBits).subdivideIn(1 bits)
  val (found, lzc) = sum_man_vec.sFindFirst(_ === B(1))
  // 修正LZC：全零时LZC=72，否则取实际值
  val lzc_real = Mux(!found, U(72, 7 bits), lzc.resize(7))

  // 6. LZC分级判断
  val lzc_large_subnormal = (lzc_real >= LZC_THRESHOLD_SUBNORMAL) && (lzc_real < LZC_THRESHOLD_ZERO)
  val lzc_huge_zero = lzc_real >= LZC_THRESHOLD_ZERO
  val lzc_normal = lzc_real < LZC_THRESHOLD_SUBNORMAL

  val norm_man = (sum_man.abs << lzc_real).resize(72)
  val result_exp = product_exp_shift - lzc_real + 24 + norm_man(71).asUInt
  val result_man = (norm_man >> 24).resize(48)

  val guard_bit = norm_man(47)
  val round_bit = norm_man(46)
  val sticky_bit = norm_man(45 downto 0).asBits.orR
  // 舍入决策逻辑
  val round_up = guard_bit & (round_bit | sticky_bit | result_man(24)) // 向偶数舍入

  val man_rounded = result_man(46 downto 24) + round_up.asUInt  // 取高23位+进位

  // 溢出处理（舍入导致进位）
  val man_overflow =result_man(46 downto 24).asBits.andR & round_up
  val exp_final = result_exp(7 downto 0) + man_overflow.asUInt
  val man_final = man_rounded

  val norm_num = (sum_man(71).asUInt @@ exp_final @@ man_final).resize(sig_w + exp_w + 1)
  val sub_sign = sum_man(71) && man_final.asBits.orR
  val subnorm_num = lzc_huge_zero?  U(0, 1+exp_w+sig_w bits) | (sub_sign.asUInt @@ U(0,exp_w bits) @@ man_final).resize(sig_w + exp_w + 1)
  val z = lzc_normal? norm_num | subnorm_num
  io.z := z.asBits

}