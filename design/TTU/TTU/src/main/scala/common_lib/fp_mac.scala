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

  val c_man_shift = UInt(72 bits)
  val c_man_ext = c_man << 24
  when(exp_diff_dir) {
    c_man_shift := (c_man_ext >> exp_diff).resize(72)
  } .otherwise{
    c_man_shift := c_man_ext.resize(72)
  }
  val c_man_align = c_man_shift

  val guard_bit = c_man_align(23)
  val round_bit = c_man_align(22)
  val sticky_bit = c_man_align(21 downto 0).asBits.orR

  val product_man = a_man * b_man
  val sign_extension = product_sign ? S(-1, 24 bits) | S(0, 24 bits)
  val product_man_signed = sign_extension @@ product_man.asSInt
  val product_man_align = (product_sign ## product_man_signed(70 downto 24) ## U(0, 24 bits)).asSInt //(product_man_signed << 24).resize(72)
  val sum_man = SInt(72 bits)
  when(c_sign) {
    sum_man := product_man_align - c_man_align.asSInt
  } .otherwise{
    sum_man := product_man_align + c_man_align.asSInt
  }

  val sum_man_vec = Vec.fill(sum_man.getBitsWidth)(Bits())
  sum_man_vec := (sum_man.reversed.asBits).subdivideIn(1 bits)
  val (found, lzc_plus) = sum_man_vec.sFindFirst(_ === B(1))
  val lzc = lzc_plus - 1
  val norm_man = (sum_man << lzc).resize(72)
  val result_exp = product_exp - lzc + norm_man(71).asUInt
  val result_man = norm_man.asUInt >> (24 + norm_man(71).asUInt)

  // 舍入决策逻辑
  val round_up = False//guard_bit & (round_bit | sticky_bit | norm_man(24)) // 向偶数舍入

  val man_rounded = result_man(46 downto 24) + round_up.asUInt  // 取高23位+进位

  // 溢出处理（舍入导致进位）
  val man_overflow =result_man(46 downto 24).asBits.andR & round_up
  val exp_final = result_exp(7 downto 0) + man_overflow.asUInt
  val man_final = man_rounded

  val z = (sum_man(71).asUInt @@ exp_final @@ man_final).resize(sig_w + exp_w + 1)
  io.z := z.asBits

}
