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

  val c_man_align = UInt(72 bits)
  when(exp_diff_dir) {
    c_man_align := c_man.resize(72) >> exp_diff
  } .otherwise{
    c_man_align := c_man.resize(72)
  }
  val product_man = a_man * b_man
  val product_man_signed = (product_sign ## product_man).asSInt.resize(72)
  val product_man_align = (product_man_signed << 24)
  val sum_man = SInt(72 bits)
  when(c_sign) {
    sum_man := product_man_align - c_man_align.asSInt
  } .otherwise{
    sum_man := product_man_align + c_man_align.asSInt
  }

  val lzc = 1
  val norm_man = sum_man << lzc
  val norm_exp = exp_diff - lzc


}
