package common_lib
import spinal.core._
import spinal.lib._

/***
 * CSA, compress 3 to 2
 */

case class csa_3t2(dw: Int) extends Component {
  val io = new Bundle {
    val a = in Bits (dw bits)
    val b = in Bits (dw bits)
    val c = in Bits (dw bits)
    val sum = out Bits (dw bits)
    val carry = out Bits (dw bits)
  }
  io.sum := io.a ^ io.b ^ io.c
  io.carry := (((io.a & io.b) | (io.a & io.c) | (io.b & io.c)) << 1).resize(dw)
}
