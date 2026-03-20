package common_lib
import spinal.core._
import spinal.lib._

/***
 * CLA
 */

case class CLA(width: Int) extends Component {
  val io = new Bundle {
    val a = in Bits(width bits)
    val b = in Bits(width bits)
    val cin = in Bool()
    val sum = out Bits(width bits)
    val cout = out Bool()
  }

  // 生成 G 和 P
  val g = io.a & io.b
  val p = io.a ^ io.b

  // 进位向量
  val c = Vec(Bool(), width + 1)
  c(0) := io.cin

  // 并行计算所有进位
  for (i <- 0 until width) {
    // 计算 C_{i+1}
    var term = g(i)
    for (j <- 0 to i) {
      val prop = if (j + 1 > i) True else p(i downto j+1).andR
      term |= prop & g(j)
      //term = term | (p(i downto j).andR & g(j))  // 需要实现与归约
    }
    // 更清晰的写法：
    val propagate_prefix = p(i downto 0).andR
    term = term | (propagate_prefix & io.cin)
    c(i + 1) := term
  }

  // 求和
  io.sum := p ^ c.take(width).asBits
  io.cout := c(width)
}
