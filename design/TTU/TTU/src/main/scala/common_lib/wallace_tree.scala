package common_lib
import spinal.core._
import spinal.lib._

/***
 * Wallace Tree, use CSA array & CLA
 */

case class wallace_tree(dw: Int, sw: Int, num: Int) extends Component {
  require(num == 13, "this wallace tree only support num = 13, please change !")
  val io = new Bundle {
    val din_vec = Vec.fill(num)(in Bits (dw bits))
    val sum = out Bits (sw bits)
  }
  // LEVEL 0
  val csa0_0 = new csa_3t2(dw)
  val csa0_1 = new csa_3t2(dw)
  val csa0_2 = new csa_3t2(dw)
  val csa0_3 = new csa_3t2(dw)
  val csa0_4 = new csa_3t2(dw)
  // LEVEL 1
  val csa1_0 = new csa_3t2(dw)
  val csa1_1 = new csa_3t2(dw)
  val csa1_2 = new csa_3t2(dw)
  val csa1_3 = new csa_3t2(dw)
  // LEVEL 2
  val csa2_0 = new csa_3t2(dw)
  val csa2_1 = new csa_3t2(dw)
  val csa2_2 = new csa_3t2(dw)
  // LEVEL 3
  val csa3_0 = new csa_3t2(dw)
  val csa3_1 = new csa_3t2(dw)
  // LEVEL 4
  val csa4_0 = new csa_3t2(dw)
  // LEVEL LAST: CLA

  csa0_0.io.a := io.din_vec(0)
  csa0_0.io.b := io.din_vec(1)
  csa0_0.io.c := io.din_vec(2)
  csa0_1.io.a := io.din_vec(3)
  csa0_1.io.b := io.din_vec(4)
  csa0_1.io.c := io.din_vec(5)
  csa0_2.io.a := io.din_vec(6)
  csa0_2.io.b := io.din_vec(7)
  csa0_2.io.c := io.din_vec(8)
  csa0_3.io.a := io.din_vec(9)
  csa0_3.io.b := io.din_vec(10)
  csa0_3.io.c := io.din_vec(11)

  csa1_0.io.a := csa0_0.io.sum
  csa1_0.io.b := csa0_0.io.carry
  csa1_0.io.c := csa0_1.io.sum
  csa1_1.io.a := csa0_1.io.carry
  csa1_1.io.b := csa0_2.io.sum
  csa1_1.io.c := csa0_2.io.carry
  csa1_2.io.a := csa0_3.io.sum
  csa1_2.io.b := csa0_3.io.carry
  csa1_2.io.c := io.din_vec(12)

  csa2_0.io.a := csa1_0.io.sum
  csa2_0.io.b := csa1_0.io.carry
  csa2_0.io.c := csa1_1.io.sum
  csa2_1.io.a := csa1_1.io.carry
  csa2_1.io.b := csa1_2.io.sum
  csa2_1.io.c := csa1_2.io.carry

  csa3_0.io.a := csa2_0.io.sum
  csa3_0.io.b := csa2_0.io.carry
  csa3_0.io.c := csa2_1.io.sum

  csa4_0.io.a := csa3_0.io.sum
  csa4_0.io.b := csa3_0.io.carry
  csa4_0.io.c := csa2_1.io.carry

  //io.sum := (csa4_0.io.sum.asSInt + csa4_0.io.carry.asSInt).resize(sw).asBits
  // todo: replace CLA
  val cla_adder = new CLA(dw)
  cla_adder.io.a := csa4_0.io.sum.asSInt.asBits
  cla_adder.io.b := csa4_0.io.carry.asSInt.asBits
  cla_adder.io.cin := False
  val sum_cla = (cla_adder.io.cout ## cla_adder.io.sum).asSInt.resize(sw).asBits
  //val sum_error = (io.sum =/= sum_cla)
  io.sum := sum_cla

}

