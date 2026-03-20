package common_lib
import spinal.core._
import spinal.lib._

case class mult_booth4(aw: Int, bw: Int) extends Component{
  require(aw <= bw, "aw must <= bw, please exchange position!")
  val io = new Bundle{
    val a = in Bits(aw bits)
    val b = in Bits(bw bits)
    val tc = in Bool()  // 0-unsigned, 1-signed
    val z = out Bits(aw+bw bits)
  }

  def faw = aw + 1
  def fbw = bw +1
  def GRP_NUM = (faw+1)/2 //(faw+1)/2
  println(f"[Compile] mult booth4 group num : ${GRP_NUM}")

  val a_ts = io.tc? Cat(io.a.msb, io.a) | Cat(False, io.a)
  val b_ts = io.tc? Cat(io.b.msb, io.b) | Cat(False, io.b)

  val b_ext = SInt(faw+1 bits)
  when(io.tc) {
    b_ext := b_ts.asSInt.resize(faw+1)
  } otherwise {
    b_ext := b_ts.asUInt.resize(faw+1).asSInt
  }

  val a_ext = Bits(faw+2 bits)
  //a_ext := ((io.tc && io.a(faw-1)) ## io.a.asSInt ## B(0,1 bits)).asSInt
  when(io.tc) {
    a_ext := Cat(a_ts.msb, a_ts.asSInt, False)   // 有符号：符号位 + a + 0
  } otherwise {
    a_ext := Cat(False, a_ts.asUInt, False)      // 无符号：0 + a(零扩展) + 0
  }

  val dec_grp =  Vec.fill(GRP_NUM)(Bits(3 bits))
  val pp_grp =  Vec.fill(GRP_NUM)(SInt(faw+fbw bits))
  val pp_sum = SInt(faw+fbw bits)
  for(i<-0 until GRP_NUM){
    dec_grp(i) := a_ext(i*2+2) ## a_ext(i*2+1) ## a_ext(i*2)
    switch(dec_grp(i)){
      is(B"000"){ pp_grp(i) := 0 }
      is(B"001"){ pp_grp(i) := b_ext.resize(faw+fbw) }
      is(B"010"){ pp_grp(i) := b_ext.resize(faw+fbw) }
      is(B"011"){ pp_grp(i) := (b_ext.resize(faw+fbw) << 1).resize(faw+fbw) }
      is(B"100"){ pp_grp(i) := -(b_ext.resize(faw+fbw) << 1).resize(faw+fbw) }
      is(B"101"){ pp_grp(i) := -b_ext.resize(faw+fbw) }
      is(B"110"){ pp_grp(i) := -b_ext.resize(faw+fbw) }
      is(B"111"){ pp_grp(i) := 0 }
    }
  }

  val pp_shifted = for (i <- 0 until GRP_NUM) yield (pp_grp(i) << (2 * i)).resize(faw+fbw)
  pp_sum := pp_shifted.reduceBalancedTree(_ + _).resize(faw + fbw)
  // todo: replace adder tree
  val wallace_adder = new wallace_tree(faw+fbw, faw+fbw, GRP_NUM)
  val pp_bit = Vec.fill(GRP_NUM)(Bits(faw+fbw bits))
  for (i <- 0 until GRP_NUM){
    pp_bit(i) := pp_shifted(i).asBits
  }
  wallace_adder.io.din_vec := pp_bit
  val pp_sum_w = wallace_adder.io.sum
  val pp_sum_error = (pp_sum_w =/= pp_sum.asBits)

  io.z := pp_sum.resize(aw+bw).asBits

}

