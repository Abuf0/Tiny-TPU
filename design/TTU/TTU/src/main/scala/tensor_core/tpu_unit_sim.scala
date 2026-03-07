package tensor_core

import config.TTConfig
import spinal.core._
import spinal.core.sim._
import java.io.PrintWriter
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer


object hdlcfg extends SpinalConfig(
  targetDirectory        = "build/s2rtl/tensor_core",        // 输出目录
  oneFilePerComponent    = true,         // 每个组件单独一个 .v
  globalPrefix           = "",
  anonymSignalPrefix     = "tmp",
  noAssert               = true,
  enumPrefixEnable       = false,



  defaultClockDomainFrequency = FixedFrequency(480 MHz),
  defaultConfigForClockDomains = ClockDomainConfig(
    resetKind         = ASYNC,
    resetActiveLevel  = LOW
  )
)


object top {
  def main(args: Array[String]) {
    val mcfg = TTConfig()
    hdlcfg.generateSystemVerilog(new tpu_unit(mcfg))
  }
}



