import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.amba4.axi._
import config._
import spinal.core.sim._
import java.io.PrintWriter
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer


object hdlcfg extends SpinalConfig(
  targetDirectory        = "build/s2rtl/ttu_core",        // 输出目录
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


object ttu_top {
  def main(args: Array[String]) {
    val mcfg = TTConfig()
    hdlcfg.generateSystemVerilog(new ttu_core(mcfg))
  }
}




