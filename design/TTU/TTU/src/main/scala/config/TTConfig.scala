package config
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.amba4.axi._

/** 统一配置中心：基础字段可覆写；派生字段自动计算 */
case class TTConfig(
                       TPU_H    : Int     = 8,
                       TPU_W    : Int     = 8,
                       TPU_IDWMAX: Int     = 32, // max support fp32
                       TPU_ODWMAX: Int     = 32, // max support fp32
                       TPU_PSDWMAX: Int     = 64,
                       TPU_IDTYPE: String = "UINT8",
                       TPU_KDTYPE: String = "INT8",
                       CMD_WB    : Int     = 32,
                       CMD_AW    : Int     = 16,
                       CMD_MSIZE : Int     = 1024,
                       CMD_BASE  : Int     = 0,
                       CMD_DEEPTH: Int     = 16,  // todo consider
                       SCB_DEEPTH: Int     = 8,
                       L1_ISIZE   : Int     = 1024, // support 双缓冲
                       L1_KSIZE   : Int     = 1024,
                       L1_OSIZE   : Int     = 1024,
                       L1_BANK    : Int     = 4,    // per PPBANK
                       L1_AW      : Int     = 20,
                       L1_DW      : Int     = 32*8, // 8x8 PE with max 32bit
                       L1_BASE    : Int     = 1024*32,
                       DMA_DLY    : Int     = 2,  // default
                       AGU_DMA_DEPTH: Int   = 8,
                       LOOP_DEPTH: Int      = 8,
                       LD_REQ_DEPTH: Int    = 8,
                       PACKET_DEPTH: Int    = 8,
                       sparsity : Boolean = true,
                       debug_en        : Boolean = true
                     ) {
  // —— 派生量（自动计算，不建议外部直接覆写）——
  def TPU_O               = if(TPU_H > TPU_W) TPU_W else TPU_H
  def L1_DB               = L1_DW/8
  def L1_IAW              = log2Up(L1_ISIZE)
  def L1_KAW              = log2Up(L1_KSIZE)
  def L1_OAW              = log2Up(L1_OSIZE)
  def L1_IBASE            = L1_BASE
  def L1_KBASE            = L1_IBASE + 65536  // reserve 64KB
  def L1_OBASE            = L1_KBASE + 65536  // reserve 64KB
  def L1_PPBIT            = L1_AW - 1
  def L1_PPBASE           = (1 << L1_PPBIT)
  def ctrlAxiLite         = AxiLite4Config(addressWidth = CMD_AW, dataWidth = 32)
  def memAxi              = Axi4Config(addressWidth = L1_AW, dataWidth = L1_DW, idWidth = 4)
  def CMD_DW              = CMD_WB * 8
  def DMA_DW              = L1_DW

}




