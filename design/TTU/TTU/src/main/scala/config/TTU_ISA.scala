package config
import spinal.core._


object TTU_ISA {
  def AGU_DMA            = M"00000001"
  def AGU_COMPUTE_IFM    = M"00000010"
  def AGU_COMPUTE_KER    = M"11111110"
  def DMA_AGU            = M"00000011"
  def DMA_NORMAL         = M"00000100"
  def STORE              = M"00000101"
  def BUFF_SW            = M"00000110"
  def WAIT               = M"00000111"
  def CONV_TILE          = M"10000001"
  def VINT               = M"10000010"
  def VFP                = M"10000011"
  // todo expend
}
