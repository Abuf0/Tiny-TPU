# Makefile
# 1. 仿真器选择（Verilator） iverilog
SIM ?= icarus
WAVES = 1
# 2. SpinalHDL生成的Verilog文件路径
S2RTL_PATH = D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl
VERILOG_SOURCES = \
${S2RTL_PATH}/ttu_core/enumdefine.sv \
${S2RTL_PATH}/ttu_core/StreamFifo_34.sv \
${S2RTL_PATH}/ttu_core/StreamFifo_33.sv \
${S2RTL_PATH}/ttu_core/StreamFifo_24.sv \
${S2RTL_PATH}/ttu_core/StreamFifo_23.sv \
${S2RTL_PATH}/ttu_core/StreamFifo_15.sv \
${S2RTL_PATH}/ttu_core/StreamFifo_14.sv \
${S2RTL_PATH}/ttu_core/StreamFifo_13.sv \
${S2RTL_PATH}/ttu_core/StreamFifo_12.sv \
${S2RTL_PATH}/ttu_core/StreamFifo_11.sv \
${S2RTL_PATH}/ttu_core/StreamFifo_10.sv \
${S2RTL_PATH}/ttu_core/StreamFifo_9.sv \
${S2RTL_PATH}/ttu_core/StreamFifo_8.sv \
${S2RTL_PATH}/ttu_core/StreamFifo_7.sv \
${S2RTL_PATH}/ttu_core/StreamFifo_6.sv \
${S2RTL_PATH}/ttu_core/StreamFifo_5.sv \
${S2RTL_PATH}/ttu_core/StreamFifo_4.sv \
${S2RTL_PATH}/ttu_core/StreamFifo_3.sv \
${S2RTL_PATH}/ttu_core/StreamFifo_2.sv \
${S2RTL_PATH}/ttu_core/StreamFifo.sv \
${S2RTL_PATH}/ttu_core/DmaEngineAxiToL1.sv \
${S2RTL_PATH}/ttu_core/fetch.sv \
${S2RTL_PATH}/ttu_core/decode.sv \
${S2RTL_PATH}/ttu_core/scoreboard.sv \
${S2RTL_PATH}/ttu_core/dma_wrap.sv \
${S2RTL_PATH}/ttu_core/agu_dma.sv \
${S2RTL_PATH}/ttu_core/agu2dma_ctrl.sv \
${S2RTL_PATH}/ttu_core/agu_compute_ifm.sv \
${S2RTL_PATH}/ttu_core/agu_compute_ker.sv \
${S2RTL_PATH}/ttu_core/load_engine.sv \
${S2RTL_PATH}/ttu_core/load_engine_1.sv \
${S2RTL_PATH}/ttu_core/loop_controller.sv \
${S2RTL_PATH}/ttu_core/store_engine.sv \
${S2RTL_PATH}/ttu_core/tpu_unit.sv \
${S2RTL_PATH}/ttu_core/tensor_core.sv \
${S2RTL_PATH}/ttu_core/alu_int.sv \
${S2RTL_PATH}/ttu_core/alu_fp.sv \
${S2RTL_PATH}/ttu_core/L1_Fabric.sv \
${S2RTL_PATH}/ttu_core/sram_1R1W_wrap.sv \
${S2RTL_PATH}/ttu_core/sram_1R1W_wrap_6.sv \
${S2RTL_PATH}/ttu_core/ttu_core.sv \
${S2RTL_PATH}/ttu_core/fp_mac.sv \

#	${S2RTL_PATH}/tensor_core/StreamFifo.sv \
#    ${S2RTL_PATH}/tensor_core/StreamFifo_1.sv \
#    ${S2RTL_PATH}/tensor_core/StreamFifo_2.sv \
#    ${S2RTL_PATH}/tensor_core/StreamFifo_3.sv \
#    ${S2RTL_PATH}/tensor_core/StreamFifo_4.sv \
#    ${S2RTL_PATH}/tensor_core/StreamFifo_5.sv \
#    ${S2RTL_PATH}/tensor_core/StreamFifo_6.sv \
#    ${S2RTL_PATH}/tensor_core/StreamFifo_7.sv \
#    ${S2RTL_PATH}/tensor_core/tpu_unit.sv \


# 3. 顶层模块名（必须和SpinalHDL生成的模块名一致，即Adder）
TOPLEVEL = ttu_core

# 4. cocotb测试用例文件名（无.py后缀）
MODULE = test_fma #test_ttu_core

# 5. Verilator额外参数（生成波形文件，可选）
EXTRA_ARGS += #--trace -g2012 -Wall
VVP_FLAGS += -v -n -vcd sim_build/ttu_core.vcd

#todo
export COCOTB_RESOLVE_X = ZEROS
# 所有Cocotb测试用例的默认超时时间（单位：秒）
# export COCOTB_TIMEOUT = 30
# 或按时钟周期设置（需Cocotb 1.8+）
export COCOTB_TIMEOUT_CYCLES = 10

# 6. 引入cocotb的通用仿真规则
include $(shell cocotb-config --makefiles)/Makefile.sim
