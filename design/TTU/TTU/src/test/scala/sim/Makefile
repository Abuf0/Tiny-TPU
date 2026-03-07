# Makefile
# 1. 仿真器选择（Verilator） iverilog
SIM ?= icarus
WAVES = 1
# 2. SpinalHDL生成的Verilog文件路径
VERILOG_SOURCES = \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/enumdefine.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/StreamFifo_33.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/StreamFifo_31.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/StreamFifo_23.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/StreamFifo_14.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/StreamFifo_13.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/StreamFifo_12.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/StreamFifo_11.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/StreamFifo_10.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/StreamFifo_9.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/StreamFifo_8.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/StreamFifo_7.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/StreamFifo_6.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/StreamFifo_5.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/StreamFifo_4.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/StreamFifo_3.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/StreamFifo_2.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/StreamFifo_1.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/StreamFifo.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/DmaEngineAxiToL1.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/fetch.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/decode.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/scoreboard.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/dma_wrap.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/agu_dma.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/agu2dma_ctrl.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/agu_compute.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/agu_compute_ifm.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/agu_compute_ker.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/load_engine_1.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/load_engine.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/loop_controller.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/store_engine.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/tpu_unit.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/tensor_core.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/alu_int.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/alu_fp.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/L1_Fabric.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/sram_1R1W_wrap.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/sram_1R1W_wrap_1.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/sram_1R1W_wrap_2.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/sram_1R1W_wrap_3.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/sram_1R1W_wrap_4.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/sram_1R1W_wrap_5.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/sram_1R1W_wrap_6.sv \
D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/ttu_core/ttu_core.sv \

#	D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/tensor_core/StreamFifo.sv \
#    D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/tensor_core/StreamFifo_1.sv \
#    D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/tensor_core/StreamFifo_2.sv \
#    D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/tensor_core/StreamFifo_3.sv \
#    D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/tensor_core/StreamFifo_4.sv \
#    D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/tensor_core/StreamFifo_5.sv \
#    D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/tensor_core/StreamFifo_6.sv \
#    D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/tensor_core/StreamFifo_7.sv \
#    D:/Learn/IC/project/NPU/design/TTU/TTU/build/s2rtl/tensor_core/tpu_unit.sv \


# 3. 顶层模块名（必须和SpinalHDL生成的模块名一致，即Adder）
TOPLEVEL = ttu_core

# 4. cocotb测试用例文件名（无.py后缀）
MODULE = test_ttu_core

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
