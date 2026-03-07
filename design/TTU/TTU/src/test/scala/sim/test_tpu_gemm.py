# test_tpu_gemm.py
import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, FallingEdge, Timer, ClockCycles
from cocotb.handle import ModifiableObject
import numpy as np
from cfg import TTConfig
from gemm_utils import generate_gemm_data, verify_gemm_result

print(f"Cocotb运行版本：{cocotb.__version__}")

# 加载TPU配置
cfg = TTConfig()
M = cfg.TPU_H    # 矩阵A的行数
K = 16 # cfg.K        # 公共维度
N = cfg.TPU_W    # 矩阵B的列数

# 全局变量：存储TPU输出结果
C_dut = np.zeros((M, N), dtype=np.int32)

async def drive_stream(dut, stream_base_name, data_list, clk, valid_delay=10):
    """
    适配Verilog独立信号的Stream驱动函数
    :param stream_base_name: Stream基础名（如"io_ifm_in_0"）
    :param data_list: 待发送的数据列表
    :param clk: 时钟信号
    :param valid_delay: 有效信号延迟（Cycle）
    """
    # 1. 获取三个独立信号（核心修复：按Verilog命名查找）
    valid = getattr(dut, f"{stream_base_name}_valid")
    ready = getattr(dut, f"{stream_base_name}_ready")
    payload = getattr(dut, f"{stream_base_name}_payload")

    # 2. 初始化信号
    valid.value = 0
    payload.value = 0
    await ClockCycles(clk, valid_delay)

    # 3. 逐数据发送（遵循valid/ready握手协议）
    for data in data_list:
        # 等待TPU侧ready拉高（表示可以接收）
        #print(f"drive wait ready")
        while not ready.value:
            await RisingEdge(clk)
        # 驱动valid和payload（单Cycle有效）
        #print(f"drive data in")
        valid.value = 1
        payload.value = int(data)
        await RisingEdge(clk)
        # 拉低valid
        valid.value = 0
    # 4. 发送完成后复位信号
    payload.value = 0
    await RisingEdge(clk)

async def monitor_stream(dut, stream_base_name, row_idx, clk, C_dut, start_cycle=30):
    """
    适配Verilog独立信号的Stream监控函数
    :param stream_base_name: Stream基础名（如"io_ofm_out_0"）
    :param row_idx: 输出矩阵的行索引
    :param start_cycle: 开始监控的Cycle
    """
    #global C_dut
    # 1. 获取独立信号
    valid = getattr(dut, f"{stream_base_name}_valid")
    ready = getattr(dut, f"{stream_base_name}_ready")
    payload = getattr(dut, f"{stream_base_name}_payload")

    # 2. 等待到指定Cycle
    ready.value = 1
    await ClockCycles(clk, start_cycle)

    #print(f"mon_{row_idx}")


    col_idx = 0
    N = 8
    while col_idx < N:
        # 等待valid拉高
        while not valid.value:
            await RisingEdge(clk)
        # 读取payload（有符号解析）
        C_dut[row_idx][col_idx] = int(payload.value.signed_integer)
        #print(f"读取OFM[{row_idx}][{col_idx}] = {C_dut[row_idx][col_idx]}")
        col_idx += 1
        await RisingEdge(clk)

# @cocotb.test()
# async def test_tpu_gemm(dut):
#     """
#     主测试用例：GEMM运算仿真
#     """
#     # 1. 初始化时钟（100MHz，周期10ns）
#     clk = dut.clk  # 假设硬件有clk信号（SpinalHDL默认ClockDomain）
#     cocotb.start_soon(Clock(clk, 10, units="ns").start())
#
#     dut.resetn.value = 0
#     await ClockCycles(clk, 2)
#     dut.resetn.value = 1
#     await ClockCycles(clk, 1)
#
#     dut.io_gemm_k.value = K
#
#
#     # 3. 生成GEMM测试数据（A=8×16，B=16×8，有符号8bit）
#     A, B, C_ref = generate_gemm_data(M, K, N, bit_width=cfg.TPU_IDWMAX, signed=True)
#     print(f"生成GEMM测试数据：A({M}×{K}), B({K}×{N})")
#     print(f"参考结果C_ref:\n{C_ref}")
#
#     # 4. 启动Stream驱动任务（并行发送IFM/KE数据）
#     # IFM_IN：按行发送矩阵A（8行，每行16个数据）
#     ifm_tasks = []
#     for i in range(cfg.TPU_H):
#         stream_if = f"io_ifm_in_{i}"  # 适配硬件接口命名（如io_ifm_in_0）
#         task = cocotb.start_soon(drive_stream(dut, stream_if, A[i], clk))
#         ifm_tasks.append(task)
#
#     # KE_IN：按列发送矩阵B（8列，每列16个数据）
#     ke_tasks = []
#     for j in range(cfg.TPU_W):
#         stream_if = f"io_ke_in_{j}"   # 适配硬件接口命名（如io_ke_in_0）
#         task = cocotb.start_soon(drive_stream(dut, stream_if, B[:, j], clk))
#         ke_tasks.append(task)
#
#     # 5. 启动OFM监控任务（并行接收8行输出）
#     ofm_tasks = []
#     for i in range(cfg.TPU_O):
#         stream_if = f"io_ofm_out_{i}" # 适配硬件接口命名（如io_ofm_out_0）
#         task = cocotb.start_soon(monitor_stream(dut, stream_if, i, clk, start_cycle=1))
#         ofm_tasks.append(task)
#
#     dut.resetn.value = 0
#     await ClockCycles(clk, 2)
#     dut.resetn.value = 1
#     await ClockCycles(clk, 1)
#
#     dut.io_flush_in.value = 1
#     await ClockCycles(clk, 1)
#     dut.io_flush_in.value = 0
#     await ClockCycles(clk, 1)
#
#     # 6. 等待所有任务完成
#     for task in ifm_tasks + ke_tasks + ofm_tasks:
#         await task
#
#     await ClockCycles(clk, 30)
#     # 7. 校验TPU输出结果
#     assert verify_gemm_result(C_dut, C_ref), "GEMM仿真校验失败！"
#
#     # 8. 仿真结束延迟
#     await ClockCycles(clk, 30)
#     print("TPU GEMM仿真完成！")
#


# 配置：要生成的测试用例数量
TEST_CASES = 120


# 封装：通用的TPU GEMM测试逻辑
def create_gemm_test(test_idx):
    # 1. 定义自定义的函数名（如test_case_0/test_case_1）
    custom_func_name = f"test_case_{test_idx}"
    custom_qualname = f"gemm_test.{custom_func_name}"
    @cocotb.test()  # 自定义测试名，便于识别
    async def test_case(dut):
        cfg = TTConfig()
        clk = dut.clk
        dut._log.info(f"test_tpu_gemm_case_{test_idx}")

        # 1. 初始化+复位
        clk.value = 0
        cocotb.start_soon(Clock(clk, 10, units="ns").start())
        await reset_tpu(dut, clk)

        # 2. 生成专属测试数据（不同用例不同seed）
        GEMM_K = test_idx + 1 #np.random.randint(8, 100) #test_idx + 1 #np.random.randint(0, 10)
        TPU_IDW = 8
        dut._log.info(f"GEMM_K：{GEMM_K}")

        dut.io_gemm_k.value = GEMM_K

        A, B, C_ref = generate_gemm_data(
            M=cfg.TPU_H, K=GEMM_K, N=cfg.TPU_W,
            bit_width=TPU_IDW, signed=True,
            seed=test_idx * 10  # 避免seed重复
        )

        # 3. 执行单次GEMM
        C_dut = await run_single_gemm(dut, cfg, A, B, clk)

        # 4. 结果校验
        assert verify_gemm_result(C_dut, C_ref), f"test_tpu_gemm_case_{test_idx} fail!"
        await ClockCycles(clk, 30)
        dut._log.info(f"test_tpu_gemm_case_{test_idx} pass！")

    test_case.__name__ = custom_func_name
    test_case.__qualname__ = custom_qualname

    return test_case

# 动态生成多个测试用例
idx = 0
for idx in range(TEST_CASES):
    # 生成测试函数
    test_func = create_gemm_test(idx)
    # 注册到全局命名空间（Cocotb会扫描globals()中的测试函数）
    globals()[f"test_tpu_gemm_case_{idx}"] = test_func


# （代码同方式1，此处省略）

# 封装：TPU复位函数（每次验证前复位）
async def reset_tpu(dut, clk):
    dut.resetn.value = 0
    await ClockCycles(clk, 2)
    dut.resetn.value = 1
    await ClockCycles(clk, 1)

    dut.io_flush_in.value = 1
    await ClockCycles(clk, 1)
    dut.io_flush_in.value = 0
    await ClockCycles(clk, 1)
    dut._log.info("TPU复位完成")

async def run_single_gemm(dut, cfg, A, B, clk):
    print("run single gemm")
    C_dut = np.zeros((cfg.TPU_H, cfg.TPU_W), dtype=np.int32)


    ifm_tasks = []
    for i in range(cfg.TPU_H):
        stream_if = f"io_ifm_in_{i}"  # 适配硬件接口命名（如io_ifm_in_0）
        task = cocotb.start_soon(drive_stream(dut, stream_if, A[i], clk))
        ifm_tasks.append(task)

    # KE_IN：按列发送矩阵B（8列，每列16个数据）
    ke_tasks = []
    for j in range(cfg.TPU_W):
        stream_if = f"io_ke_in_{j}"   # 适配硬件接口命名（如io_ke_in_0）
        task = cocotb.start_soon(drive_stream(dut, stream_if, B[:, j], clk))
        ke_tasks.append(task)

    # 5. 启动OFM监控任务（并行接收8行输出）
    ofm_tasks = []
    for i in range(cfg.TPU_O):
        stream_if = f"io_ofm_out_{i}" # 适配硬件接口命名（如io_ofm_out_0）
        task = cocotb.start_soon(monitor_stream(dut, stream_if, i, clk, C_dut, start_cycle=1))
        ofm_tasks.append(task)


    await reset_tpu(dut, clk)

    # 等待所有任务完成
    for task in ifm_tasks + ke_tasks + ofm_tasks:
        await task

    return C_dut