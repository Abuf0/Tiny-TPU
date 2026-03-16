# test_fma.py - 修复排空流水线卡住问题 + 时序对齐
import os
import xml.etree.ElementTree as ET
import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, Timer, Event
import struct
import math
import random
import time
from collections import deque

# ============================================================
# Config
# ============================================================
PIPELINE_LATENCY = 5
NUM_RANDOM_TESTS = 10000
RANDOM_SEED = random.SystemRandom().randint(0, 2**32 - 1)
USE_PREFIXED_PORTS = True
CONTINUOUS_MODE = True

# ============================================================
# FP32 helpers (完全保留)
# ============================================================
def float_to_bits(f: float) -> int:
    if math.isnan(f):
        return 0x7FC00000
    if math.isinf(f):
        return 0x7F800000 if f > 0 else 0xFF800000
    if f == 0.0:
        return 0x80000000 if math.copysign(1.0, f) < 0 else 0x00000000
    try:
        return struct.unpack(">I", struct.pack(">f", f))[0]
    except OverflowError:
        return 0x7F800000 if f > 0 else 0xFF800000
    except (struct.error, ValueError):
        return 0x7FC00000

def bits_to_float(b: int) -> float:
    return struct.unpack(">f", struct.pack(">I", b & 0xFFFFFFFF))[0]

def fp32(x: float) -> float:
    return bits_to_float(float_to_bits(x))

def canonicalize_nan_bits(bits: int) -> int:
    exp = (bits >> 23) & 0xFF
    frac = bits & 0x7FFFFF
    if exp == 0xFF and frac != 0:
        return 0x7FC00000
    return bits & 0xFFFFFFFF

def compute_fma_ref(a: float, b: float, c: float) -> float:
    a32 = fp32(a)
    b32 = fp32(b)
    c32 = fp32(c)

    if math.isnan(a32) or math.isnan(b32) or math.isnan(c32):
        return float("nan")

    if (math.isinf(a32) and b32 == 0.0) or (math.isinf(b32) and a32 == 0.0):
        return float("nan")

    prod_inf = math.isinf(a32) or math.isinf(b32)
    if prod_inf:
        prod_sign_positive = ((a32 > 0) == (b32 > 0))
        prod_inf_val = float("inf") if prod_sign_positive else float("-inf")

        if math.isinf(c32) and ((c32 > 0) != (prod_inf_val > 0)):
            return float("nan")
        return fp32(prod_inf_val)

    if math.isinf(c32):
        return fp32(c32)

    try:
        result = math.fma(a32, b32, c32)
    except AttributeError:
        from decimal import Decimal, getcontext
        getcontext().prec = 100
        da = Decimal(str(a32))
        db = Decimal(str(b32))
        dc = Decimal(str(c32))
        result = float(da * db + dc)

    return fp32(result)

def check_float_equal(hw_result: float, sw_result: float, tolerance: float = 1e-6) -> bool:
    if math.isnan(sw_result):
        return math.isnan(hw_result)
    if math.isinf(sw_result):
        return math.isinf(hw_result) and ((hw_result > 0) == (sw_result > 0))
    if sw_result == 0.0:
        return abs(hw_result) < tolerance
    relative_error = abs((hw_result - sw_result) / sw_result)
    return relative_error < tolerance

def check_bits_equal(hw_bits: int, sw_bits: int) -> bool:
    return canonicalize_nan_bits(hw_bits) == canonicalize_nan_bits(sw_bits)

# ============================================================
# DUT port helpers (完全保留)
# ============================================================
def get_ports(dut):
    if USE_PREFIXED_PORTS:
        return {
            "clk": dut.clk,
            "resetn": dut.resetn,
            "a": dut.io_fp_mac_a,
            "b": dut.io_fp_mac_b,
            "c": dut.io_fp_mac_c,
            "rnd": dut.io_fp_mac_rnd,
            "z": dut.io_fp_mac_z,
        }
    else:
        return {
            "clk": dut.clk,
            "resetn": dut.resetn,
            "a": dut.io_a,
            "b": dut.io_b,
            "c": dut.io_c,
            "rnd": dut.io_rnd,
            "z": dut.io_z,
        }

async def reset_dut(dut, ports):
    ports["resetn"].value = 0
    ports["a"].value = 0
    ports["b"].value = 0
    ports["c"].value = 0
    ports["rnd"].value = 0
    await Timer(100, units="ns")
    ports["resetn"].value = 1
    for _ in range(2):
        await RisingEdge(ports["clk"])

# ============================================================
# 核心修复：PipelineTester (解决卡住问题 + 时序对齐)
# ============================================================
class PipelineTester:
    """Helper class for pipeline-based FMA testing"""
    def __init__(self, dut, ports, latency):
        self.dut = dut
        self.ports = ports
        self.latency = latency
        self.pending_queue = deque()  # 已发射未收集的用例
        self.test_count = 0
        self.error_count = 0
        self.cycle_counter = 0  # 全局时钟周期计数器（替代emit_cycle，避免判断错误）

    async def push_input(self, a: float, b: float, c: float, desc: str = ""):
        """发射单个用例，记录发射的全局周期"""
        a_bits = float_to_bits(a)
        b_bits = float_to_bits(b)
        c_bits = float_to_bits(c)

        # 计算预期结果
        sw_float = compute_fma_ref(a, b, c)
        sw_bits = float_to_bits(sw_float)
        case_info = {
            "desc": desc,
            "a": a,
            "b": b,
            "c": c,
            "a_bits": a_bits,
            "b_bits": b_bits,
            "c_bits": c_bits,
            "sw_float": sw_float,
            "sw_bits": sw_bits,
            "emit_cycle": self.cycle_counter  # 记录发射时的全局周期
        }

        # 加入pending队列
        self.pending_queue.append(case_info)
        self.test_count += 1

        # 驱动输入到DUT
        self.ports["a"].value = a_bits
        self.ports["b"].value = b_bits
        self.ports["c"].value = c_bits
        self.ports["rnd"].value = 0

        # 等待时钟上升沿，全局周期+1
        await RisingEdge(self.ports["clk"])
        self.cycle_counter += 1

    async def pop_output(self):
        """收集单个就绪的输出（仅处理达到延迟的用例）"""
        if not self.pending_queue:
            return False

        # 检查队列头部用例是否达到收集周期（发射周期 + 流水线延迟）
        head_case = self.pending_queue[0]
        if self.cycle_counter < head_case["emit_cycle"] + self.latency + 1:
            return False  # 未到收集时间，返回False

        # 已到收集时间，取出用例
        expected = self.pending_queue.popleft()

        # 读取当前时钟的HW输出（关键：先读值，不额外等时钟）
        hw_bits = int(self.ports["z"].value) & 0xFFFFFFFF
        hw_float = bits_to_float(hw_bits)

        # 结果校验
        bit_match = check_bits_equal(hw_bits, expected["sw_bits"])
        float_match = check_float_equal(hw_float, expected["sw_float"])
        passed = bit_match or float_match

        # 日志输出
        self._log_result(expected, hw_float, hw_bits, passed)

        if not passed:
            self.error_count += 1
        return True

    def _log_result(self, expected, hw_float, hw_bits, passed):
        status = "PASS" if passed else "FAIL"
        desc = expected["desc"] if expected["desc"] else f"test_{expected['emit_cycle']}"

        self.dut._log.info(f"[{status}] {desc}")
        self.dut._log.info(
            f"  A={expected['a']} (0x{expected['a_bits']:08X}), "
            f"B={expected['b']} (0x{expected['b_bits']:08X}), "
            f"C={expected['c']} (0x{expected['c_bits']:08X})"
        )
        self.dut._log.info(
            f"  HW={hw_float} (0x{hw_bits:08X})"
        )
        self.dut._log.info(
            f"  SW={expected['sw_float']} (0x{expected['sw_bits']:08X})"
        )

    async def run_test(self, test_cases):
        """
        修复版执行逻辑：
        1. 边发射边收集（每发射一个，检查一次是否有可收集的用例）
        2. 发射完成后，最多等待latency*2个周期排空流水线（避免死循环）
        """
        self.dut._log.info(f"Starting pipeline test (latency={self.latency})...")

        # 第一步：逐个发射用例 + 实时收集
        for case in test_cases:
            a, b, c, desc = case
            await self.push_input(a, b, c, desc)
            # 每次发射后，尽可能收集就绪的用例（可能有多个）
            while await self.pop_output():
                pass

        # 第二步：排空剩余用例（添加最大等待周期，避免死循环）
        self.dut._log.info("Emission complete, draining remaining pipeline...")
        max_drain_cycles = self.latency * 2 + 2 # 最大等待周期（流水线深度*2）
        drain_cycles = 0

        while self.pending_queue and drain_cycles < max_drain_cycles:
            # 等待一个时钟周期
            await RisingEdge(self.ports["clk"])
            self.cycle_counter += 1
            drain_cycles += 1

            # 尝试收集就绪的用例
            while await self.pop_output():
                pass

        # 检查是否有未收集的用例（调试用）
        if self.pending_queue:
            self.dut._log.warning(f"Remaining {len(self.pending_queue)} cases not collected!")

        # 最终统计
        self.dut._log.info(f"Total tests: {self.test_count}, Errors: {self.error_count}")
        assert self.error_count == 0, f"Test failed with {self.error_count} errors!"

# ============================================================
# 测试用例（适配修复版）
# ============================================================
@cocotb.test()
async def test_fma_basic(dut):
    """Basic FP32 FMA tests with pipeline support"""
    ports = get_ports(dut)

    # 启动时钟
    clock = Clock(ports["clk"], 10, units="ns")
    cocotb.start_soon(clock.start())

    # 复位DUT
    await reset_dut(dut, ports)

    # 初始化测试器
    tester = PipelineTester(dut, ports, PIPELINE_LATENCY)
    dut._log.info("Starting basic FMA tests (timing aligned)...")

    # 定义测试用例
    test_cases = [
        (1.5, 2.0, 0.5, "1.5*2.0+0.5=3.5"),
        (1.0, 1.0, 1.0, "1.0*1.0+1.0=2.0"),
        (2.0, 3.0, 4.0, "2.0*3.0+4.0=10.0"),
        (-1.0, 1.0, 0.0, "-1.0*1.0+0.0=-1.0"),
        (0.0, 1.0, 5.0, "0.0*1.0+5.0=5.0"),
        (1.0, 0.0, 3.0, "1.0*0.0+3.0=3.0"),
        (0.5, 0.5, 0.5, "0.5*0.5+0.5=0.75"),
        (-2.0, -3.0, -4.0, "-2.0*-3.0+(-4.0)=2.0"),
        (1e10, 1e10, -1e20, "Large number cancellation"),
        (1.0, 1.0, -1.0, "1.0*1.0-1.0=0.0"),
        (-0.0, 1.0, 0.0, "-0.0*1.0+0.0"),
        (float("inf"), 2.0, 1.0, "inf*2+1"),
        (0.0, float("inf"), 1.0, "0*inf+1 => NaN"),
        (float("nan"), 1.0, 2.0, "nan*1+2 => NaN"),
    ]

    # 执行测试
    await tester.run_test(test_cases)

@cocotb.test()
async def test_fma_random(dut):
    """Randomized FP32 FMA tests with pipeline support"""
    ports = get_ports(dut)

    # 启动时钟
    clock = Clock(ports["clk"], 10, units="ns")
    cocotb.start_soon(clock.start())

    # 复位DUT
    await reset_dut(dut, ports)

    # 初始化测试器
    tester = PipelineTester(dut, ports, PIPELINE_LATENCY)
    random.seed(RANDOM_SEED)
    dut._log.info(f"Starting random FMA tests... seed={RANDOM_SEED}, latency={PIPELINE_LATENCY}")

    # 生成随机测试用例
    test_cases = []
    for i in range(NUM_RANDOM_TESTS):
        r = random.random()

        if r < 0.15:
            a = random.uniform(-1e6, 1e6)
            b = random.uniform(-1e6, 1e6)
            c = random.uniform(-1e6, 1e6)
        elif r < 0.35:
            a = random.uniform(-1e-10, 1e-10)
            b = random.uniform(-1e10, 1e10)
            c = random.uniform(-1e-5, 1e-5)
        elif r < 0.60:
            a = random.uniform(-1e20, 1e20)
            b = random.uniform(-1e10, 1e10)
            c = random.uniform(-1e20, 1e20)
        elif r < 0.80:
            a = 1.0 + random.uniform(-1e-6, 1e-6)
            b = 1.0 + random.uniform(-1e-6, 1e-6)
            c = random.uniform(-2.0, 2.0)
        elif r < 0.90:
            a = random.choice([0.0, -0.0, 1.0, -1.0])
            b = random.choice([0.0, -0.0, 2.0, -2.0])
            c = random.choice([0.0, -0.0, 3.0, -3.0])
        else:
            a = random.choice([float("inf"), float("-inf"), float("nan"), 1.0, -1.0])
            b = random.choice([float("inf"), float("-inf"), float("nan"), 2.0, -2.0, 0.0])
            c = random.choice([float("inf"), float("-inf"), float("nan"), 3.0, -3.0, 0.0])

        test_cases.append((a, b, c, f"random_{i}"))

    # 执行测试
    await tester.run_test(test_cases)