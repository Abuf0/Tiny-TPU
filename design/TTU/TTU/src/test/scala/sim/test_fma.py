# test_fma.py - cocotb testbench for FP32 FMA

import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, Timer
import struct
import math
import random


# ============================================================
# Config
# ============================================================
PIPELINE_LATENCY = 3
NUM_RANDOM_TESTS = 10000
RANDOM_SEED = 123456

# 如果你的顶层端口名不是 io_fp_mac_a / b / c / z，
# 可以改成 False，然后在 get_ports() 里切到 io.a / io.b / io.c / io.z
USE_PREFIXED_PORTS = True


# ============================================================
# FP32 helpers
# ============================================================
def float_to_bits(f: float) -> int:
    """Convert Python float to IEEE754 FP32 bit pattern."""
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
    """Convert IEEE754 FP32 bit pattern to Python float."""
    return struct.unpack(">f", struct.pack(">I", b & 0xFFFFFFFF))[0]


def fp32(x: float) -> float:
    """Round a Python float to FP32 and back."""
    return bits_to_float(float_to_bits(x))


def canonicalize_nan_bits(bits: int) -> int:
    """Map any NaN payload to canonical qNaN for relaxed NaN comparison."""
    exp = (bits >> 23) & 0xFF
    frac = bits & 0x7FFFFF
    if exp == 0xFF and frac != 0:
        return 0x7FC00000
    return bits & 0xFFFFFFFF


def compute_fma_ref(a: float, b: float, c: float) -> float:
    """
    FP32 reference model:
      1) quantize inputs to FP32
      2) handle IEEE special cases first
      3) compute fused multiply-add in high precision if possible
      4) round result back to FP32
    """
    a32 = fp32(a)
    b32 = fp32(b)
    c32 = fp32(c)

    # ------------------------------------------------------------
    # IEEE special-case handling first
    # ------------------------------------------------------------
    # NaN propagation
    if math.isnan(a32) or math.isnan(b32) or math.isnan(c32):
        return float("nan")

    # invalid: inf * 0 + c  or  0 * inf + c
    if (math.isinf(a32) and b32 == 0.0) or (math.isinf(b32) and a32 == 0.0):
        return float("nan")

    # product is inf
    prod_inf = math.isinf(a32) or math.isinf(b32)
    if prod_inf:
        prod_sign_positive = ((a32 > 0) == (b32 > 0))
        prod_inf_val = float("inf") if prod_sign_positive else float("-inf")

        # inf + (-inf) => NaN
        if math.isinf(c32) and ((c32 > 0) != (prod_inf_val > 0)):
            return float("nan")

        # inf + finite / same-sign inf => inf
        return fp32(prod_inf_val)

    # finite product + inf => inf
    if math.isinf(c32):
        return fp32(c32)

    # ------------------------------------------------------------
    # Normal finite path
    # ------------------------------------------------------------
    try:
        # If available, true fused op in Python runtime
        result = math.fma(a32, b32, c32)
    except AttributeError:
        # No math.fma in this Python: use high precision decimal for finite numbers only
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
    """
    Prefer bit-exact compare.
    Relaxation:
      - all NaNs are treated as equivalent
    """
    return canonicalize_nan_bits(hw_bits) == canonicalize_nan_bits(sw_bits)


# ============================================================
# DUT port helpers
# ============================================================
def get_ports(dut):
    if USE_PREFIXED_PORTS:
        return {
            "clk": dut.clk,
            "resetn": dut.resetn,
            "a": dut.io_fp_mac_a,
            "b": dut.io_fp_mac_b,
            "c": dut.io_fp_mac_c,
            "z": dut.io_fp_mac_z,
        }
    else:
        return {
            "clk": dut.clk,
            "resetn": dut.resetn,
            "a": dut.io_a,
            "b": dut.io_b,
            "c": dut.io_c,
            "z": dut.io_z,
        }


async def reset_dut(dut, ports):
    ports["resetn"].value = 0
    ports["a"].value = 0
    ports["b"].value = 0
    ports["c"].value = 0
    await Timer(100, units="ns")
    ports["resetn"].value = 1
    await RisingEdge(ports["clk"])


async def drive_and_sample(dut, ports, a: float, b: float, c: float, latency: int = PIPELINE_LATENCY):
    a_bits = float_to_bits(a)
    b_bits = float_to_bits(b)
    c_bits = float_to_bits(c)

    ports["a"].value = a_bits
    ports["b"].value = b_bits
    ports["c"].value = c_bits

    # 输入打一拍
    await RisingEdge(ports["clk"])

    # 等待流水线
    for _ in range(latency):
        await RisingEdge(ports["clk"])

    hw_bits = int(ports["z"].value) & 0xFFFFFFFF
    hw_float = bits_to_float(hw_bits)

    sw_float = compute_fma_ref(a, b, c)
    sw_bits = float_to_bits(sw_float)

    return {
        "a_bits": a_bits,
        "b_bits": b_bits,
        "c_bits": c_bits,
        "hw_bits": hw_bits,
        "hw_float": hw_float,
        "sw_bits": sw_bits,
        "sw_float": sw_float,
    }


def log_case(dut, desc, a, b, c, result, passed):
    status = "PASS" if passed else "FAIL"
    dut._log.info(f"[{status}] {desc}")
    dut._log.info(
        f"  A={a} (0x{result['a_bits']:08X}), "
        f"B={b} (0x{result['b_bits']:08X}), "
        f"C={c} (0x{result['c_bits']:08X})"
    )
    dut._log.info(
        f"  HW={result['hw_float']} (0x{result['hw_bits']:08X})"
    )
    dut._log.info(
        f"  SW={result['sw_float']} (0x{result['sw_bits']:08X})"
    )


# ============================================================
# Tests
# ============================================================
@cocotb.test()
async def test_fma_basic(dut):
    """Basic FP32 FMA tests."""

    ports = get_ports(dut)

    clock = Clock(ports["clk"], 10, units="ns")
    cocotb.start_soon(clock.start())

    await reset_dut(dut, ports)

    dut._log.info("Starting basic FMA tests...")

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

    errors = 0

    for a, b, c, desc in test_cases:
        result = await drive_and_sample(dut, ports, a, b, c)

        bit_match = check_bits_equal(result["hw_bits"], result["sw_bits"])
        float_match = check_float_equal(result["hw_float"], result["sw_float"])
        passed = bit_match or float_match

        log_case(dut, desc, a, b, c, result, passed)

        if not passed:
            errors += 1
            dut._log.error(
                f"Mismatch! abs diff = {abs(result['hw_float'] - result['sw_float']) if not (math.isnan(result['hw_float']) or math.isnan(result['sw_float'])) else 'NaN'}"
            )

    assert errors == 0, f"Basic test failed with {errors} errors"


@cocotb.test()
async def test_fma_random(dut):
    """Randomized FP32 FMA tests."""

    ports = get_ports(dut)

    clock = Clock(ports["clk"], 10, units="ns")
    cocotb.start_soon(clock.start())

    await reset_dut(dut, ports)

    random.seed(RANDOM_SEED)
    dut._log.info(f"Starting random FMA tests... seed={RANDOM_SEED}")

    errors = 0

    for i in range(NUM_RANDOM_TESTS):
        r = random.random()

        if r < 0.15:
            # normal range
            a = random.uniform(-1e6, 1e6)
            b = random.uniform(-1e6, 1e6)
            c = random.uniform(-1e6, 1e6)
        elif r < 0.35:
            # small values
            a = random.uniform(-1e-10, 1e-10)
            b = random.uniform(-1e10, 1e10)
            c = random.uniform(-1e-5, 1e-5)
        elif r < 0.60:
            # large values
            a = random.uniform(-1e20, 1e20)
            b = random.uniform(-1e10, 1e10)
            c = random.uniform(-1e20, 1e20)
        elif r < 0.80:
            # around 1.0
            a = 1.0 + random.uniform(-1e-6, 1e-6)
            b = 1.0 + random.uniform(-1e-6, 1e-6)
            c = random.uniform(-2.0, 2.0)
        elif r < 0.90:
            # zeros / signed zeros
            a = random.choice([0.0, -0.0, 1.0, -1.0])
            b = random.choice([0.0, -0.0, 2.0, -2.0])
            c = random.choice([0.0, -0.0, 3.0, -3.0])
        else:
            # special values
            a = random.choice([float("inf"), float("-inf"), float("nan"), 1.0, -1.0])
            b = random.choice([float("inf"), float("-inf"), float("nan"), 2.0, -2.0, 0.0])
            c = random.choice([float("inf"), float("-inf"), float("nan"), 3.0, -3.0, 0.0])

        result = await drive_and_sample(dut, ports, a, b, c)

        bit_match = check_bits_equal(result["hw_bits"], result["sw_bits"])
        float_match = check_float_equal(result["hw_float"], result["sw_float"], tolerance=1e-5)
        passed = bit_match or float_match

        if not passed:
            errors += 1
            if errors <= 10:
                dut._log.error(f"Random test {i} failed:")
                log_case(dut, f"random_{i}", a, b, c, result, passed)

    dut._log.info(f"Random test completed: {NUM_RANDOM_TESTS} tests, {errors} errors")
    assert errors == 0, f"Random test failed with {errors} errors"