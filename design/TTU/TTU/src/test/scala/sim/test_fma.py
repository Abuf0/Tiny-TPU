# test_fma.py - cocotb testbench for FP32 FMA

import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, FallingEdge, Timer
from cocotb.binary import BinaryValue
import struct
import math
import random
from cocotb.result import TestFailure

def float_to_bits(f):
    """Convert Python float to 32-bit unsigned int representation"""
    return struct.unpack('>I', struct.pack('>f', f))[0]

def bits_to_float(b):
    """Convert 32-bit unsigned int to Python float"""
    return struct.unpack('>f', struct.pack('>I', b & 0xFFFFFFFF))[0]

def compute_fma_ref(a, b, c):
    """
    Reference model: IEEE 754-2008 compliant FMA
    A*B + C with single rounding (using Python's fma if available)
    """
    try:
        # Python 3.6+ has math.fma for exact FMA
        result = math.fma(a, b, c)
    except AttributeError:
        # Fallback: use decimal for high precision
        from decimal import Decimal, getcontext
        getcontext().prec = 50
        result = float(Decimal(a) * Decimal(b) + Decimal(c))

    # Handle IEEE 754 special cases
    if math.isnan(result):
        return float('nan')
    if math.isinf(result):
        return result
    return result

def check_float_equal(hw_result, sw_result, tolerance=1e-6):
    """
    Compare hardware and software float results
    Handles NaN, Inf, and normal numbers
    """
    # Both NaN?
    if math.isnan(hw_result) and math.isnan(sw_result):
        return True

    # Both Inf with same sign?
    if math.isinf(hw_result) and math.isinf(sw_result):
        return (hw_result > 0) == (sw_result > 0)

    # One Inf, other not?
    if math.isinf(hw_result) or math.isinf(sw_result):
        return False

    # Both zero? (check sign for -0.0 vs +0.0)
    if hw_result == 0.0 and sw_result == 0.0:
        return True  # Allow -0.0 == +0.0 for now, or check sign explicitly

    # Normal comparison with ULP tolerance
    if sw_result == 0:
        return abs(hw_result) < tolerance
    relative_error = abs((hw_result - sw_result) / sw_result)
    return relative_error < tolerance

@cocotb.test()
async def test_fma_basic(dut):
    """Test basic FMA operations"""

    # Create clock (100MHz)
    clock = Clock(dut.clk, 10, units="ns")
    cocotb.start_soon(clock.start())

    # Reset
    dut.resetn <= 0
    await Timer(100, units="ns")
    dut.resetn <= 1
    await RisingEdge(dut.clk)

    dut._log.info("Starting basic FMA tests...")

    # Test vectors: (a, b, c, description)
    test_cases = [
        (1.5, 2.0, 0.5, "1.5*2.0+0.5=3.5"),
        (1.0, 1.0, 1.0, "1.0*1.0+1.0=2.0"),
        (2.0, 3.0, 4.0, "2.0*3.0+4.0=10.0"),
        (-1.0, 1.0, 0.0, "-1.0*1.0+0.0=-1.0"),
        (0.0, 1.0, 5.0, "0.0*1.0+5.0=5.0"),
        (1.0, 0.0, 3.0, "1.0*0.0+3.0=3.0"),
        (0.5, 0.5, 0.5, "0.5*0.5+0.5=0.75"),
        (-2.0, -3.0, -4.0, "-2.0*-3.0-4.0=2.0"),
        (1e10, 1e10, -1e20, "Large number cancellation"),
        (1.0, 1.0, -1.0, "1.0*1.0-1.0=0.0"),
    ]

    errors = 0

    for a, b, c, desc in test_cases:
        # Drive inputs
        dut.io_fp_mac_a <= float_to_bits(a)
        dut.io_fp_mac_b <= float_to_bits(b)
        dut.io_fp_mac_c <= float_to_bits(c)

        await RisingEdge(dut.clk)
        await RisingEdge(dut.clk)  # Wait for pipeline latency

        # Read output (adjust based on your pipeline depth)
        await RisingEdge(dut.clk)

        hw_result_bits = int(dut.io_fp_mac_z.value)
        hw_result = bits_to_float(hw_result_bits)
        sw_result = compute_fma_ref(a, b, c)

        match = check_float_equal(hw_result, sw_result)

        status = "PASS" if match else "FAIL"
        dut._log.info(f"[{status}] {desc}")
        dut._log.info(f"  A={a}, B={b}, C={c}")
        dut._log.info(f"  HW={hw_result} (0x{hw_result_bits:08X})")
        dut._log.info(f"  SW={sw_result} (0x{float_to_bits(sw_result):08X})")

        if not match:
            errors += 1
            dut._log.error(f"Mismatch! Diff={abs(hw_result-sw_result)}")

    assert errors == 0, f"Test failed with {errors} errors"

@cocotb.test()
async def test_fma_special_values(dut):
    """Test IEEE 754 special values"""

    clock = Clock(dut.clk, 10, units="ns")
    cocotb.start_soon(clock.start())

    dut.resetn <= 0
    await Timer(100, units="ns")
    dut.resetn <= 1
    await RisingEdge(dut.clk)

    dut._log.info("Testing special values...")

    inf = float('inf')
    neg_inf = float('-inf')
    nan = float('nan')

    special_cases = [
        (1.0, inf, 0.0, inf, "1.0 * Inf + 0.0 = Inf"),
        (inf, 0.0, 1.0, nan, "Inf * 0.0 + 1.0 = NaN (invalid)"),
        (inf, inf, neg_inf, inf, "Inf * Inf - Inf = NaN"),
        (1.0, 1.0, inf, inf, "1.0 * 1.0 + Inf = Inf"),
        (0.0, 0.0, nan, nan, "0.0 * 0.0 + NaN = NaN"),
        (-0.0, 1.0, 0.0, 0.0, "-0.0 * 1.0 + 0.0 = 0.0"),
    ]

    for a, b, c, expected, desc in special_cases:
        dut.io_fp_mac_a <= float_to_bits(a)
        dut.io_fp_mac_b <= float_to_bits(b)
        dut.io_fp_mac_c <= float_to_bits(c)

        await RisingEdge(dut.clk)
        await RisingEdge(dut.clk)
        await RisingEdge(dut.clk)

        hw_result = bits_to_float(int(dut.io_fp_mac_z.value))

        # Check special value properties
        if math.isnan(expected):
            if not math.isnan(hw_result):
                dut._log.error(f"[FAIL] {desc}: expected NaN, got {hw_result}")
        elif math.isinf(expected):
            if not math.isinf(hw_result) or (hw_result > 0) != (expected > 0):
                dut._log.error(f"[FAIL] {desc}: expected {expected}, got {hw_result}")
        else:
            if hw_result != expected:
                dut._log.error(f"[FAIL] {desc}: expected {expected}, got {hw_result}")

        dut._log.info(f"[CHECK] {desc}: HW={hw_result}")

@cocotb.test()
async def test_fma_random(dut):
    """Randomized testing with coverage"""

    clock = Clock(dut.clk, 10, units="ns")
    cocotb.start_soon(clock.start())

    dut.resetn <= 0
    await Timer(100, units="ns")
    dut.resetn <= 1
    await RisingEdge(dut.clk)

    dut._log.info("Starting random FMA tests...")

    NUM_TESTS = 1000
    errors = 0

    for i in range(NUM_TESTS):
        # Generate random floats across ranges
        r = random.random()

        if r < 0.3:
            # Normal range
            a = random.uniform(-1e6, 1e6)
            b = random.uniform(-1e6, 1e6)
            c = random.uniform(-1e6, 1e6)
        elif r < 0.6:
            # Small numbers
            a = random.uniform(-1e-10, 1e-10)
            b = random.uniform(-1e10, 1e10)
            c = random.uniform(-1e-5, 1e-5)
        elif r < 0.9:
            # Large numbers
            a = random.uniform(-1e30, 1e30)
            b = random.uniform(-1e10, 1e10)
            c = random.uniform(-1e35, 1e35)
        else:
            # Edge cases near 1.0
            a = 1.0 + random.uniform(-1e-6, 1e-6)
            b = 1.0 + random.uniform(-1e-6, 1e-6)
            c = random.uniform(-2.0, 2.0)

        dut.io_fp_mac_a <= float_to_bits(a)
        dut.io_fp_mac_b <= float_to_bits(b)
        dut.io_fp_mac_c <= float_to_bits(c)

        await RisingEdge(dut.clk)
        await RisingEdge(dut.clk)
        await RisingEdge(dut.clk)

        hw_result = bits_to_float(int(dut.io_fp_mac_z.value))
        sw_result = compute_fma_ref(a, b, c)

        if not check_float_equal(hw_result, sw_result, tolerance=1e-5):
            errors += 1
            if errors <= 10:  # Log first 10 errors
                dut._log.error(f"Random test {i} failed:")
                dut._log.error(f"  A={a}, B={b}, C={c}")
                dut._log.error(f"  HW={hw_result}, SW={sw_result}")

    dut._log.info(f"Random test completed: {NUM_TESTS} tests, {errors} errors")
    assert errors == 0, f"Random test failed with {errors} errors"

@cocotb.test()
async def test_fma_precision(dut):
    """Test FMA precision advantage over separate multiply-add"""

    clock = Clock(dut.clk, 10, units="ns")
    cocotb.start_soon(clock.start())

    dut.resetn <= 0
    await Timer(100, units="ns")
    dut.resetn <= 1
    await RisingEdge(dut.clk)

    dut._log.info("Testing FMA precision...")

    # Classic example where FMA matters: (1 + 2^-24) - 1
    # Separate: 1 + 2^-24 rounds to 1, then 1 - 1 = 0
    # FMA: computes exactly, keeps 2^-24

    a = 1.0 + 2**-24  # Slightly more than 1
    b = 1.0
    c = -1.0

    dut.io_fp_mac_a <= float_to_bits(a)
    dut.io_fp_mac_b <= float_to_bits(b)
    dut.io_fp_mac_c <= float_to_bits(c)

    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)

    hw_result = bits_to_float(int(dut.io_fp_mac_z.value))

    # Expected: 2^-24 (about 5.96e-8)
    expected = 2**-24
    separate_mul_add = (a * b) + c  # Will be 0 due to rounding

    dut._log.info(f"Precision test: (1+2^-24)*1 + (-1)")
    dut._log.info(f"  FMA result: {hw_result} (expected ~{expected})")
    dut._log.info(f"  Separate MUL+ADD would give: {separate_mul_add}")

    # Check if FMA preserved the small value
    if abs(hw_result - expected) < expected * 0.1:
        dut._log.info("  [PASS] FMA precision preserved")
    else:
        dut._log.error("  [FAIL] FMA precision lost")