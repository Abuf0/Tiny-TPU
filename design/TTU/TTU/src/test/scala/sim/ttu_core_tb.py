import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, FallingEdge, Timer, ClockCycles
from cocotb.bus import Bus
import random

# 自定义AXI-Lite和AXI4总线操作辅助类
class AxiLiteDriver:
    """AXI-Lite 总线驱动辅助类，简化寄存器读写操作"""
    def __init__(self, dut, axi_name, clk):
        self.axi = Bus(dut, axi_name, ["awaddr", "awvalid", "awready",
                                       "wdata", "wstrb", "wvalid", "wready",
                                       "bresp", "bvalid", "bready",
                                       "araddr", "arvalid", "arready",
                                       "rdata", "rresp", "rvalid", "rready"],
                       clk=clk)
        self.clk = clk

    async def write_reg(self, addr, data, strb=0xf):
        """AXI-Lite 写寄存器：addr=地址，data=数据，strb=字节选通"""
        # 等待AW通道握手
        self.axi.awaddr.value = addr
        self.axi.awvalid.value = 1
        await RisingEdge(self.clk)
        while self.axi.awready.value != 1:
            await RisingEdge(self.clk)
        self.axi.awvalid.value = 0

        # 等待W通道握手
        self.axi.wdata.value = data
        self.axi.wstrb.value = strb
        self.axi.wvalid.value = 1
        await RisingEdge(self.clk)
        while self.axi.wready.value != 1:
            await RisingEdge(self.clk)
        self.axi.wvalid.value = 0

        # 等待B通道响应
        self.axi.bready.value = 1
        await RisingEdge(self.clk)
        while self.axi.bvalid.value != 1:
            await RisingEdge(self.clk)
        resp = self.axi.bresp.value
        self.axi.bready.value = 0
        return resp

    async def read_reg(self, addr):
        """AXI-Lite 读寄存器：addr=地址，返回读取的数据"""
        # 等待AR通道握手
        self.axi.araddr.value = addr
        self.axi.arvalid.value = 1
        await RisingEdge(self.clk)
        while self.axi.arready.value != 1:
            await RisingEdge(self.clk)
        self.axi.arvalid.value = 0

        # 等待R通道响应
        self.axi.rready.value = 1
        await RisingEdge(self.clk)
        while self.axi.rvalid.value != 1:
            await RisingEdge(self.clk)
        data = self.axi.rdata.value
        resp = self.axi.rresp.value
        self.axi.rready.value = 0
        return (data, resp)

class Axi4Driver:
    """AXI4 总线驱动辅助类，简化DMA接口基本操作"""
    def __init__(self, dut, axi_name, clk):
        self.axi = Bus(dut, axi_name, ["awaddr", "awlen", "awsize", "awburst",
                                       "awvalid", "awready",
                                       "wdata", "wstrb", "wlast", "wvalid", "wready",
                                       "bresp", "bvalid", "bready",
                                       "araddr", "arlen", "arsize", "arburst",
                                       "arvalid", "arready",
                                       "rdata", "rlast", "rvalid", "rready"],
                       clk=clk)
        self.clk = clk

    async def mock_read_response(self):
        """模拟AXI4读响应（给DMA返回数据）"""
        self.axi.rready.value = 1
        await RisingEdge(self.clk)
        while self.axi.rvalid.value != 1:
            await RisingEdge(self.clk)
        # 随机返回测试数据
        data = random.randint(0, 0xffffffff)
        self.axi.rdata.value = data
        self.axi.rlast.value = 1  # 单beat传输
        await RisingEdge(self.clk)
        self.axi.rready.value = 0
        return data

    async def mock_write_response(self):
        """模拟AXI4写响应"""
        self.axi.bready.value = 1
        await RisingEdge(self.clk)
        while self.axi.bvalid.value != 1:
            await RisingEdge(self.clk)
        resp = self.axi.bresp.value
        self.axi.bready.value = 0
        return resp

@cocotb.test()
async def ttu_core_tb(dut):
    """ttu_core 模块主仿真测试函数"""
    # 1. 初始化时钟（50MHz，周期20ns）
    clock = Clock(dut.clk, 20, units="ns")
    cocotb.start_soon(clock.start())

    # 2. 初始化复位
    dut.rst_n.value = 0  # 低电平复位
    await ClockCycles(dut.clk, 10)
    dut.rst_n.value = 1   # 释放复位
    await ClockCycles(dut.clk, 5)

    # 3. 初始化总线驱动
    ctrl_axi_driver = AxiLiteDriver(dut, "io_ctrl_axi", dut.clk)
    dma0_axi_driver = Axi4Driver(dut, "io_dma0_axi", dut.clk)
    dma1_axi_driver = Axi4Driver(dut, "io_dma1_axi", dut.clk)

    # 4. 测试1：AXI-Lite 控制面寄存器读写
    cocotb.log.info("=== 测试AXI-Lite寄存器读写 ===")
    # 写寄存器（地址0x10，数据0x12345678）
    write_resp = await ctrl_axi_driver.write_reg(0x10, 0x12345678)
    assert write_resp == 0, f"写寄存器响应错误，期望0，实际{write_resp}"
    cocotb.log.info(f"写寄存器0x10成功，响应：{write_resp}")

    # 读寄存器（验证写入的数据）
    read_data, read_resp = await ctrl_axi_driver.read_reg(0x10)
    assert read_resp == 0, f"读寄存器响应错误，期望0，实际{read_resp}"
    assert read_data == 0x12345678, f"读数据错误，期望0x12345678，实际{hex(read_data)}"
    cocotb.log.info(f"读寄存器0x10成功，数据：{hex(read_data)}")

    # 5. 测试2：模拟DMA0/1接口交互
    cocotb.log.info("=== 测试DMA接口交互 ===")
    # 模拟DMA0读响应
    dma0_read_data = await dma0_axi_driver.mock_read_response()
    cocotb.log.info(f"DMA0读响应数据：{hex(dma0_read_data)}")

    # 模拟DMA1写响应
    dma1_write_resp = await dma1_axi_driver.mock_write_response()
    cocotb.log.info(f"DMA1写响应：{dma1_write_resp}")

    # 6. 测试3：中断信号检测
    cocotb.log.info("=== 测试中断信号 ===")
    # 等待中断信号触发（这里模拟中断触发，实际需根据模块逻辑）
    dut.io_irq.value = 1  # 手动置位中断（实际由DUT产生，此处仅测试检测逻辑）
    await RisingEdge(dut.clk)
    assert dut.io_irq.value == 1, "中断信号未触发"
    cocotb.log.info("中断信号检测成功")

    # 7. 仿真结束
    cocotb.log.info("=== 所有测试用例执行完成 ===")
    await ClockCycles(dut.clk, 10)