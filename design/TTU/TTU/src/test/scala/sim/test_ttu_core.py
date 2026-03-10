import cocotb
from cocotb.clock import Clock
from cocotb.triggers import Timer, RisingEdge, ClockCycles
import random
import asyncio
import numpy as np

# -------------------------
# 通用调试工具函数
# -------------------------
def print_signal_status(sig, name: str):
    """打印信号的详细状态（方向、值、是否驱动）"""
    try:
        dir_str = "INPUT" if hasattr(sig, '_direction') and sig._direction == "input" else "OUTPUT" if hasattr(sig, '_direction') and sig._direction == "output" else "UNKNOWN"
        val_str = f"0x{sig.value:08x}" if hasattr(sig, '_width') and sig._width > 1 else f"{sig.value}"
        cocotb.log.info(f"[信号状态] {name:30} | 方向：{dir_str:6} | 值：{val_str}")
    except Exception as e:
        cocotb.log.error(f"[信号状态] {name} 读取失败：{str(e)}")

def conv2d_ref(
        ifm,
        ker,
        H,
        W,
        C,
        Kh,
        Kw,
        Oc,
        stride_h=1,
        stride_w=1,
        pad_h=0,
        pad_w=0,
):
    """
    Reference Conv2D (HWC x KHWC -> HWC)

    IFM layout : H, W, C
    KER layout : Kh, Kw, C, Oc

    return: OH, OW, Oc
    """

    ifm = np.array(ifm).reshape(H, W, C)
    ker = np.array(ker).reshape(Kh, Kw, C, Oc)

    # Output size
    OH = (H + 2 * pad_h - Kh) // stride_h + 1
    OW = (W + 2 * pad_w - Kw) // stride_w + 1

    # Padding
    if pad_h > 0 or pad_w > 0:
        ifm_pad = np.zeros((H + 2 * pad_h, W + 2 * pad_w, C), dtype=ifm.dtype)
        ifm_pad[pad_h:pad_h+H, pad_w:pad_w+W, :] = ifm
    else:
        ifm_pad = ifm

    out = np.zeros((OH, OW, Oc), dtype=np.int64)

    for oh in range(OH):
        for ow in range(OW):

            ih_base = oh * stride_h
            iw_base = ow * stride_w

            for oc in range(Oc):

                acc = 0

                for kh in range(Kh):
                    for kw in range(Kw):
                        for c in range(C):

                            a = ifm_pad[ih_base + kh, iw_base + kw, c]
                            b = ker[kh, kw, c, oc]

                            acc += int(a) * int(b)

                out[oh, ow, oc] = acc

    return out

# -------------------------
# AXI-Lite 驱动类（终极修复：强制B通道空闲）
# -------------------------
class AxiLiteDriver:
    """适配 ttu_core 的 AXI-Lite 驱动（强制B通道空闲，确保AW响应）"""
    def __init__(self, dut, axi_prefix, clk):
        self.dut = dut
        self.clk = clk
        self.prefix = axi_prefix
        # 1. 绑定所有信号
        self.aw_valid    = getattr(dut, f"{axi_prefix}_aw_valid")
        self.aw_ready    = getattr(dut, f"{axi_prefix}_aw_ready")
        self.aw_addr     = getattr(dut, f"{axi_prefix}_aw_payload_addr")
        self.aw_prot     = getattr(dut, f"{axi_prefix}_aw_payload_prot")

        self.w_valid     = getattr(dut, f"{axi_prefix}_w_valid")
        self.w_ready     = getattr(dut, f"{axi_prefix}_w_ready")
        self.w_data      = getattr(dut, f"{axi_prefix}_w_payload_data")
        self.w_strb      = getattr(dut, f"{axi_prefix}_w_payload_strb")

        self.b_valid     = getattr(dut, f"{axi_prefix}_b_valid")
        self.b_ready     = getattr(dut, f"{axi_prefix}_b_ready")
        self.b_resp      = getattr(dut, f"{axi_prefix}_b_payload_resp")

        self.ar_valid    = getattr(dut, f"{axi_prefix}_ar_valid")
        self.ar_ready    = getattr(dut, f"{axi_prefix}_ar_ready")
        self.ar_addr     = getattr(dut, f"{axi_prefix}_ar_payload_addr")
        self.ar_prot     = getattr(dut, f"{axi_prefix}_ar_payload_prot")

        self.r_valid     = getattr(dut, f"{axi_prefix}_r_valid")
        self.r_ready     = getattr(dut, f"{axi_prefix}_r_ready")
        self.r_data      = getattr(dut, f"{axi_prefix}_r_payload_data")
        self.r_resp      = getattr(dut, f"{axi_prefix}_r_payload_resp")

        # 2. 全量初始化 + 强制B通道空闲（核心修复）
        self._force_idle_state()

        # 3. 打印初始化状态
        cocotb.log.info(f"\n=== {axi_prefix} 信号初始化状态 ===")
        print_signal_status(self.aw_valid,  f"{axi_prefix}_aw_valid")
        print_signal_status(self.aw_ready,  f"{axi_prefix}_aw_ready")
        print_signal_status(self.w_valid,   f"{axi_prefix}_w_valid")
        print_signal_status(self.w_ready,   f"{axi_prefix}_w_ready")
        print_signal_status(self.b_valid,   f"{axi_prefix}_b_valid")
        print_signal_status(self.b_ready,   f"{axi_prefix}_b_ready")
        cocotb.log.info("="*50 + "\n")

    def _force_idle_state(self):
        """终极修复：强制所有通道为空闲态（尤其是B通道）"""
        # AW通道：空闲
        self.aw_valid.value = 0
        self.aw_addr.value = 0
        self.aw_prot.value = 0
        # W通道：空闲
        self.w_valid.value = 0
        self.w_data.value = 0
        self.w_strb.value = 0
        # B通道：强制空闲（核心！b_valid必须为0，b_ready为0）
        self.b_ready.value = 0
        # 额外：主动等待b_valid变为0（若复位后b_valid是1，先握手清空）
        cocotb.scheduler.add(self._clear_b_channel())
        # AR通道：空闲
        self.ar_valid.value = 0
        self.ar_addr.value = 0
        self.ar_prot.value = 0
        # R通道：空闲
        self.r_ready.value = 0

    async def _clear_b_channel(self):
        """清空B通道的未完成响应（若b_valid=1，主动握手）"""
        await RisingEdge(self.clk)
        if self.b_valid.value == 1:
            cocotb.log.warning(f"[B通道清理] 复位后b_valid=1，主动握手清空")
            self.b_ready.value = 1
            await RisingEdge(self.clk)
            self.b_ready.value = 0
        cocotb.log.info(f"[B通道清理] B通道已空闲（b_valid={self.b_valid.value}）")

    async def _wait_signal_with_timeout(self, signal, target_value, timeout_cycles: int):
        """等待逻辑（无修改）"""
        cycle_count = 0
        while signal.value != target_value:
            await RisingEdge(self.clk)
            cycle_count += 1
            if cycle_count % 5 == 0:
                print_signal_status(signal, signal._name if hasattr(signal, '_name') else "unknown")
            if cycle_count >= timeout_cycles:
                cocotb.log.error(f"[超时] 等待{signal._name}到{target_value}，已等{cycle_count}周期（超时{timeout_cycles}周期）")
                return False
        return True

    async def write_reg(self, addr, data, strb=0xf, prot=0, timeout_ms=1):
        """写操作（AW阶段强制B通道空闲）"""
        timeout_cycles = int(timeout_ms * 10 * 10 / 20)
        cocotb.log.info(f"\n[WRITE_REG] 写操作：addr=0x{addr:04x}, data=0x{data:08x}（超时{timeout_ms}ms/{timeout_cycles}周期）")

        # 前置：确保B通道是空闲态（核心！）
        await self._clear_b_channel()

        # 1. 同时驱动AW+W通道（适配联动）
        self.w_data.value = data
        self.w_strb.value = strb
        self.w_valid.value = 1  # W提前置1
        self.aw_addr.value = addr
        self.aw_prot.value = prot
        self.aw_valid.value = 1
        self.b_ready.value = 1

        # 打印当前所有关键信号（排查问题）
        cocotb.log.info(f"[AW阶段] 驱动后信号状态：")
        print_signal_status(self.aw_valid,  f"{self.prefix}_aw_valid")
        print_signal_status(self.aw_ready,  f"{self.prefix}_aw_ready")
        print_signal_status(self.w_valid,   f"{self.prefix}_w_valid")
        print_signal_status(self.b_valid,   f"{self.prefix}_b_valid")
        print_signal_status(self.b_ready,   f"{self.prefix}_b_ready")

        # 2. 等待AW握手
        cocotb.log.info(f"[AW阶段] 等待aw_ready=1")
        if not await self._wait_signal_with_timeout(self.aw_ready, 1, timeout_cycles):
            self.aw_valid.value = 0
            self.w_valid.value = 0
            return -1
        cocotb.log.info(f"[AW阶段] ✅ aw_ready=1，完成握手")
        self.aw_valid.value = 0

        # 3. 等待W握手
        cocotb.log.info(f"[W阶段] 等待w_ready=1")
        if not await self._wait_signal_with_timeout(self.w_ready, 1, timeout_cycles):
            self.w_valid.value = 0
            return -2
        cocotb.log.info(f"[W阶段] ✅ w_ready=1，完成握手")
        self.w_valid.value = 0

        # 4. 等待B响应
        self.b_ready.value = 1
        cocotb.log.info(f"[B阶段] 驱动b_ready=1，等待b_valid=1")
        if not await self._wait_signal_with_timeout(self.b_valid, 1, timeout_cycles):
            self.b_ready.value = 0
            return -3
        cocotb.log.info(f"[B阶段] ✅ b_valid=1，完成响应")

        # 读取响应
        resp = self.b_resp.value
        self.b_ready.value = 0
        cocotb.log.info(f"[WRITE_REG] ✅ 写完成，b_resp=0x{resp.integer:02x}")
        return resp

    async def read_reg(self, addr, prot=0, timeout_ms=1):
        """读操作（无修改）"""
        timeout_cycles = int(timeout_ms * 10 * 10 / 20)
        cocotb.log.info(f"\n[READ_REG] 读操作：addr=0x{addr:04x}（超时{timeout_ms}ms/{timeout_cycles}周期）")

        self.ar_addr.value = addr
        self.ar_prot.value = prot
        self.ar_valid.value = 1
        cocotb.log.info(f"[AR阶段] 驱动ar_valid=1, ar_addr=0x{addr:04x}")
        if not await self._wait_signal_with_timeout(self.ar_ready, 1, timeout_cycles):
            self.ar_valid.value = 0
            return (-1, -1)
        cocotb.log.info(f"[AR阶段] ✅ ar_ready=1，完成握手")
        self.ar_valid.value = 0

        self.r_ready.value = 1
        cocotb.log.info(f"[R阶段] 驱动r_ready=1，等待r_valid=1")
        if not await self._wait_signal_with_timeout(self.r_valid, 1, timeout_cycles):
            self.r_ready.value = 0
            return (-2, -2)
        cocotb.log.info(f"[R阶段] ✅ r_valid=1，完成响应")

        data = self.r_data.value
        resp = self.r_resp.value
        self.r_ready.value = 0
        cocotb.log.info(f"[READ_REG] ✅ 读完成，data=0x{data:08x}, r_resp=0x{resp:02x}")
        return (data, resp)

# -------------------------
# AXI4 驱动类（适配Verilog实际信号命名：带_payload_后缀）
# -------------------------
class Axi4Driver:
    """适配 ttu_core 的 AXI4 驱动（匹配Verilog生成的_payload_后缀信号）"""
    def __init__(self, dut, axi_prefix, clk):
        self.dut = dut
        self.clk = clk
        self.prefix = axi_prefix

        # 1. 严格匹配Verilog信号命名绑定（核心修改点）
        # 写地址通道（TTU作为Master → 输出aw_valid，输入aw_ready）
        self.aw_valid     = getattr(dut, f"{axi_prefix}_aw_valid")
        self.aw_ready     = getattr(dut, f"{axi_prefix}_aw_ready")
        self.aw_addr      = getattr(dut, f"{axi_prefix}_aw_payload_addr")      # [19:0]
        self.aw_id        = getattr(dut, f"{axi_prefix}_aw_payload_id")        # [3:0]
        self.aw_region    = getattr(dut, f"{axi_prefix}_aw_payload_region")    # [3:0]
        self.aw_len       = getattr(dut, f"{axi_prefix}_aw_payload_len")       # [7:0]
        self.aw_size      = getattr(dut, f"{axi_prefix}_aw_payload_size")      # [2:0]
        self.aw_burst     = getattr(dut, f"{axi_prefix}_aw_payload_burst")     # [1:0]
        self.aw_lock      = getattr(dut, f"{axi_prefix}_aw_payload_lock")      # [0:0]
        self.aw_cache     = getattr(dut, f"{axi_prefix}_aw_payload_cache")     # [3:0]
        self.aw_qos       = getattr(dut, f"{axi_prefix}_aw_payload_qos")       # [3:0]
        self.aw_prot      = getattr(dut, f"{axi_prefix}_aw_payload_prot")      # [2:0]

        # 写数据通道
        self.w_valid      = getattr(dut, f"{axi_prefix}_w_valid")
        self.w_ready      = getattr(dut, f"{axi_prefix}_w_ready")
        self.w_data       = getattr(dut, f"{axi_prefix}_w_payload_data")       # [31:0]
        self.w_strb       = getattr(dut, f"{axi_prefix}_w_payload_strb")       # [3:0]
        self.w_last       = getattr(dut, f"{axi_prefix}_w_payload_last")       # 单bit

        # 写响应通道（TTU作为Master → 输入b_valid，输出b_ready）
        self.b_valid      = getattr(dut, f"{axi_prefix}_b_valid")
        self.b_ready      = getattr(dut, f"{axi_prefix}_b_ready")
        self.b_id         = getattr(dut, f"{axi_prefix}_b_payload_id")         # [3:0]
        self.b_resp       = getattr(dut, f"{axi_prefix}_b_payload_resp")       # [1:0]

        # 读地址通道
        self.ar_valid     = getattr(dut, f"{axi_prefix}_ar_valid")
        self.ar_ready     = getattr(dut, f"{axi_prefix}_ar_ready")
        self.ar_addr      = getattr(dut, f"{axi_prefix}_ar_payload_addr")      # [19:0]
        self.ar_id        = getattr(dut, f"{axi_prefix}_ar_payload_id")        # [3:0]
        self.ar_region    = getattr(dut, f"{axi_prefix}_ar_payload_region")    # [3:0]
        self.ar_len       = getattr(dut, f"{axi_prefix}_ar_payload_len")       # [7:0]
        self.ar_size      = getattr(dut, f"{axi_prefix}_ar_payload_size")      # [2:0]
        self.ar_burst     = getattr(dut, f"{axi_prefix}_ar_payload_burst")     # [1:0]
        self.ar_lock      = getattr(dut, f"{axi_prefix}_ar_payload_lock")      # [0:0]
        self.ar_cache     = getattr(dut, f"{axi_prefix}_ar_payload_cache")     # [3:0]
        self.ar_qos       = getattr(dut, f"{axi_prefix}_ar_payload_qos")       # [3:0]
        self.ar_prot      = getattr(dut, f"{axi_prefix}_ar_payload_prot")      # [2:0]

        # 读数据通道（TTU作为Master → 输入r_valid，输出r_ready）
        self.r_valid      = getattr(dut, f"{axi_prefix}_r_valid")
        self.r_ready      = getattr(dut, f"{axi_prefix}_r_ready")
        self.r_data       = getattr(dut, f"{axi_prefix}_r_payload_data")       # [31:0]
        self.r_id         = getattr(dut, f"{axi_prefix}_r_payload_id")         # [3:0]
        self.r_resp       = getattr(dut, f"{axi_prefix}_r_payload_resp")       # [1:0]
        self.r_last       = getattr(dut, f"{axi_prefix}_r_payload_last")       # 单bit

        # 2. 初始化所有Slave侧信号（模拟外部Slave响应）
        self._init_slave_signals()

        # 3. 打印初始化状态（包含Verilog特有的信号）
        cocotb.log.info(f"\n=== {axi_prefix} (AXI4) 信号初始化状态（匹配Verilog）===")
        print_signal_status(self.aw_valid,  f"{axi_prefix}_aw_valid")
        print_signal_status(self.aw_ready,  f"{axi_prefix}_aw_ready")
        print_signal_status(self.aw_addr,   f"{axi_prefix}_aw_payload_addr")
        print_signal_status(self.ar_valid,  f"{axi_prefix}_ar_valid")
        print_signal_status(self.ar_ready,  f"{axi_prefix}_ar_ready")
        print_signal_status(self.ar_addr,   f"{axi_prefix}_ar_payload_addr")
        cocotb.log.info("="*60 + "\n")

    def _init_slave_signals(self):
        """初始化Slave侧信号（严格匹配Verilog信号，避免悬空）"""
        # --------------------------
        # Slave侧输出信号（模拟响应 → 初始化为0）
        # --------------------------
        # 写地址通道响应
        self.aw_ready.value = 0
        # 写数据通道响应
        self.w_ready.value = 0
        # 写响应通道（Slave输出给TTU）
        self.b_valid.value = 0
        self.b_id.value = 0        # 默认ID=0
        self.b_resp.value = 0      # 默认OKAY响应（[1:0]=00）
        # 读地址通道响应
        self.ar_ready.value = 0
        # 读数据通道（Slave输出给TTU）
        self.r_valid.value = 0
        self.r_data.value = 0      # 默认数据=0
        self.r_id.value = 0        # 默认ID=0
        self.r_resp.value = 0      # 默认OKAY响应
        self.r_last.value = 0      # 默认last=0

        # --------------------------
        # TTU侧输出信号（Master → 无需初始化，由TTU驱动）
        # --------------------------
        # aw_valid/aw_addr/w_valid/w_data/ar_valid/ar_addr等由TTU驱动，此处不赋值

    async def mock_slave_write_response(self, delay_cycles=2):
        """模拟Slave响应TTU Master的写操作（适配Verilog信号）"""
        cocotb.log.info(f"\n[AXI4_WRITE] 模拟Slave响应TTU Master写操作（延迟{delay_cycles}周期）")

        # 1. 等待TTU发起写地址（aw_valid=1）
        await self._wait_signal_with_timeout(self.aw_valid, 1, 1000)
        cocotb.log.info(
            f"[AXI4_WRITE] 检测到TTU aw_valid=1 | "
            f"addr=0x{self.aw_addr.value.integer:06x} (19:0) | "
            f"len=0x{self.aw_len.value.integer:02x} | "
            f"burst=0x{self.aw_burst.value.integer:01x}"
        )

        # 2. 延迟后响应aw_ready=1（完成AW握手）
        await ClockCycles(self.clk, delay_cycles)
        self.aw_ready.value = 1
        await RisingEdge(self.clk)
        self.aw_ready.value = 0
        cocotb.log.info(f"[AXI4_WRITE] 已响应aw_ready=1，完成AW握手")

        # 3. 等待TTU发起写数据（w_valid=1）
        await self._wait_signal_with_timeout(self.w_valid, 1, 1000)
        cocotb.log.info(
            f"[AXI4_WRITE] 检测到TTU w_valid=1 | "
            f"data=0x{self.w_data.value.integer:08x} | "
            f"strb=0x{self.w_strb.value.integer:01x} | "
            f"last={self.w_last.value.integer}"
        )

        # 4. 延迟后响应w_ready=1（完成W握手）
        await ClockCycles(self.clk, delay_cycles)
        self.w_ready.value = 1
        await RisingEdge(self.clk)
        self.w_ready.value = 0
        cocotb.log.info(f"[AXI4_WRITE] 已响应w_ready=1，完成W握手")

        # 5. 发送写响应（b_valid=1 + OKAY响应）
        await ClockCycles(self.clk, delay_cycles)
        self.b_resp.value = 0        # [1:0] = 00 → OKAY
        self.b_id.value = self.aw_id.value  # 回显TTU的aw_id
        self.b_valid.value = 1
        cocotb.log.info(
            f"[AXI4_WRITE] 发送b_valid=1 | "
            f"resp=0x{self.b_resp.value.integer:02x} (OKAY) | "
            f"id=0x{self.b_id.value.integer:01x}"
        )

        # 6. 等待TTU响应b_ready=1（完成B握手）
        await self._wait_signal_with_timeout(self.b_ready, 1, 1000)
        await RisingEdge(self.clk)
        self.b_valid.value = 0
        cocotb.log.info(f"[AXI4_WRITE] ✅ TTU Master写操作响应完成")
        return self.b_resp.value.integer  # 返回响应码（整数）

    async def mock_slave_read_response(
            self,
            delay_cycles=2,
            mock_data=None,
            trans_len=1,
            l1_dw=32*8,          # 新增：AXI4数据位宽（对应memAxi.dataWidth）
            l1_aw=20,          # 新增：AXI4地址位宽（对应memAxi.addressWidth）
            axi_id_width=4     # 新增：AXI4 ID位宽（对应memAxi.idWidth）
    ):
        """
        模拟Slave响应TTU Master的AXI4读操作（通过传参指定硬件配置）
        :param delay_cycles: 每笔数据的响应延迟周期
        :param mock_data: 模拟数据（列表，每个元素是l1_dw位整数，长度=trans_len）
        :param trans_len: 连续传输的数据个数（AXI4：ar_len = trans_len-1）
        :param l1_dw: AXI4数据位宽（对应memAxi.dataWidth，默认64bit）
        :param l1_aw: AXI4地址位宽（对应memAxi.addressWidth，默认32bit）
        :param axi_id_width: AXI4 ID位宽（对应memAxi.idWidth，默认4bit）
        :return: 传输的所有数据列表
        """
        # ========== 1. 硬件配置参数校验（传参后先校验合法性） ==========
        # 数据位宽必须是8的整数倍（字节对齐）
        if l1_dw % 8 != 0:
            raise ValueError(f"l1_dw必须是8的整数倍（字节对齐），当前值：{l1_dw}")
        # 地址/ID位宽必须≥1
        if l1_aw < 1:
            raise ValueError(f"l1_aw必须≥1，当前值：{l1_aw}")
        if axi_id_width < 1:
            raise ValueError(f"axi_id_width必须≥1，当前值：{axi_id_width}")

        data_byte_width = l1_dw // 8  # 数据字节数（64bit=8字节，32bit=4字节）

        # ========== 2. 严格校验mock_data格式（适配传参的l1_dw位宽） ==========
        transfer_data = []
        if mock_data is None:
            raise ValueError(f"必须传入mock_data参数，且为长度{trans_len}的列表（每个元素是{l1_dw}位整数）")
        elif not isinstance(mock_data, list):
            raise TypeError(f"mock_data必须是列表类型（长度{trans_len}），当前类型：{type(mock_data)}")
        elif len(mock_data) != trans_len:
            raise ValueError(f"mock_data列表长度必须为{trans_len}（当前长度：{len(mock_data)}）")

        # 校验每个数据的位宽是否符合传参的l1_dw
        for idx, d in enumerate(mock_data):
            if not isinstance(d, int):
                raise TypeError(f"mock_data[{idx}]必须是整数（{l1_dw}位），当前类型：{type(d)}")
            # 检查数据是否超出l1_dw位无符号数范围
            max_data = (1 << l1_dw) - 1
            if d < 0 or d > max_data:
                raise ValueError(f"mock_data[{idx}]={d} 超出{l1_dw}位无符号数范围（0~{max_data}）")

        mock_data_list = mock_data
        cocotb.log.info(f"\n[AXI4_READ] 模拟Slave响应（传参配置：l1_dw={l1_dw}bit, l1_aw={l1_aw}bit, axi_id_width={axi_id_width}bit）")
        cocotb.log.info(f"[AXI4_READ] 连续传输{trans_len}笔数据 | 延迟{delay_cycles}周期/笔")
        # 动态计算数据16进制打印宽度
        data_hex_width = l1_dw // 4
        cocotb.log.info(f"[AXI4_READ] 待传输数据：{[f'0x{d:0{data_hex_width}x}' for d in mock_data_list]}")

        # ========== 3. 等待并校验AR通道（适配传参的l1_aw/axi_id_width） ==========
        # 等待ar_valid=1（带超时）
        await self._wait_signal_with_timeout(self.ar_valid, 1, 1000)

        # 校验ar_len（AXI4：ar_len = 传输个数-1）
        ttu_ar_len = self.ar_len.value.integer
        if ttu_ar_len != trans_len - 1:
            cocotb.log.warning(f"[AXI4_READ] TTU ar_len={ttu_ar_len}，应配置为{trans_len-1}（trans_len={trans_len}）")

        # 读取并校验AR通道信号（适配传参的位宽）
        ttu_ar_addr = self.ar_addr.value.integer
        ttu_ar_id = self.ar_id.value.integer
        # 校验ar_id是否超出传参的axi_id_width范围
        max_ar_id = (1 << axi_id_width) - 1
        if ttu_ar_id > max_ar_id:
            cocotb.log.warning(f"[AXI4_READ] TTU ar_id={ttu_ar_id} 超出{axi_id_width}位范围（0~{max_ar_id}）")

        # 动态计算地址16进制打印宽度（根据传参的l1_aw）
        addr_hex_width = (l1_aw + 3) // 4  # 位宽转16进制字符数（32bit=8位，64bit=16位）
        id_hex_width = (axi_id_width + 3) // 4  # ID位宽转16进制字符数（4bit=1位）
        cocotb.log.info(
            f"[AXI4_READ] 检测到AR握手请求 | "
            f"addr=0x{ttu_ar_addr:0{addr_hex_width}x} ({l1_aw-1}:0) | "
            f"ar_len=0x{ttu_ar_len:02x} (传输{trans_len}笔) | "
            f"ar_id=0x{ttu_ar_id:0{id_hex_width}x} ({axi_id_width-1}:0) | "
            f"burst=0x{self.ar_burst.value.integer:01x}"
        )

        # ========== 4. 响应AR握手（标准AXI4流程） ==========
        await ClockCycles(self.clk, delay_cycles)
        self.ar_ready.value = 1
        await RisingEdge(self.clk)
        self.ar_ready.value = 0
        cocotb.log.info(f"[AXI4_READ] 完成AR握手，开始连续传输{trans_len}笔{l1_dw}bit数据")

        # ========== 5. 循环发送R通道数据（适配传参的硬件配置） ==========
        for i in range(trans_len):
            # 每笔数据延迟
            await ClockCycles(self.clk, delay_cycles)

            # 当前笔数据（l1_dw位）
            curr_data = mock_data_list[i]
            is_last = 1 if (i == trans_len - 1) else 0

            # 驱动R通道信号（严格适配传参的硬件配置）
            self.r_data.value = curr_data                  # 传参的l1_dw位数据
            self.r_resp.value = 0                          # 00=OKAY响应（AXI4标准）
            self.r_id.value = ttu_ar_id                    # 回显AR ID（适配传参的axi_id_width）
            self.r_last.value = is_last                    # 最后一笔置1
            self.r_valid.value = 1

            # 打印日志（动态适配传参的位宽）
            cocotb.log.info(
                f"[AXI4_READ] 发送第{i+1}/{trans_len}笔 | "
                f"data=0x{curr_data:0{data_hex_width}x} ({l1_dw}bit) | "
                f"r_resp=0x{self.r_resp.value.integer:02x} | "
                f"r_id=0x{ttu_ar_id:0{id_hex_width}x} | "
                f"last={is_last} {'(最后一笔)' if is_last else ''}"
            )

            # 等待TTU的r_ready=1（完成R握手）
            if not await self._wait_signal_with_timeout(self.r_ready, 1, 1000):
                cocotb.log.error(f"[AXI4_READ] 第{i+1}笔数据等待r_ready超时，终止传输")
                self.r_valid.value = 0
                self.r_last.value = 0
                return transfer_data

            # 握手完成，释放R通道信号
            await RisingEdge(self.clk)
            self.r_valid.value = 0
            if is_last:
                self.r_last.value = 0

            # 记录传输的数据
            transfer_data.append(curr_data)
            cocotb.log.info(f"[AXI4_READ] 第{i+1}/{trans_len}笔{l1_dw}bit数据传输完成")

        # ========== 6. 传输完成 ==========
        cocotb.log.info(f"[AXI4_READ] ✅ AXI4读操作完成 | 共传输{trans_len}笔{l1_dw}bit数据 | "
                        f"数据列表：{[f'0x{d:0{data_hex_width}x}' for d in transfer_data]}")
        return transfer_data

    async def _wait_signal_with_timeout(self, signal, target_value, timeout_cycles: int):
        """AXI4专用等待函数（适配Verilog信号，无None值）"""
        cycle_count = 0
        target_int = target_value if isinstance(target_value, int) else target_value.integer
        while signal.value.integer != target_int:
            await RisingEdge(self.clk)
            cycle_count += 1
            if cycle_count >= timeout_cycles:
                cocotb.log.error(
                    f"[AXI4超时] 等待{signal._name}到{target_int} | "
                    f"已等{cycle_count}周期（超时{timeout_cycles}周期） | "
                    f"当前值={signal.value.integer}"
                )
                return False
        return True

@cocotb.coroutine
async def axi_read_transfer_old(axi_inst, raw_8bit_len, target_addr, max_burst_len=8, timeout_cycles=10000):
    """
    通用AXI4读传输函数：生成8bit递增数据→128bit拼接，并行传输
    :param axi_inst: AXI从机实例（如dma0_axi）
    :param raw_8bit_len: 原始8bit数据长度（ifm=128，ker=576）
    :param target_addr: 等待的目标地址（ifm=0x8000，ker=0x10000）
    :param max_burst_len: 最大突发长度
    :param timeout_cycles: 超时周期
    :return: 完整的传输数据列表（每个元素是128bit整数）
    """
    # ======================
    # 核心：8bit → 256bit 拼接
    # 256bit = 32 字节
    # ======================
    align_8bit_len = ((raw_8bit_len + 31) // 32) * 32  # 向上对齐32字节
    raw_8bit_data = list(range(raw_8bit_len)) + [0] * (align_8bit_len - raw_8bit_len)

    data_256bit = []
    for i in range(0, align_8bit_len, 32):
        val = 0
        for j in range(32):  # 小端：byte0 最低位，byte31 最高位
            val |= (raw_8bit_data[i + j] & 0xFF) << (8 * j)
        data_256bit.append(val)

    total_len = len(data_256bit)
    cocotb.log.info(f"📦 生成 {raw_8bit_len} 8bit → {total_len} 个 256bit 数据，等待地址 ≥ 0x{target_addr:08x}")

    # ========== 2. 等待目标地址（逻辑不变，仅日志适配） ==========
    cycle_count = 0
    while axi_inst.ar_addr.value.integer < target_addr:
        await RisingEdge(axi_inst.clk)
        cycle_count += 1
        if cycle_count >= timeout_cycles:
            raise TimeoutError(f"等待地址>=0x{target_addr:08x}超时！当前地址=0x{axi_inst.ar_addr.value.integer:08x}")
    cocotb.log.info(f"✅ {raw_8bit_len}bit数据传输：检测到地址=0x{axi_inst.ar_addr.value.integer:08x} >= 0x{target_addr:08x}")

    # ========== 3. 切分突发传输（逻辑不变，数据位宽改为128bit） ==========
    start_idx = 0
    total_data = []
    while start_idx < total_len:
        current_trans_len = min(max_burst_len, total_len - start_idx)
        current_mock_data = data_256bit[start_idx:start_idx + current_trans_len]

        # 调用mock_slave_read_response（需确保该函数已适配128bit）
        data_segment = await axi_inst.mock_slave_read_response(
            mock_data=current_mock_data,
            trans_len=current_trans_len,
            l1_dw=256  # 显式指定128bit数据位宽
        )

        # 拼接数据（按实际返回长度）
        actual_len = len(data_segment)
        total_data += data_segment

        # 打印进度（适配128bit）
        cocotb.log.info(
            f"📤 {raw_8bit_len}bit数据传输：起始索引{start_idx}，突发长度{actual_len}，"
            f"已传输{start_idx+actual_len}/{total_len}个128bit数据（对应{(start_idx+actual_len)*16}字节）"
        )

        # 更新索引
        start_idx += actual_len

    # ========== 4. 验证结果（适配128bit） ==========
    if len(total_data) == total_len:
        last_data_hex = f"0x{total_data[-1]:032x}" if total_data else "无"
        cocotb.log.info(f"🎉 {raw_8bit_len}bit数据传输完成！总长度：{len(total_data)}个128bit数据，最后一个数据：{last_data_hex}")
    else:
        cocotb.log.error(f"❌ {raw_8bit_len}bit数据传输不完整：实际{len(total_data)}/{total_len}个128bit数据")
        # 应急截断
        total_data = total_data[:total_len]

    return total_data

async def axi_read_transfer(
        axi_inst,
        rawdata,
        dtype="u8",
        target_addr=0x0,
        max_burst_len=8,
        timeout_cycles=10000,
        l1_dw=256,
        endian="little",
):
    """
    通用AXI4读传输函数：根据外部输入 rawdata + dtype，做打包(8/16/32/64bit -> l1_dw bit line)，然后按原逻辑突发传输
    传输逻辑保持不变：等待地址>=target_addr -> 分burst -> 调 mock_slave_read_response -> 拼接返回

    Args:
        axi_inst: AXI从机实例（如 dma0_axi）
        rawdata: 外部输入数据数组（list/tuple/iterable），元素为 int
        dtype: 数据类型字符串： "u8","s8","u16","s16","u32","s32","u64","s64"
        target_addr: 等待的目标地址门限（例如 ifm=0x8000）
        max_burst_len: 最大突发长度（以“line”为单位，line宽度=l1_dw）
        timeout_cycles: 超时周期
        l1_dw: L1 line 位宽（默认256bit=32B）
        endian: "little" or "big"（决定一个 line 内 byte 的拼接顺序）
    Returns:
        total_data: list[int]，每个元素是 l1_dw-bit 的整数（按 line 返回）
    """

    # --------------------------
    # 0) dtype 解析
    # --------------------------
    dtype = dtype.lower().strip()
    dtype_map = {
        "u8":  (8,  False),
        "s8":  (8,  True),
        "u16": (16, False),
        "s16": (16, True),
        "u32": (32, False),
        "s32": (32, True),
        "u64": (64, False),
        "s64": (64, True),
    }
    if dtype not in dtype_map:
        raise ValueError(f"Unsupported dtype={dtype}, choose from {list(dtype_map.keys())}")

    elem_bits, signed = dtype_map[dtype]
    if l1_dw % 8 != 0:
        raise ValueError(f"l1_dw must be multiple of 8, got {l1_dw}")
    line_bytes = l1_dw // 8
    elem_bytes = elem_bits // 8

    if endian not in ("little", "big"):
        raise ValueError(f"endian must be 'little' or 'big', got {endian}")

    # --------------------------
    # 1) rawdata -> byte stream (two's complement for signed) -> pad to line_bytes
    # --------------------------
    raw_list = list(rawdata)
    raw_elems = len(raw_list)
    cocotb.log.info(f"elements len = ${raw_elems}\n")

    def _to_unsigned(v: int, bits: int, signed_: bool) -> int:
        mask = (1 << bits) - 1
        if signed_:
            # allow negative values
            v = int(v)
            if v < 0:
                v = (v + (1 << bits)) & mask
            else:
                v = v & mask
        else:
            v = int(v) & mask
        return v

    byte_stream = []
    for v in raw_list:
        uv = _to_unsigned(v, elem_bits, signed)
        # 以 elem 的 little-endian 字节序写入 byte_stream（最常见的内存布局）
        for b in range(elem_bytes):
            byte_stream.append((uv >> (8 * b)) & 0xFF)

    raw_byte_len = len(byte_stream)
    align_byte_len = ((raw_byte_len + line_bytes - 1) // line_bytes) * line_bytes
    if align_byte_len > raw_byte_len:
        byte_stream += [0] * (align_byte_len - raw_byte_len)

    cocotb.log.info(f"raw_byte_len = ${raw_byte_len}\n")
    # --------------------------
    # 2) byte stream -> l1_dw-bit line list
    # --------------------------
    data_lines = []
    for i in range(0, align_byte_len, line_bytes):
        val = 0
        chunk = byte_stream[i : i + line_bytes]

        if endian == "little":
            # byte0 -> lowest bits
            for j, by in enumerate(chunk):
                val |= (by & 0xFF) << (8 * j)
        else:
            # byte0 -> highest bits
            for j, by in enumerate(chunk):
                val |= (by & 0xFF) << (8 * (line_bytes - 1 - j))

        data_lines.append(val)

    total_len = len(data_lines)
    cocotb.log.info(
        f"📦 raw_elems={raw_elems}, dtype={dtype} ({elem_bits}b), raw_bytes={raw_byte_len} -> "
        f"aligned_bytes={align_byte_len}, lines={total_len} (l1_dw={l1_dw}), "
        f"等待地址 ≥ 0x{target_addr:08x}"
    )

    # --------------------------
    # 3) 等待目标地址（逻辑不变）
    # --------------------------
    cycle_count = 0
    while axi_inst.ar_addr.value.integer < target_addr:
        await RisingEdge(axi_inst.clk)
        cycle_count += 1
        if cycle_count >= timeout_cycles:
            raise TimeoutError(
                f"等待地址>=0x{target_addr:08x}超时！当前地址=0x{axi_inst.ar_addr.value.integer:08x}"
            )
    cocotb.log.info(
        f"✅ 传输开始：检测到地址=0x{axi_inst.ar_addr.value.integer:08x} >= 0x{target_addr:08x}"
    )

    # --------------------------
    # 4) 切分突发传输（逻辑不变：按 line burst）
    # --------------------------
    start_idx = 0
    total_data = []

    cocotb.log.info(f"mock slave read")
    while start_idx < total_len:
        current_trans_len = min(max_burst_len, total_len - start_idx)
        current_mock_data = data_lines[start_idx : start_idx + current_trans_len]

        # 注意：mock_slave_read_response 应该支持 l1_dw 位宽
        data_segment = await axi_inst.mock_slave_read_response(
            mock_data=current_mock_data,
            trans_len=current_trans_len,
            l1_dw=l1_dw
        )

        actual_len = len(data_segment)
        total_data += data_segment

        cocotb.log.info(
            f"📤 传输进度：起始line {start_idx}，突发len {actual_len}，"
            f"已传输 {start_idx + actual_len}/{total_len} lines（对应 {(start_idx + actual_len) * line_bytes} bytes）"
        )

        start_idx += actual_len

    # --------------------------
    # 5) 校验结果
    # --------------------------
    if len(total_data) == total_len:
        hexw = l1_dw // 4
        last_data_hex = f"0x{total_data[-1]:0{hexw}x}" if total_data else "无"
        cocotb.log.info(
            f"🎉 传输完成！总长度：{len(total_data)} lines（l1_dw={l1_dw}），最后一个数据：{last_data_hex}"
        )
    else:
        cocotb.log.error(
            f"❌ 传输不完整：实际{len(total_data)}/{total_len} lines，执行截断"
        )
        total_data = total_data[:total_len]

    return total_data

# -------------------------
# 主测试函数（加DUT信号保底赋值）
# -------------------------
@cocotb.test()
async def test_ttu_core(dut):
    """ttu_core 测试（终极修复：强制所有通道空闲）"""
    # 1. 打印基础信息
    cocotb.log.info(f"\n=== 测试初始化 ===")
    cocotb.log.info(f"Cocotb版本：{cocotb.__version__ if hasattr(cocotb, '__version__') else '未知'}")
    print_signal_status(dut.clk, "clk")
    print_signal_status(dut.resetn, "resetn")

    # 2. 启动时钟（50MHz）
    clock = Clock(dut.clk, 20, units="ns")
    if hasattr(cocotb, 'start_soon'):
        cocotb.start_soon(clock.start())
    else:
        cocotb.fork(clock.start())
    cocotb.log.info("时钟已启动（50MHz）")

    # 3. 超彻底复位（核心：复位后等足够久，确保DUT内部状态清空）
    cocotb.log.info("\n=== 超彻底复位流程 ===")
    dut.resetn.value = 0
    await ClockCycles(dut.clk, 50)  # 复位50个周期（远超常规）
    print_signal_status(dut.resetn, "resetn")

    dut.resetn.value = 1
    await ClockCycles(dut.clk, 20)  # 释放后等20个周期
    print_signal_status(dut.resetn, "resetn")
    cocotb.log.info("复位完成")

    # 4. 给DUT的AXI-Lite信号加保底赋值（防止内部逻辑悬空）
    cocotb.log.info("\n=== 给DUT信号加保底赋值 ===")
    # 强制DUT的b_valid为0（若RTL未赋值）
    try:
        dut.io_ctrl_axi_b_valid.value = 0
    except:
        cocotb.log.warning("无法直接赋值io_ctrl_axi_b_valid（DUT输出信号）")

    # 5. 初始化驱动
    ctrl_axi = AxiLiteDriver(dut, "io_ctrl_axi", dut.clk)
    dma0_axi = Axi4Driver(dut, "io_dma0_axi", dut.clk)
    dma1_axi = Axi4Driver(dut, "io_dma1_axi", dut.clk)

    # 6. 核心测试：AXI-Lite写操作
    cocotb.log.info("\n=== 测试AXI-Lite寄存器写操作 ===")
    write_resp = await ctrl_axi.write_reg(addr=0x10, data=0x0, timeout_ms=2)  # 超时加长到2ms

    # 错误判断
    error_map = {
        -1: "AW通道超时（排查：1.地址0x10是否在DUT响应范围 2.DUT的aw_ready是否有赋值 3.B通道是否真的空闲）",
        -2: "W通道超时（DUT的w_ready逻辑异常）",
        -3: "B通道超时（DUT的b_valid逻辑异常）"
    }
    if write_resp == -1:
        cocotb.log.error(f"\n❌ 写寄存器失败：{error_map[write_resp]}")
        # 打印最终所有信号状态，方便排查
        cocotb.log.info("\n=== 最终信号状态（排查用）===")
        print_signal_status(ctrl_axi.aw_valid,  f"{ctrl_axi.prefix}_aw_valid")
        print_signal_status(ctrl_axi.aw_ready,  f"{ctrl_axi.prefix}_aw_ready")
        print_signal_status(ctrl_axi.w_valid,   f"{ctrl_axi.prefix}_w_valid")
        print_signal_status(ctrl_axi.w_ready,   f"{ctrl_axi.prefix}_w_ready")
        print_signal_status(ctrl_axi.b_valid,   f"{ctrl_axi.prefix}_b_valid")
        print_signal_status(ctrl_axi.b_ready,   f"{ctrl_axi.prefix}_b_ready")
        assert False, f"写寄存器失败：{error_map[write_resp]}"

    # 验证响应
    assert write_resp == 0, f"写响应错误，期望0，实际{write_resp}"
    cocotb.log.info(f"\n✅ 写寄存器0x10成功！响应码：0x{write_resp.integer:02x}")

    write_resp = await ctrl_axi.write_reg(addr=0x18, data=0x9, timeout_ms=2)    # cmd size
    write_resp = await ctrl_axi.write_reg(addr=0x1C, data=0x1, timeout_ms=2)
    write_resp = await ctrl_axi.write_reg(addr=0x24, data=0x0, timeout_ms=2)
    write_resp = await ctrl_axi.write_reg(addr=0x28, data=0x0, timeout_ms=2)
    write_resp = await ctrl_axi.write_reg(addr=0x2C, data=0x120, timeout_ms=2)    # cmd bytes
    write_resp = await ctrl_axi.write_reg(addr=0x30, data=0x1, timeout_ms=2)    # cmd axi trans len
    write_resp = await ctrl_axi.write_reg(addr=0x20, data=0x1, timeout_ms=2)    # write last one

# 7. DMA和中断测试
    cocotb.log.info("\n=== 测试DMA和中断(TODO) ===")
    # 定义8个32bit的数值列表（完全由你自定义）
    # little bian
#     cmd0 = [0x0000000000000008000000300000800080008000ffffffffffffffff00010004]
#     cmd1 = [0x0000000000000008000001b00001800080018000ffffffffffffffff00010004]
#     cmd2 = [0x0000000000000041089266006008008100008000ffffffffffff000000010102]
#     cmd3 = [0x0008000000010041089266006008008100018000ffffffffffff0001000101fe]
#     cmd4 = [0x000000080000801b000080040000200000000000ffffffff0003000200020281]
#     cmd5 = [0x00000000ddeeff0099aabbcc5566778811223344ffffffffffffffff00000005]
#     cmd6 = [0x000000080000801b000080040000200800000000ffffffffffff000400020181]
#     cmd7 = [0x00000000ddeeff0099aabbcc5566778811223344ffffffffffff000400000105]
#     cmd8 = [0x000000080000801b000080040000200000000002ffffffffffff000600020181]
#     cmd9 = [0x00000000ddeeff0099aabbcc5566778811223344ffffffffffff000600000105]
#     cmd10 = [0x000000080000801b000080040000200800000002ffffffffffff000800000181]
#     cmd11 = [0x00000000ddeeff0099aabbcc5566778811223344ffffffffffff000800000105]
    cmd0 = [0x0000000000000008000000300000800080008000ffffffffffffffff00010004]
    cmd1 = [0x0000000000000008000001b00001800080018000ffffffffffffffff00010004]
    cmd2 = [0x0000000000000041089266006008008100008000ffffffffffff000000010102]
    cmd3 = [0x0008000000010041089266006008008100018000ffffffffffff0001000101fe]
    cmd4 = [0x0000000000000000000000010008008100028000ffffffff0001000000010205]
    cmd5 = [0x000000080000801b000080040000200000000000ffff00040003000200010381]
    cmd6 = [0x000000080000801b000080040000200800000000ffffffffffff000500010181]
    cmd7 = [0x000000080000801b000080040000200000000002ffffffffffff000600010181]
    cmd8 = [0x000000080000801b000080040000200800000002ffffffffffff000700000181]
    # 调用时指定trans_len=8，mock_data=上述列表
    dma0_data = await dma0_axi.mock_slave_read_response(mock_data=cmd0, trans_len=1)
    dma0_data = await dma0_axi.mock_slave_read_response(mock_data=cmd1, trans_len=1)
    dma0_data = await dma0_axi.mock_slave_read_response(mock_data=cmd2, trans_len=1)
    dma0_data = await dma0_axi.mock_slave_read_response(mock_data=cmd3, trans_len=1)
    dma0_data = await dma0_axi.mock_slave_read_response(mock_data=cmd4, trans_len=1)
    dma0_data = await dma0_axi.mock_slave_read_response(mock_data=cmd5, trans_len=1)
    dma0_data = await dma0_axi.mock_slave_read_response(mock_data=cmd6, trans_len=1)
    dma0_data = await dma0_axi.mock_slave_read_response(mock_data=cmd7, trans_len=1)
    dma0_data = await dma0_axi.mock_slave_read_response(mock_data=cmd8, trans_len=1)
#     dma0_data = await dma0_axi.mock_slave_read_response(mock_data=cmd9, trans_len=1)
#     dma0_data = await dma0_axi.mock_slave_read_response(mock_data=cmd10, trans_len=1)
#     dma0_data = await dma0_axi.mock_slave_read_response(mock_data=cmd11, trans_len=1)

# 1. 生成32bit有效数据的ifm_data（0~99对应32bit值：0x00000000 ~ 0x00000063）
    ifm_data = [i & 0xFFFFFFFF for i in range(48)]  # 确保是32bit无符号整数
    ker_data = [(i%16)+1 & 0xFFFFFFFF for i in range(432)]  # 确保是32bit无符号整数

    golden = conv2d_ref(
        ifm_data,
        ker_data,
        H=4,
        W=4,
        C=3,
        Kh=3,
        Kw=3,
        Oc=16,
        stride_h=1,
        stride_w=1,
        pad_h=1,
        pad_w=1
    )
    # cocotb.log.info(golden)

    #global dma0_axi, dma1_axi  # 你的两个AXI实例

    try:
        # ========== 极简并行：cocotb 1.9.2 原生支持 ==========
        # 1. 启动两个并行任务（fork是1.9.2底层API，无任何报错）
        # task_ifm = cocotb.fork(axi_read_transfer_old(dma0_axi, 128, 0x8000, 1))
        # task_ker = cocotb.fork(axi_read_transfer_old(dma1_axi, 576, 0x18000, 1))
        task_ifm = cocotb.fork(axi_read_transfer(dma0_axi, ifm_data, dtype="u8", target_addr=0x8000, max_burst_len=1, l1_dw=256))
        task_ker = cocotb.fork(axi_read_transfer(dma1_axi, ker_data, dtype="s8", target_addr=0x18000, max_burst_len=1, l1_dw=256))
        # 2. 等待两个任务完成（一行获取结果，等价于gather的简洁性）
        ifm_rdata = await task_ifm.join()
        ker_rdata = await task_ker.join()

        # ========== 结果验证 ==========
        #assert len(ifm_data) == 32, f"IFM长度错误：{len(ifm_data)}≠32"
        #assert len(ker_data) == 144, f"KER长度错误：{len(ker_data)}≠144"

        #cocotb.log.info(f"\n✅ 并行传输完成！IFM={len(ifm_data)}, KER={len(ker_data)}")
    except Exception as e:
        cocotb.log.error(f"❌ 异常：{e}")
        raise


    await RisingEdge(dut.io_irq), Timer(100_000, units="ns")
    assert dut.io_irq.value == 1, "❌超时！中断信号未触发"
    dut.io_irq.value = 1
    cocotb.log.info("✅ 中断信号检测成功")
    cocotb.log.info(golden)

    # 8. 测试完成
    cocotb.log.info("\n=== 所有测试完成 ===")
    await ClockCycles(dut.clk, 1000)