#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
NPU compiler for:
- Conv2D with implicit-im2col in CONV unit (AGU_CONV + CONV)
- Linear with GEMM unit (AGU_GEMM + GEMM)

Hardware assumption (per your message):
- AGU+DMA only moves contiguous/2D blocks (no gather/scatter)
- CONV/GEMM does window-gather internally (implicit im2col)
- Conv weights are standard W[OC, C, KH, KW] in DRAM (OIHW / OCHW)

Notes:
- Conv output is stored as Ycol [M, OC] where M=OH*OW (row-major by m then oc).
  This makes "conv -> linear" natural without extra reshape op.
- Quantization math (zp/scale/clamp) NOT implemented. This is a schedule/address compiler.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from enum import IntEnum
from typing import List, Dict, Tuple, Optional, Any
import struct
import argparse
import json
import os


# ============================================================
# ISA / Binary format
# ============================================================

class Op(IntEnum):
    NOP         = 0x00
    AGU_DMA     = 0x01  # configure DMA descriptor (SRC+DST 2D)
    DMA         = 0x02  # execute descriptor
    GEMM        = 0x03
    WAIT        = 0x04
    BUFF_SWITCH = 0x05
    STORE       = 0x06
    AGU_GEMM    = 0x07
    AGU_CONV    = 0x08
    CONV        = 0x09
    END         = 0xFF


class BufGroup(IntEnum):
    IN  = 0
    WGT = 1
    OUT = 2


class BufSel(IntEnum):
    PING = 0
    PONG = 1


class DmaDir(IntEnum):
    H2L1 = 0
    L12H = 1


class WaitEvent(IntEnum):
    DMA0_DONE  = 0
    DMA1_DONE  = 1
    GEMM_DONE  = 2
    STORE_DONE = 3
    CONV_DONE  = 4


MAX_DEPS = 4
RECORD_SIZE = 32
BIN_MAGIC = b"NPU0"
BIN_VERSION = 1


@dataclass
class Instr:
    op: Op
    w0: int = 0
    w1: int = 0
    w2: int = 0
    w3: int = 0
    deps: List[int] = field(default_factory=list)
    succ_count: int = 0
    comment: str = ""

    def encode_record(self) -> bytes:
        dep_count = min(len(self.deps), MAX_DEPS)
        deps_padded = (self.deps[:MAX_DEPS] + [0xFFFF] * MAX_DEPS)[:MAX_DEPS]
        return struct.pack(
            "<BBBB4H4I4s",
            int(self.op) & 0xFF,
            dep_count & 0xFF,
            self.succ_count & 0xFF,
            0,
            deps_padded[0] & 0xFFFF,
            deps_padded[1] & 0xFFFF,
            deps_padded[2] & 0xFFFF,
            deps_padded[3] & 0xFFFF,
            self.w0 & 0xFFFFFFFF,
            self.w1 & 0xFFFFFFFF,
            self.w2 & 0xFFFFFFFF,
            self.w3 & 0xFFFFFFFF,
            b"\x00\x00\x00\x00",
        )

    @staticmethod
    def decode_record(rec: bytes) -> "Instr":
        op, dep_count, succ_count, _rsv, d0, d1, d2, d3, w0, w1, w2, w3, _pad = struct.unpack("<BBBB4H4I4s", rec)
        deps_all = [d0, d1, d2, d3]
        deps = [d for d in deps_all[:dep_count] if d != 0xFFFF]
        return Instr(op=Op(op), w0=w0, w1=w1, w2=w2, w3=w3, deps=deps, succ_count=succ_count, comment="")

    def disasm(self, idx: int) -> str:
        deps_str = ",".join(str(d) for d in self.deps) if self.deps else "-"
        s = (f"{idx:04d}: {self.op.name:<11} "
             f"w0={self.w0:#010x} w1={self.w1:#010x} w2={self.w2:#010x} w3={self.w3:#010x} "
             f"deps=[{deps_str}] succ={self.succ_count}")
        if self.comment:
            s += f"  ; {self.comment}"
        return s


def write_bin(path: str, prog: List[Instr]) -> None:
    header = struct.pack("<4sHHI", BIN_MAGIC, BIN_VERSION, RECORD_SIZE, len(prog))
    with open(path, "wb") as f:
        f.write(header)
        for ins in prog:
            f.write(ins.encode_record())


def read_bin(path: str) -> List[Instr]:
    with open(path, "rb") as f:
        hdr = f.read(12)
        magic, ver, recsz, n = struct.unpack("<4sHHI", hdr)
        if magic != BIN_MAGIC or ver != BIN_VERSION or recsz != RECORD_SIZE:
            raise ValueError("Bad header")
        prog: List[Instr] = []
        for _ in range(n):
            rec = f.read(RECORD_SIZE)
            if len(rec) != RECORD_SIZE:
                raise ValueError("Truncated")
            prog.append(Instr.decode_record(rec))
        return prog


def write_disasm(path: str, prog: List[Instr]) -> None:
    with open(path, "w", encoding="utf-8") as f:
        for i, ins in enumerate(prog):
            f.write(ins.disasm(i) + "\n")


# ============================================================
# Network IR
# ============================================================

@dataclass
class Tensor:
    name: str
    shape: Tuple[int, ...]
    dtype: str = "fp16"
    addr: Optional[int] = None
    bytes: Optional[int] = None

    def bpe(self) -> int:
        dt = self.dtype.lower()
        if dt in ("fp16", "bf16", "f16"):
            return 2
        if dt in ("fp32", "f32"):
            return 4
        if dt in ("u8", "uint8", "i8", "int8"):
            return 1
        return 2

    def nbytes(self) -> int:
        if self.bytes is not None:
            return self.bytes
        n = 1
        for d in self.shape:
            n *= int(d)
        return n * self.bpe()


@dataclass
class OpNode:
    op_type: str
    name: str
    inputs: List[str]
    outputs: List[str]
    attrs: Dict[str, Any] = field(default_factory=dict)


@dataclass
class Network:
    tensors: Dict[str, Tensor] = field(default_factory=dict)
    ops: List[OpNode] = field(default_factory=list)

    def add_tensor(self, t: Tensor) -> None:
        if t.name in self.tensors:
            raise ValueError(f"Tensor exists: {t.name}")
        self.tensors[t.name] = t

    def add_op(self, op: OpNode) -> None:
        self.ops.append(op)


def load_network_from_json(path: str) -> Network:
    with open(path, "r", encoding="utf-8") as f:
        obj = json.load(f)
    net = Network()
    for t in obj.get("tensors", []):
        net.add_tensor(Tensor(
            name=t["name"],
            shape=tuple(int(x) for x in t["shape"]),
            dtype=t.get("dtype", "fp16"),
        ))
    for o in obj.get("ops", []):
        net.add_op(OpNode(
            op_type=o["op_type"],
            name=o.get("name", o["op_type"]),
            inputs=list(o["inputs"]),
            outputs=list(o["outputs"]),
            attrs=dict(o.get("attrs", {})),
        ))
    return net


# ============================================================
# Memory planning
# ============================================================

class MemoryPlanner:
    def __init__(self, base: int = 0x10000000, align: int = 256) -> None:
        self.base = base
        self.align = align

    def _align_up(self, x: int) -> int:
        a = self.align
        return (x + (a - 1)) // a * a

    def plan(self, net: Network) -> None:
        addr = self.base
        for name in sorted(net.tensors.keys()):
            t = net.tensors[name]
            addr = self._align_up(addr)
            t.addr = addr
            t.bytes = t.nbytes()
            addr += t.bytes


# ============================================================
# Tile IR
# ============================================================

@dataclass
class ConvTile:
    layer: str
    oh0: int
    ow0: int
    ohT: int
    owT: int
    oc0: int
    ocT: int
    # DRAM bases
    x_base_dram: int
    w_base_dram: int
    y_base_dram: int
    meta: Dict[str, Any] = field(default_factory=dict)


@dataclass
class GemmTile:
    layer: str
    m0: int
    n0: int
    k0: int
    tm: int
    tn: int
    tk: int
    a_base_dram: int
    b_base_dram: int
    c_base_dram: int
    meta: Dict[str, Any] = field(default_factory=dict)


# ============================================================
# Lowering
# ============================================================

def lower_network(net: Network, tile_oh: int, tile_ow: int, tile_oc: int,
                 tile_m: int, tile_n: int, tile_k: int) -> Tuple[List[ConvTile], List[GemmTile]]:
    conv_tiles: List[ConvTile] = []
    gemm_tiles: List[GemmTile] = []

    for op in net.ops:
        if op.op_type == "Conv2D":
            X = net.tensors[op.inputs[0]]   # [1,C,H,W]
            W = net.tensors[op.inputs[1]]   # [OC,C,KH,KW]
            Y = net.tensors[op.outputs[0]]  # expect Ycol [M,OC] (M=OH*OW)

            if X.addr is None or W.addr is None or Y.addr is None:
                raise ValueError("Memory not planned")

            layout = op.attrs.get("layout", "NCHW")
            if layout != "NCHW":
                raise NotImplementedError("Only NCHW supported here")

            stride = int(op.attrs.get("stride", 1))
            pad = int(op.attrs.get("pad", 0))
            dilation = int(op.attrs.get("dilation", 1))
            KH = int(op.attrs.get("kh", 3))
            KW = int(op.attrs.get("kw", 3))

            N, C, H, Ww = X.shape
            if N != 1:
                raise NotImplementedError("batch=1 only in this demo")

            OC, C2, KH2, KW2 = W.shape
            if C2 != C or KH2 != KH or KW2 != KW:
                raise ValueError(f"W shape mismatch: expect ({OC},{C},{KH},{KW}) got {W.shape}")

            OH = (H + 2*pad - dilation*(KH-1) - 1)//stride + 1
            OW = (Ww + 2*pad - dilation*(KW-1) - 1)//stride + 1
            M = OH * OW

            # Must be Ycol [M,OC] for conv->linear-friendly layout
            if Y.shape != (M, OC):
                raise ValueError(f"Conv output must be Ycol [M,OC]={((M,OC))}, got {Y.shape}")

            bpe_x = X.bpe()
            bpe_w = W.bpe()
            bpe_y = Y.bpe()

            for oc0 in range(0, OC, tile_oc):
                ocT = min(tile_oc, OC - oc0)
                for oh0 in range(0, OH, tile_oh):
                    ohT = min(tile_oh, OH - oh0)
                    for ow0 in range(0, OW, tile_ow):
                        owT = min(tile_ow, OW - ow0)
                        conv_tiles.append(ConvTile(
                            layer=op.name,
                            oh0=oh0, ow0=ow0, ohT=ohT, owT=owT,
                            oc0=oc0, ocT=ocT,
                            x_base_dram=int(X.addr),
                            w_base_dram=int(W.addr),
                            y_base_dram=int(Y.addr),
                            meta={
                                "C": C, "H": H, "W": Ww,
                                "OC": OC, "OH": OH, "OW": OW,
                                "KH": KH, "KW": KW,
                                "stride": stride, "pad": pad, "dilation": dilation,
                                "bpe_x": bpe_x, "bpe_w": bpe_w, "bpe_y": bpe_y,
                            }
                        ))

        elif op.op_type in ("Linear", "MatMul"):
            A = net.tensors[op.inputs[0]]  # [M,K]
            B = net.tensors[op.inputs[1]]  # [K,N]
            Cc = net.tensors[op.outputs[0]] # [M,N]
            if A.addr is None or B.addr is None or Cc.addr is None:
                raise ValueError("Memory not planned")
            if len(A.shape) != 2 or len(B.shape) != 2 or len(Cc.shape) != 2:
                raise ValueError(f"{op.name}: Linear expects 2D tensors, got A{A.shape} B{B.shape} C{Cc.shape}")
            M, K = A.shape
            K2, N = B.shape
            if K != K2:
                raise ValueError(f"{op.name}: shape mismatch A{A.shape} B{B.shape}")
            if Cc.shape != (M, N):
                raise ValueError(f"{op.name}: output shape mismatch expect {(M,N)} got {Cc.shape}")

            bpe_a = A.bpe()
            bpe_b = B.bpe()
            bpe_c = Cc.bpe()

            for m0 in range(0, M, tile_m):
                tm = min(tile_m, M - m0)
                for n0 in range(0, N, tile_n):
                    tn = min(tile_n, N - n0)
                    for k0 in range(0, K, tile_k):
                        tk = min(tile_k, K - k0)
                        gemm_tiles.append(GemmTile(
                            layer=op.name,
                            m0=m0, n0=n0, k0=k0,
                            tm=tm, tn=tn, tk=tk,
                            a_base_dram=int(A.addr) + ((m0 * K + k0) * bpe_a),
                            b_base_dram=int(B.addr) + ((k0 * N + n0) * bpe_b),
                            c_base_dram=int(Cc.addr) + ((m0 * N + n0) * bpe_c),
                            meta={
                                "A_K": K,
                                "B_N": N,
                                "bpe_a": bpe_a, "bpe_b": bpe_b, "bpe_c": bpe_c
                            }
                        ))
        else:
            raise NotImplementedError(f"Unsupported op: {op.op_type}")

    return conv_tiles, gemm_tiles


# ============================================================
# ISA compiler (DMA block + implicit CONV/GEMM)
# ============================================================

@dataclass
class BufferState:
    sel: Dict[BufGroup, BufSel] = field(default_factory=lambda: {
        BufGroup.IN:  BufSel.PING,
        BufGroup.WGT: BufSel.PING,
        BufGroup.OUT: BufSel.PING,
    })

    def get(self, g: BufGroup) -> BufSel:
        return self.sel[g]

    def toggle(self, g: BufGroup) -> None:
        self.sel[g] = BufSel.PONG if self.sel[g] == BufSel.PING else BufSel.PING


@dataclass
class EventTracker:
    last_event: Dict[WaitEvent, Optional[int]] = field(default_factory=lambda: {
        WaitEvent.DMA0_DONE: None,
        WaitEvent.DMA1_DONE: None,
        WaitEvent.GEMM_DONE: None,
        WaitEvent.STORE_DONE: None,
        WaitEvent.CONV_DONE: None,
    })

    def set(self, ev: WaitEvent, idx: int) -> None:
        self.last_event[ev] = idx

    def get(self, ev: WaitEvent) -> Optional[int]:
        return self.last_event.get(ev, None)


class Compiler:
    # Toy L1 base addresses
    L1_BASE = {
        (BufGroup.IN,  BufSel.PING): 0x00000000,
        (BufGroup.IN,  BufSel.PONG): 0x00100000,
        (BufGroup.WGT, BufSel.PING): 0x00200000,
        (BufGroup.WGT, BufSel.PONG): 0x00300000,
        (BufGroup.OUT, BufSel.PING): 0x00400000,
        (BufGroup.OUT, BufSel.PONG): 0x00500000,
    }

    ZERO_PAGE = 0x0F000000  # DRAM page of zeros (assumed)

    def __init__(self) -> None:
        self.buf = BufferState()
        self.ev = EventTracker()
        self.prog: List[Instr] = []
        self.last_dma: Dict[int, Optional[int]] = {0: None, 1: None}
        self.last_agu_dma: Optional[int] = None
        self.last_agu_gemm: Optional[int] = None
        self.last_agu_conv: Optional[int] = None
        self.last_compute: Optional[int] = None   # last GEMM/CONV
        self.last_store: Optional[int] = None
        self.dma_desc_prod: Dict[int, int] = {}

    def emit(self, ins: Instr) -> int:
        if len(ins.deps) > MAX_DEPS:
            ins.deps = ins.deps[:MAX_DEPS]
            ins.comment += " [deps truncated]"
        idx = len(self.prog)
        self.prog.append(ins)
        return idx

    def finalize_succ(self) -> None:
        succ = [0] * len(self.prog)
        for ins in self.prog:
            for d in ins.deps:
                if 0 <= d < len(self.prog):
                    succ[d] += 1
        for i, ins in enumerate(self.prog):
            ins.succ_count = succ[i]

    # ----------------- ISA emitters -----------------

    def ins_wait(self, ev: WaitEvent) -> int:
        deps: List[int] = []
        prod = self.ev.get(ev)
        if prod is not None:
            deps.append(prod)
        return self.emit(Instr(op=Op.WAIT, w0=int(ev), deps=deps, comment=f"wait {ev.name}"))

    def ins_buff_switch(self, group: BufGroup) -> int:
        new_sel = BufSel.PONG if self.buf.get(group) == BufSel.PING else BufSel.PING
        deps: List[int] = []
        # conservative barriers
        if group == BufGroup.OUT:
            if self.last_compute is not None:
                deps.append(self.last_compute)
            if self.last_store is not None:
                deps.append(self.last_store)
        else:
            p0 = self.ev.get(WaitEvent.DMA0_DONE)
            p1 = self.ev.get(WaitEvent.DMA1_DONE)
            if p0 is not None:
                deps.append(p0)
            if p1 is not None:
                deps.append(p1)

        idx = self.emit(Instr(
            op=Op.BUFF_SWITCH,
            w0=int(group), w1=int(new_sel),
            deps=deps[:MAX_DEPS],
            comment=f"switch {group.name}->{new_sel.name}"
        ))
        self.buf.toggle(group)
        return idx

    def ins_agu_dma(self, desc_id: int, src_base: int, dst_base: int,
                    row_bytes: int, rows: int, src_stride: int, dst_stride: int) -> int:
        """
        2D DMA descriptor:
          w0 = desc_id[7:0] | rows[19:8] | row_bytes[31:20]
          w1 = src_base
          w2 = dst_base
          w3 = src_stride[15:0] | dst_stride[31:16]
        """
        deps: List[int] = []
        if self.last_agu_dma is not None:
            deps.append(self.last_agu_dma)

        w0 = (desc_id & 0xFF) | ((rows & 0xFFF) << 8) | ((row_bytes & 0xFFF) << 20)
        w3 = (src_stride & 0xFFFF) | ((dst_stride & 0xFFFF) << 16)
        idx = self.emit(Instr(
            op=Op.AGU_DMA,
            w0=w0, w1=src_base, w2=dst_base, w3=w3,
            deps=deps[:MAX_DEPS],
            comment=f"AGU_DMA desc={desc_id} src={src_base:#x} dst={dst_base:#x} rowB={row_bytes} rows={rows} sstr={src_stride} dstr={dst_stride}"
        ))
        self.last_agu_dma = idx
        self.dma_desc_prod[desc_id] = idx
        return idx

    def ins_dma(self, ch: int, direction: DmaDir, group: BufGroup, bufsel: BufSel, desc_id: int, bytes_trace: int) -> int:
        deps: List[int] = []
        if self.last_dma[ch] is not None:
            deps.append(self.last_dma[ch])
        if desc_id not in self.dma_desc_prod:
            raise ValueError(f"DMA uses desc={desc_id} but no AGU_DMA configured")
        deps.append(self.dma_desc_prod[desc_id])

        w0 = (ch & 0x3) | ((int(direction) & 0x1) << 2) | ((int(group) & 0x3) << 3) | ((int(bufsel) & 0x1) << 5) | ((desc_id & 0xFF) << 8)
        idx = self.emit(Instr(
            op=Op.DMA,
            w0=w0, w1=bytes_trace, w2=0, w3=0,
            deps=deps[:MAX_DEPS],
            comment=f"DMA{ch} {direction.name} {group.name}.{bufsel.name} desc={desc_id} bytes={bytes_trace}"
        ))
        self.last_dma[ch] = idx
        self.ev.set(WaitEvent.DMA0_DONE if ch == 0 else WaitEvent.DMA1_DONE, idx)
        return idx

    def ins_agu_gemm(self, a_l1: int, b_l1: int, c_l1: int, tm: int, tn: int, tk: int) -> int:
        deps: List[int] = []
        if self.last_agu_gemm is not None:
            deps.append(self.last_agu_gemm)
        dims = (tm & 0x3FF) | ((tn & 0x3FF) << 10) | ((tk & 0x3FF) << 20)
        idx = self.emit(Instr(
            op=Op.AGU_GEMM,
            w0=a_l1, w1=b_l1, w2=c_l1, w3=dims,
            deps=deps[:MAX_DEPS],
            comment=f"AGU_GEMM A={a_l1:#x} B={b_l1:#x} C={c_l1:#x} dims=({tm},{tn},{tk})"
        ))
        self.last_agu_gemm = idx
        return idx

    def ins_gemm(self, in_sel: BufSel, wgt_sel: BufSel, out_sel: BufSel, accumulate: int) -> int:
        deps: List[int] = []
        if self.last_agu_gemm is not None:
            deps.append(self.last_agu_gemm)
        p0 = self.ev.get(WaitEvent.DMA0_DONE)
        p1 = self.ev.get(WaitEvent.DMA1_DONE)
        if p0 is not None:
            deps.append(p0)
        if p1 is not None:
            deps.append(p1)
        if self.last_compute is not None:
            deps.append(self.last_compute)

        key = (int(BufGroup.IN)&3) | ((int(BufGroup.WGT)&3)<<2) | ((int(BufGroup.OUT)&3)<<4) \
              | ((int(in_sel)&1)<<6) | ((int(wgt_sel)&1)<<7) | ((int(out_sel)&1)<<8) | ((accumulate&1)<<9)

        idx = self.emit(Instr(
            op=Op.GEMM,
            w0=key, w1=0, w2=0, w3=0,
            deps=deps[:MAX_DEPS],
            comment=f"GEMM IN.{in_sel.name} WGT.{wgt_sel.name} OUT.{out_sel.name} acc={accumulate}"
        ))
        self.last_compute = idx
        self.ev.set(WaitEvent.GEMM_DONE, idx)
        return idx

    def ins_agu_conv(self, in_l1: int, w_l1: int, out_l1: int) -> int:
        deps: List[int] = []
        if self.last_agu_conv is not None:
            deps.append(self.last_agu_conv)
        idx = self.emit(Instr(
            op=Op.AGU_CONV,
            w0=in_l1, w1=w_l1, w2=out_l1, w3=0,
            deps=deps[:MAX_DEPS],
            comment=f"AGU_CONV IN={in_l1:#x} W={w_l1:#x} OUT={out_l1:#x}"
        ))
        self.last_agu_conv = idx
        return idx

    def ins_conv(self, C: int, ihT: int, iwT: int, ocT: int,
                 KH: int, KW: int, stride: int, dilation: int,
                 ohT: int, owT: int) -> int:
        """
        CONV params packed across w0-w2:

        w0: [7:0]=ohT [15:8]=owT [23:16]=ocT [31:24]=reserved
        w1: [7:0]=C [19:8]=ihT [31:20]=iwT
        w2: [7:0]=KH [15:8]=KW [23:16]=stride [31:24]=dilation
        """
        deps: List[int] = []
        if self.last_agu_conv is not None:
            deps.append(self.last_agu_conv)
        p0 = self.ev.get(WaitEvent.DMA0_DONE)
        p1 = self.ev.get(WaitEvent.DMA1_DONE)
        if p0 is not None:
            deps.append(p0)
        if p1 is not None:
            deps.append(p1)
        if self.last_compute is not None:
            deps.append(self.last_compute)

        w0 = (ohT & 0xFF) | ((owT & 0xFF) << 8) | ((ocT & 0xFF) << 16)
        w1 = (C & 0xFF) | ((ihT & 0xFFF) << 8) | ((iwT & 0xFFF) << 20)
        w2 = (KH & 0xFF) | ((KW & 0xFF) << 8) | ((stride & 0xFF) << 16) | ((dilation & 0xFF) << 24)

        idx = self.emit(Instr(
            op=Op.CONV,
            w0=w0, w1=w1, w2=w2, w3=0,
            deps=deps[:MAX_DEPS],
            comment=f"CONV ohT={ohT} owT={owT} ocT={ocT} ihT={ihT} iwT={iwT} C={C} KH={KH} KW={KW} stride={stride} dil={dilation}"
        ))
        self.last_compute = idx
        self.ev.set(WaitEvent.CONV_DONE, idx)
        return idx

    def ins_store(self, out_sel: BufSel, dst_addr: int, bytes_: int) -> int:
        deps: List[int] = []
        # store depends on latest compute (GEMM or CONV)
        if self.last_compute is not None:
            deps.append(self.last_compute)
        if self.last_store is not None:
            deps.append(self.last_store)
        key = (int(out_sel) & 1)
        idx = self.emit(Instr(
            op=Op.STORE,
            w0=key, w1=dst_addr, w2=bytes_, w3=0,
            deps=deps[:MAX_DEPS],
            comment=f"STORE OUT.{out_sel.name} -> {dst_addr:#x} bytes={bytes_}"
        ))
        self.last_store = idx
        self.ev.set(WaitEvent.STORE_DONE, idx)
        return idx

    # ----------------- Address helpers -----------------

    @staticmethod
    def x_addr_nchw(x_base: int, C: int, H: int, W: int, c: int, h: int, w: int, bpe: int) -> int:
        return x_base + (((c * H + h) * W + w) * bpe)

    @staticmethod
    def w_addr_ochw(w_base: int, OC: int, C: int, KH: int, KW: int, oc: int, c: int, kh: int, kw: int, bpe: int) -> int:
        # W[oc, c, kh, kw] contiguous in last dimension
        idx = (((oc * C + c) * KH + kh) * KW + kw)
        return w_base + idx * bpe

    # ----------------- Conv tile emission -----------------

    def emit_conv_tile(self, t: ConvTile, desc_in: int, desc_w: int) -> None:
        """
        Conv tile flow (implicit im2col in CONV):
        DMA0: load input tile (C x ihT x iwT) into L1 IN (first zero-fill, then copy valid region)
        DMA1: load weight tile for oc range into L1 WGT (pack as [ocT, C, KH, KW] contiguous)
        AGU_CONV + CONV: compute output tile (ohT x owT x ocT) into L1 OUT
        STORE: write output tile to DRAM Ycol [M,OC]
        """
        m = t.meta
        C = m["C"]; H = m["H"]; W = m["W"]
        OC = m["OC"]; OH = m["OH"]; OW = m["OW"]
        KH = m["KH"]; KW = m["KW"]
        stride = m["stride"]; pad = m["pad"]; dil = m["dilation"]
        bpe_x = m["bpe_x"]; bpe_w = m["bpe_w"]; bpe_y = m["bpe_y"]

        in_sel  = self.buf.get(BufGroup.IN)
        w_sel   = self.buf.get(BufGroup.WGT)
        out_sel = self.buf.get(BufGroup.OUT)

        in_l1  = self.L1_BASE[(BufGroup.IN, in_sel)]
        w_l1   = self.L1_BASE[(BufGroup.WGT, w_sel)]
        out_l1 = self.L1_BASE[(BufGroup.OUT, out_sel)]

        # -------- input tile geometry (with halo) --------
        # input top-left (in original X coordinates)
        ih0 = t.oh0 * stride - pad
        iw0 = t.ow0 * stride - pad
        # tile input extent needed to compute ohT x owT outputs
        # ihT = (ohT-1)*stride + (KH-1)*dil + 1
        # iwT = (owT-1)*stride + (KW-1)*dil + 1
        ihT = (t.ohT - 1) * stride + (KH - 1) * dil + 1
        iwT = (t.owT - 1) * stride + (KW - 1) * dil + 1

        # First: zero-fill entire L1 IN tile buffer for each channel (block DMA from ZERO_PAGE)
        for c in range(C):
            dst_c = in_l1 + (c * ihT * iwT * bpe_x)
            self.ins_agu_dma(
                desc_id=desc_in,
                src_base=self.ZERO_PAGE,
                dst_base=dst_c,
                row_bytes=iwT * bpe_x,
                rows=ihT,
                src_stride=0,
                dst_stride=iwT * bpe_x
            )
            self.ins_dma(ch=0, direction=DmaDir.H2L1, group=BufGroup.IN, bufsel=in_sel,
                         desc_id=desc_in, bytes_trace=ihT * iwT * bpe_x)

        # Then: copy valid sub-rectangle into the correct offset inside the tile buffer
        v_h0 = max(0, ih0)
        v_w0 = max(0, iw0)
        v_h1 = min(H, ih0 + ihT)
        v_w1 = min(W, iw0 + iwT)
        valid_h = max(0, v_h1 - v_h0)
        valid_w = max(0, v_w1 - v_w0)

        # offsets within tile buffer where valid region starts
        dst_off_h = v_h0 - ih0
        dst_off_w = v_w0 - iw0

        if valid_h > 0 and valid_w > 0:
            for c in range(C):
                src = self.x_addr_nchw(t.x_base_dram, C, H, W, c, v_h0, v_w0, bpe_x)
                dst = in_l1 + (c * ihT * iwT * bpe_x) + ((dst_off_h * iwT + dst_off_w) * bpe_x)
                self.ins_agu_dma(
                    desc_id=desc_in,
                    src_base=src,
                    dst_base=dst,
                    row_bytes=valid_w * bpe_x,
                    rows=valid_h,
                    src_stride=W * bpe_x,
                    dst_stride=iwT * bpe_x
                )
                self.ins_dma(ch=0, direction=DmaDir.H2L1, group=BufGroup.IN, bufsel=in_sel,
                             desc_id=desc_in, bytes_trace=valid_h * valid_w * bpe_x)

        self.ins_wait(WaitEvent.DMA0_DONE)

        # -------- weight tile DMA (pack ocT blocks into L1 WGT) --------
        # L1 pack layout: contiguous [ocT][C][KH][KW]
        K_total = C * KH * KW
        for ioc in range(t.ocT):
            oc = t.oc0 + ioc
            src_w = self.w_addr_ochw(t.w_base_dram, OC, C, KH, KW, oc, 0, 0, 0, bpe_w)
            dst_w = w_l1 + (ioc * K_total * bpe_w)
            bytes_w = K_total * bpe_w
            self.ins_agu_dma(desc_id=desc_w, src_base=src_w, dst_base=dst_w,
                             row_bytes=bytes_w, rows=1, src_stride=0, dst_stride=0)
            self.ins_dma(ch=1, direction=DmaDir.H2L1, group=BufGroup.WGT, bufsel=w_sel,
                         desc_id=desc_w, bytes_trace=bytes_w)

        self.ins_wait(WaitEvent.DMA1_DONE)

        # -------- CONV compute (implicit window gather inside CONV unit) --------
        # Since L1 IN already includes padding (zeros), CONV can treat pad as 0 within L1 tile.
        self.ins_agu_conv(in_l1, w_l1, out_l1)
        self.ins_conv(C=C, ihT=ihT, iwT=iwT, ocT=t.ocT,
                      KH=KH, KW=KW, stride=stride, dilation=dil,
                      ohT=t.ohT, owT=t.owT)

        self.ins_wait(WaitEvent.CONV_DONE)

        # -------- STORE output tile to DRAM Ycol [M,OC] --------
        # Output tile in Ycol:
        #   m = oh*OW + ow
        # For a rectangular tile, rows correspond to raster order within the tile:
        #   row r = (local_oh * owT + local_ow)
        # We store as a contiguous block of size (ohT*owT) x ocT.
        M = OH * OW
        assert M > 0

        # Ycol base index for (oh0,ow0,oc0):
        m0 = t.oh0 * OW + t.ow0
        dst = t.y_base_dram + ((m0 * OC + t.oc0) * bpe_y)

        bytes_out = (t.ohT * t.owT) * t.ocT * bpe_y
        self.ins_store(out_sel, dst, bytes_out)
        self.ins_wait(WaitEvent.STORE_DONE)

    # ----------------- GEMM tile emission (Linear) -----------------

    def emit_gemm_tile(self, t: GemmTile, desc_a: int, desc_b: int) -> None:
        in_sel  = self.buf.get(BufGroup.IN)
        w_sel   = self.buf.get(BufGroup.WGT)
        out_sel = self.buf.get(BufGroup.OUT)

        a_l1  = self.L1_BASE[(BufGroup.IN, in_sel)]
        b_l1  = self.L1_BASE[(BufGroup.WGT, w_sel)]
        c_l1  = self.L1_BASE[(BufGroup.OUT, out_sel)]

        A_K = t.meta["A_K"]
        B_N = t.meta["B_N"]
        bpe_a = t.meta["bpe_a"]
        bpe_b = t.meta["bpe_b"]
        bpe_c = t.meta["bpe_c"]

        # DMA A tile [tm,tk]
        self.ins_agu_dma(desc_id=desc_a, src_base=t.a_base_dram, dst_base=a_l1,
                         row_bytes=t.tk * bpe_a, rows=t.tm,
                         src_stride=A_K * bpe_a, dst_stride=t.tk * bpe_a)
        self.ins_dma(ch=0, direction=DmaDir.H2L1, group=BufGroup.IN, bufsel=in_sel,
                     desc_id=desc_a, bytes_trace=t.tm * t.tk * bpe_a)

        # DMA B tile [tk,tn]
        self.ins_agu_dma(desc_id=desc_b, src_base=t.b_base_dram, dst_base=b_l1,
                         row_bytes=t.tn * bpe_b, rows=t.tk,
                         src_stride=B_N * bpe_b, dst_stride=t.tn * bpe_b)
        self.ins_dma(ch=1, direction=DmaDir.H2L1, group=BufGroup.WGT, bufsel=w_sel,
                     desc_id=desc_b, bytes_trace=t.tk * t.tn * bpe_b)

        self.ins_wait(WaitEvent.DMA0_DONE)
        self.ins_wait(WaitEvent.DMA1_DONE)

        self.ins_agu_gemm(a_l1, b_l1, c_l1, t.tm, t.tn, t.tk)
        acc = 1 if t.k0 != 0 else 0
        self.ins_gemm(in_sel, w_sel, out_sel, accumulate=acc)
        self.ins_wait(WaitEvent.GEMM_DONE)

        # Store only if last k-slice for this (m0,n0)
        self.ins_store(out_sel, t.c_base_dram, t.tm * t.tn * bpe_c)
        self.ins_wait(WaitEvent.STORE_DONE)

    # ----------------- Full compile -----------------

    def compile(self, conv_tiles: List[ConvTile], gemm_tiles: List[GemmTile], double_buffer: bool = True) -> List[Instr]:
        # descriptor IDs
        DESC_IN = 0
        DESC_W  = 1
        DESC_A  = 2
        DESC_B  = 3

        # Conv tiles first, then GEMM tiles (assumes net order conv->linear)
        for t in conv_tiles:
            self.emit_conv_tile(t, desc_in=DESC_IN, desc_w=DESC_W)
            if double_buffer:
                self.ins_buff_switch(BufGroup.OUT)
                self.ins_buff_switch(BufGroup.IN)
                self.ins_buff_switch(BufGroup.WGT)

        for t in gemm_tiles:
            self.emit_gemm_tile(t, desc_a=DESC_A, desc_b=DESC_B)
            if double_buffer:
                self.ins_buff_switch(BufGroup.OUT)
                self.ins_buff_switch(BufGroup.IN)
                self.ins_buff_switch(BufGroup.WGT)

        self.emit(Instr(op=Op.END, comment="program end"))
        self.finalize_succ()
        return self.prog


# ============================================================
# Top-level pipeline
# ============================================================

def compile_network(
    net: Network,
    outdir: str,
    bin_name: str,
    disasm_name: str,
    dump_json: str,
    mem_base: int,
    mem_align: int,
    tile_oh: int,
    tile_ow: int,
    tile_oc: int,
    tile_m: int,
    tile_n: int,
    tile_k: int,
) -> None:
    os.makedirs(outdir, exist_ok=True)

    MemoryPlanner(base=mem_base, align=mem_align).plan(net)

    conv_tiles, gemm_tiles = lower_network(
        net=net,
        tile_oh=tile_oh, tile_ow=tile_ow, tile_oc=tile_oc,
        tile_m=tile_m, tile_n=tile_n, tile_k=tile_k
    )

    comp = Compiler()
    prog = comp.compile(conv_tiles, gemm_tiles, double_buffer=True)

    bin_path = os.path.join(outdir, bin_name)
    dis_path = os.path.join(outdir, disasm_name)
    write_bin(bin_path, prog)
    write_disasm(dis_path, prog)

    if dump_json:
        obj = {
            "tensors": [
                {"name": t.name, "shape": list(t.shape), "dtype": t.dtype, "addr": t.addr, "bytes": t.bytes}
                for t in net.tensors.values()
            ],
            "ops": [
                {"op_type": op.op_type, "name": op.name, "inputs": op.inputs, "outputs": op.outputs, "attrs": op.attrs}
                for op in net.ops
            ],
            "conv_tiles": [
                {
                    "layer": tt.layer, "oh0": tt.oh0, "ow0": tt.ow0, "ohT": tt.ohT, "owT": tt.owT,
                    "oc0": tt.oc0, "ocT": tt.ocT,
                    "x_base_dram": tt.x_base_dram, "w_base_dram": tt.w_base_dram, "y_base_dram": tt.y_base_dram,
                    "meta": tt.meta
                } for tt in conv_tiles
            ],
            "gemm_tiles": [
                {
                    "layer": tt.layer, "m0": tt.m0, "n0": tt.n0, "k0": tt.k0,
                    "tm": tt.tm, "tn": tt.tn, "tk": tt.tk,
                    "a_base_dram": tt.a_base_dram, "b_base_dram": tt.b_base_dram, "c_base_dram": tt.c_base_dram,
                    "meta": tt.meta
                } for tt in gemm_tiles
            ],
            "program": [
                {
                    "idx": i,
                    "op": ins.op.name,
                    "w0": ins.w0, "w1": ins.w1, "w2": ins.w2, "w3": ins.w3,
                    "deps": ins.deps, "succ": ins.succ_count,
                    "comment": ins.comment
                } for i, ins in enumerate(prog)
            ]
        }
        json_path = os.path.join(outdir, dump_json)
        with open(json_path, "w", encoding="utf-8") as f:
            json.dump(obj, f, indent=2, ensure_ascii=False)

    print(f"[OK] wrote {bin_path}")
    print(f"[OK] wrote {dis_path}")
    if dump_json:
        print(f"[OK] wrote {os.path.join(outdir, dump_json)}")


# ============================================================
# CLI
# ============================================================

def build_argparser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description="Implicit-Conv + Linear NPU compiler (bin+disasm)")
    p.add_argument("--net-json", type=str, required=True)
    p.add_argument("--outdir", type=str, default="out")
    p.add_argument("--bin", type=str, default="program.bin")
    p.add_argument("--disasm", type=str, default="program.disasm")
    p.add_argument("--dump-json", type=str, default="program_debug.json")
    p.add_argument("--mem-base", type=str, default="0x10000000")
    p.add_argument("--mem-align", type=int, default=256)

    # Conv tiling
    p.add_argument("--tile-oh", type=int, default=4)
    p.add_argument("--tile-ow", type=int, default=4)
    p.add_argument("--tile-oc", type=int, default=16)

    # GEMM tiling (Linear)
    p.add_argument("--tile-m", type=int, default=256)
    p.add_argument("--tile-n", type=int, default=256)
    p.add_argument("--tile-k", type=int, default=256)

    return p


def main() -> None:
    args = build_argparser().parse_args()
    net = load_network_from_json(args.net_json)

    compile_network(
        net=net,
        outdir=args.outdir,
        bin_name=args.bin,
        disasm_name=args.disasm,
        dump_json=args.dump_json,
        mem_base=int(args.mem_base, 0),
        mem_align=args.mem_align,
        tile_oh=args.tile_oh,
        tile_ow=args.tile_ow,
        tile_oc=args.tile_oc,
        tile_m=args.tile_m,
        tile_n=args.tile_n,
        tile_k=args.tile_k
    )


if __name__ == "__main__":
    main()
