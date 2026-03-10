import json
from typing import Dict, Any

# ====================== 指令类型与OP_DEC映射（严格匹配你的TTU_ISA） ======================
OP_DEC_MAP = {
    "AGU_DMA": 0x01,
    "AGU_COMPUTE_IFM": 0x02,
    "AGU_COMPUTE_KER": 0xFE,
    "DMA_AGU": 0x03,
    "DMA_NORMAL": 0x04,
    "STORE": 0x05,
    "BUFF_SW": 0x06,
    "WAIT": 0x07,
    "CONV_TILE": 0x81,
    "VINT": 0x82,
    "VFP": 0x83
}

# ====================== 字段位宽定义（匹配SpinalHDL位域） ======================
FIELD_BIT_WIDTH = {
    # 基础字段
    "OP_DEC": 8,
    "DEP_CNT": 8,
    "SUCC_CNT": 8,
    "RESERVE_BYTE": 8,
    "DEP_ID_0": 16,
    "DEP_ID_1": 16,
    "DEP_ID_2": 16,
    "DEP_ID_3": 16,
    # WORD系列（32bit）
    "WORD_0": 32,
    "WORD_1": 32,
    "WORD_2": 32,
    "WORD_3": 32,
    "WORD_4": 32
}

# ====================== 子字段位域映射（针对复杂WORD拆分） ======================
SUB_FIELD_MAP = {
    # AGU_COMPUTE_IFM 子字段（严格匹配你的SpinalHDL代码）
    "AGU_COMPUTE_IFM": {
        # WORD_0 → BASE（直接复用整个WORD_0）
        "BASE": ("WORD_0", 0, 31, 32),
        # WORD_1拆分（issue_flow.WORD_1的位域）
        "ELEM_BYTES": ("WORD_1", 0, 2, 3),        # WORD_1[2:0]
        "LAYOUT_TYPE": ("WORD_1", 3, 4, 2),       # WORD_1[4:3]
        "H_IN": ("WORD_1", 5, 16, 12),            # WORD_1[16:5]
        "W_IN": ("WORD_1", 17, 28, 12),           # WORD_1[28:17]
        "C_IN_LOW": ("WORD_1", 29, 31, 3),        # WORD_1[31:29] → C_IN[2:0]
        # WORD_2拆分（issue_flow.WORD_2的位域）
        "C_IN_HIGH": ("WORD_2", 0, 8, 9),         # WORD_2[8:0] → C_IN[11:3]
        "K_H": ("WORD_2", 9, 12, 4),              # WORD_2[12:9]
        "K_W": ("WORD_2", 13, 16, 4),             # WORD_2[16:13]
        "STRIDE_H": ("WORD_2", 17, 19, 3),        # WORD_2[19:17]
        "STRIDE_W": ("WORD_2", 20, 22, 3),        # WORD_2[22:20]
        "DILATION_H": ("WORD_2", 23, 26, 4),      # WORD_2[26:23]
        "DILATION_W": ("WORD_2", 27, 30, 4),      # WORD_2[30:27]
        # WORD_3拆分（issue_flow.WORD_3的位域）
        "PAD_TOP": ("WORD_3", 0, 5, 6),           # WORD_3[5:0]
        "PAD_LEFT": ("WORD_3", 6, 11, 6),         # WORD_3[11:6]
        "TILE_H0": ("WORD_3", 12, 23, 12),        # WORD_3[23:12]
        "TILE_W0_LOW": ("WORD_3", 24, 31, 8),     # WORD_3[31:24] → TILE_W0[7:0]
        # WORD_4拆分（issue_flow.WORD_4的位域）
        "TILE_W0_HIGH": ("WORD_4", 0, 3, 4),      # WORD_4[3:0] → TILE_W0[11:8]
        "TILE_C0": ("WORD_4", 4, 15, 12)          # WORD_4[15:4]
    },
    # AGU_COMPUTE_KER 子字段（和IFM完全一致，复用相同位域）
    "AGU_COMPUTE_KER": {
        "BASE": ("WORD_0", 0, 31, 32),
        "ELEM_BYTES": ("WORD_1", 0, 2, 3),
        "LAYOUT_TYPE": ("WORD_1", 3, 4, 2),
        "H_IN": ("WORD_1", 5, 16, 12),
        "W_IN": ("WORD_1", 17, 28, 12),
        "C_IN_LOW": ("WORD_1", 29, 31, 3),
        "C_IN_HIGH": ("WORD_2", 0, 8, 9),
        "K_H": ("WORD_2", 9, 12, 4),
        "K_W": ("WORD_2", 13, 16, 4),
        "STRIDE_H": ("WORD_2", 17, 19, 3),
        "STRIDE_W": ("WORD_2", 20, 22, 3),
        "DILATION_H": ("WORD_2", 23, 26, 4),
        "DILATION_W": ("WORD_2", 27, 30, 4),
        "PAD_TOP": ("WORD_3", 0, 5, 6),
        "PAD_LEFT": ("WORD_3", 6, 11, 6),
        #"TILE_H0": ("WORD_3", 12, 23, 12),
        "C_OUT": ("WORD_3", 12, 23, 12),
        "TILE_W0_LOW": ("WORD_3", 24, 31, 8),
        "TILE_W0_HIGH": ("WORD_4", 0, 3, 4),
        "TILE_C0": ("WORD_4", 4, 15, 12),
        "OC_TILE": ("WORD_4", 16, 27, 12),
    },
    # STORE 子字段（严格匹配你的SpinalHDL代码）
    "STORE": {
        # WORD_0 → BASE（直接复用整个WORD_0）
        "BASE": ("WORD_0", 0, 31, 32),
        # WORD_1拆分（issue_flow.WORD_1的位域）
        "ELEM_BYTES": ("WORD_1", 0, 2, 3),        # WORD_1[2:0]
        "LAYOUT_TYPE": ("WORD_1", 3, 4, 2),       # WORD_1[4:3]
        "H_OUT": ("WORD_1", 5, 16, 12),            # WORD_1[16:5]
        "W_OUT": ("WORD_1", 17, 28, 12),           # WORD_1[28:17]
        "C_OUT_LOW": ("WORD_1", 29, 31, 3),        # WORD_1[31:29] → C_IN[2:0]
        # WORD_2拆分（issue_flow.WORD_2的位域）
        "C_OUT_HIGH": ("WORD_2", 0, 8, 9),         # WORD_2[8:0] → C_IN[11:3]
        "OH0": ("WORD_3", 12, 23, 12),        # WORD_3[23:12]
        "OW0_LOW": ("WORD_3", 24, 31, 8),     # WORD_3[31:24] → TILE_W0[7:0]
        # WORD_4拆分（issue_flow.WORD_4的位域）
        "OW0_HIGH": ("WORD_4", 0, 3, 4),      # WORD_4[3:0] → TILE_W0[11:8]
        "OC0": ("WORD_4", 4, 15, 12)          # WORD_4[15:4]
    },
    # DMA_NORMAL 子字段
    "DMA_NORMAL": {
        "isLoad": ("WORD_0", 31, 31, 1),          # WORD_0[31]
        "extAddr": ("WORD_0", 0, 30, 31),         # WORD_0[30:0]
        "l1Addr": ("WORD_1", 0, 31, 32),          # WORD_1[31:0]
        "bytes": ("WORD_2", 0, 31, 32),           # WORD_2[31:0]
        "trans_len": ("WORD_3", 0, 7, 8)          # WORD_3[7:0]
    },
    # CONV_TILE 子字段
    "CONV_TILE": {
        "TILE_OH0": ("WORD_0", 0, 11, 12),        # WORD_0[11:0]
        "TILE_OW0": ("WORD_0", 12, 23, 12),       # WORD_0[23:12]
        "TILE_OC0": ("WORD_1", 0, 11, 12),        # WORD_1[11:0]
        "OH_TILE": ("WORD_1", 12, 23, 12),        # WORD_1[23:12]
        "OW_TILE": ("WORD_2", 0, 11, 12),         # WORD_2[11:0]
        "OC_TILE": ("WORD_2", 12, 23, 12),        # WORD_2[23:12]
        "ORDER": ("WORD_2", 24, 25, 2),           # WORD_2[25:24]
        "K_TILE": ("WORD_3", 0, 11, 12),          # WORD_3[11:0]
        "M_TILE": ("WORD_3", 12, 23, 12),         # WORD_3[23:12]
        "N_TILE": ("WORD_4", 0, 11, 12)           # WORD_4[11:0]
    }
}

# ====================== 新增：自动转换十六进制/十进制为整数 ======================
def parse_value(value: Any) -> int:
    """
    解析配置值：支持十进制整数、十六进制字符串（0x前缀）
    :param value: 配置值（如16、"0x10"、"16"）
    :return: 转换后的整数
    """
    if isinstance(value, int):
        return value
    elif isinstance(value, str):
        # 处理十六进制字符串（0x开头）
        if value.startswith(("0x", "0X")):
            return int(value, 16)
        # 处理十进制字符串
        else:
            return int(value)
    else:
        raise ValueError(f"不支持的数值类型：{type(value)}，值：{value}")

def check_field_value(value: int, field_name: str) -> None:
    """校验字段值是否超出位宽范围"""
    if field_name not in FIELD_BIT_WIDTH:
        return  # 子字段不单独校验，父字段校验即可
    max_val = (1 << FIELD_BIT_WIDTH[field_name]) - 1
    if value < 0 or value > max_val:
        raise ValueError(
            f"字段{field_name}值{value}(0x{value:X})超出{FIELD_BIT_WIDTH[field_name]}bit范围（0~{max_val}(0x{max_val:X})）"
        )

def assemble_word(field_config: Dict[str, Any], word_name: str) -> int:
    """根据子字段配置组装WORD（如WORD_1）"""
    word_val = 0
    # 遍历所有子字段，找到属于当前WORD的
    for sub_field, (parent_word, start, end, width) in SUB_FIELD_MAP.get(field_config["type"], {}).items():
        if parent_word != word_name:
            continue
        # 子字段值，默认0
        sub_val = parse_value(field_config.get(sub_field, 0))
        # 校验子字段位宽
        max_sub_val = (1 << width) - 1
        if sub_val < 0 or sub_val > max_sub_val:
            raise ValueError(
                f"子字段{sub_field}值{sub_val}(0x{sub_val:X})超出{width}bit范围（0~{max_sub_val}(0x{max_sub_val:X})）"
            )
        # 拼接到位域
        word_val |= (sub_val & max_sub_val) << start
    # 如果直接配置了WORD本身，覆盖子字段（优先级更高）
    if word_name in field_config:
        word_val = parse_value(field_config[word_name])
    # 校验WORD总位宽
    check_field_value(word_val, word_name)
    return word_val

def generate_instruction(cmd_config: Dict[str, Any]) -> str:
    """
    生成单条256bit指令的十六进制字符串
    :param cmd_config: 单条指令的JSON配置
    :return: 64位十六进制字符串（大写，无0x前缀）
    """
    # 1. 基础字段初始化（默认0）
    op_dec = OP_DEC_MAP[cmd_config["type"]]
    dep_cnt = parse_value(cmd_config.get("DEP_CNT", 0))
    succ_cnt = parse_value(cmd_config.get("SUCC_CNT", 0))
    reserve_byte = parse_value(cmd_config.get("RESERVE_BYTE", 0))
    dep_id_0 = parse_value(cmd_config.get("DEP_ID_0", "0xFFFF"))
    dep_id_1 = parse_value(cmd_config.get("DEP_ID_1", "0xFFFF"))
    dep_id_2 = parse_value(cmd_config.get("DEP_ID_2", "0xFFFF"))
    dep_id_3 = parse_value(cmd_config.get("DEP_ID_3", "0xFFFF"))

    # 2. 校验基础字段
    check_field_value(op_dec, "OP_DEC")
    check_field_value(dep_cnt, "DEP_CNT")
    check_field_value(succ_cnt, "SUCC_CNT")
    check_field_value(reserve_byte, "RESERVE_BYTE")
    check_field_value(dep_id_0, "DEP_ID_0")
    check_field_value(dep_id_1, "DEP_ID_1")
    check_field_value(dep_id_2, "DEP_ID_2")
    check_field_value(dep_id_3, "DEP_ID_3")

    # 3. 组装WORD系列（支持子字段拆分）
    word_0 = assemble_word(cmd_config, "WORD_0")
    word_1 = assemble_word(cmd_config, "WORD_1")
    word_2 = assemble_word(cmd_config, "WORD_2")
    word_3 = assemble_word(cmd_config, "WORD_3")
    word_4 = assemble_word(cmd_config, "WORD_4")

    # 4. 按位域拼接256bit指令（严格匹配SpinalHDL定义）
    instr = 0
    # [7:0] OP_DEC
    instr |= (op_dec & 0xFF) << 0
    # [15:8] DEP_CNT
    instr |= (dep_cnt & 0xFF) << 8
    # [23:16] SUCC_CNT
    instr |= (succ_cnt & 0xFF) << 16
    # [31:24] RESERVE_BYTE
    instr |= (reserve_byte & 0xFF) << 24
    # [47:32] DEP_ID_0
    instr |= (dep_id_0 & 0xFFFF) << 32
    # [63:48] DEP_ID_1
    instr |= (dep_id_1 & 0xFFFF) << 48
    # [79:64] DEP_ID_2
    instr |= (dep_id_2 & 0xFFFF) << 64
    # [95:80] DEP_ID_3
    instr |= (dep_id_3 & 0xFFFF) << 80
    # [127:96] WORD_0
    instr |= (word_0 & 0xFFFFFFFF) << 96
    # [159:128] WORD_1
    instr |= (word_1 & 0xFFFFFFFF) << 128
    # [191:160] WORD_2
    instr |= (word_2 & 0xFFFFFFFF) << 160
    # [223:192] WORD_3
    instr |= (word_3 & 0xFFFFFFFF) << 192
    # [255:224] WORD_4
    instr |= (word_4 & 0xFFFFFFFF) << 224

    # 5. 转换为256bit无符号数，输出64位十六进制字符串
    instr_256bit = instr & ((1 << 256) - 1)
    hex_str = f"{instr_256bit:064X}"
    return hex_str

def generate_instruction_stream(json_path: str, output_path: str) -> None:
    """
    从JSON文件生成指令流
    :param json_path: 输入JSON配置文件路径
    :param output_path: 输出十六进制指令流文件路径
    """
    # 1. 读取JSON配置
    with open(json_path, "r", encoding="utf-8") as f:
        config = json.load(f)
    
    # 2. 生成每条指令
    instr_hex_list = []
    for idx, cmd in enumerate(config.get("instruction_stream", [])):
        try:
            hex_str = generate_instruction(cmd)
            instr_hex_list.append(hex_str)
            print(f"✅ 生成指令{idx+1}（{cmd['type']}）：{hex_str}")
        except Exception as e:
            raise RuntimeError(f"生成指令{idx+1}失败：{e}")
    
    # 3. 写入输出文件
    with open(output_path, "w", encoding="utf-8") as f:
        f.write("\n".join(instr_hex_list))

    # ====================== 新增：打印cmd格式的指令 ======================
    print("\n" + "="*80)
    print("📌 指令变量格式（可直接复制到硬件测试代码）：")
    print("="*80)
    for idx, hex_str in enumerate(instr_hex_list):
        # 转换为小写（硬件常用），添加0x前缀
        cmd_var = f"cmd{idx} = [0x{hex_str.lower()}]"
        print(cmd_var)
    
    print(f"\n🎉 指令流生成完成！输出文件：{output_path}")
    print(f"📝 共生成{len(instr_hex_list)}条指令")
    
    print(f"\n🎉 指令流生成完成！输出文件：{output_path}")
    print(f"📝 共生成{len(instr_hex_list)}条指令")

# ====================== 示例配置与测试 ======================
if __name__ == "__main__":
    # 示例JSON配置（支持十六进制）
    sample_config = {
        "instruction_stream": [
            # 示例1：AGU_COMPUTE_IFM（混合十六进制/十进制配置）
            {
                "type": "AGU_COMPUTE_IFM",
                "DEP_CNT": "0x01",       # 十六进制字符串
                "SUCC_CNT": 2,           # 十进制整数
                # WORD_0（BASE）
                "BASE": "0x80000000",    # 十六进制
                # WORD_1子字段
                "ELEM_BYTES": "0x1",
                "LAYOUT_TYPE": 0,
                "H_IN": "0x10",
                "W_IN": 16,
                "C_IN_LOW": "0x7",
                # WORD_2子字段
                "C_IN_HIGH": 0,
                "K_H": "0x3",
                "K_W": "0x3",
                "STRIDE_H": "0x1",
                "STRIDE_W": 1,
                # WORD_3子字段
                "PAD_TOP": "0x1",
                "PAD_LEFT": 1,
                "TILE_H0": "0x8",
                "TILE_W0_LOW": 8,
                # WORD_4子字段
                "TILE_W0_HIGH": 0,
                "TILE_C0": "0x10"
            },
            # 示例2：DMA_NORMAL（全十六进制配置）
            {
                "type": "DMA_NORMAL",
                "RESERVE_BYTE": "0xAA",
                # 直接配置WORD（优先级高于子字段）
                "WORD_0": "0x80001234",  # isLoad=1, extAddr=0x00001234
                "l1Addr": "0x10000000",
                "bytes": "0x00000200",
                "trans_len": "0x08"
            }
        ]
    }

    # 保存示例配置到文件
    # with open("instr_config.json", "w", encoding="utf-8") as f:
    #     json.dump(sample_config, f, indent=4, ensure_ascii=False)
    
    # 生成指令流
    generate_instruction_stream("instr_config.json", "instr_stream.hex")