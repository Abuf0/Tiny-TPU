def generate_256bit_cmd_code(
    op_dec: int,
    dep_cnt: int,
    succ_cnt: int,
    dep_id_0: int = 0,
    dep_id_1: int = 0,
    dep_id_2: int = 0,
    dep_id_3: int = 0,
    word_0: int = 0,
    word_1: int = 0,
    word_2: int = 0,
    word_3: int = 0,
    padding: int = 0
) -> str:
    """
    生成256bit的cmd code（匹配SpinalHDL位域定义）
    :param op_dec: [7:0] 操作码（8bit）
    :param dep_cnt: [15:8] 依赖计数（8bit）
    :param succ_cnt: [23:16] 后继计数（8bit）
    :param dep_id_0: [47:32] 依赖ID0（16bit）
    :param dep_id_1: [63:48] 依赖ID1（16bit）
    :param dep_id_2: [79:64] 依赖ID2（16bit）
    :param dep_id_3: [95:80] 依赖ID3（16bit）
    :param word_0: [127:96] 数据字0（32bit）
    :param word_1: [159:128] 数据字1（32bit）
    :param word_2: [191:160] 数据字2（32bit）
    :param word_3: [191:160] 数据字3（32bit）
    :param padding: [231:224] 填充位（8bit）
    :return: 256bit cmd code的16进制字符串（64位16进制）
    """
    # ========== 1. 位宽校验（避免数据溢出） ==========
    def check_bit_width(val: int, name: str, bits: int):
        max_val = (1 << bits) - 1
        if val < 0 or val > max_val:
            raise ValueError(f"{name} 超出 {bits}bit 范围（0~{max_val}），当前值：{val}")

    check_bit_width(op_dec, "OP_DEC", 8)
    check_bit_width(dep_cnt, "DEP_CNT", 8)
    check_bit_width(succ_cnt, "SUCC_CNT", 8)
    check_bit_width(dep_id_0, "DEP_ID_0", 16)
    check_bit_width(dep_id_1, "DEP_ID_1", 16)
    check_bit_width(dep_id_2, "DEP_ID_2", 16)
    check_bit_width(dep_id_3, "DEP_ID_3", 16)
    check_bit_width(word_0, "WORD_0", 32)
    check_bit_width(word_1, "WORD_1", 32)
    check_bit_width(word_2, "WORD_2", 32)
    check_bit_width(word_3, "WORD_3", 32)
    check_bit_width(padding, "PADDING", 8)

    # ========== 2. 按位域拼接256bit数据（未提及的bit自动置0） ==========
    cmd_code = 0

    # [7:0] OP_DEC
    cmd_code |= (op_dec & 0xFF) << 0

    # [15:8] DEP_CNT
    cmd_code |= (dep_cnt & 0xFF) << 8

    # [23:16] SUCC_CNT
    cmd_code |= (succ_cnt & 0xFF) << 16

    # [47:32] DEP_ID_0（16bit）
    cmd_code |= (dep_id_0 & 0xFFFF) << 32

    # [63:48] DEP_ID_1（16bit）
    cmd_code |= (dep_id_1 & 0xFFFF) << 48

    # [79:64] DEP_ID_2（16bit）
    cmd_code |= (dep_id_2 & 0xFFFF) << 64

    # [95:80] DEP_ID_3（16bit）
    cmd_code |= (dep_id_3 & 0xFFFF) << 80

    # [127:96] WORD_0（32bit）
    cmd_code |= (word_0 & 0xFFFFFFFF) << 96

    # [159:128] WORD_1（32bit）
    cmd_code |= (word_1 & 0xFFFFFFFF) << 128

    # [191:160] WORD_2（32bit）
    cmd_code |= (word_2 & 0xFFFFFFFF) << 160

    # [223:192] WORD_3（32bit）
    cmd_code |= (word_3 & 0xFFFFFFFF) << 192

    # [231:224] PADDING（8bit）
    cmd_code |= (padding & 0xFF) << 224

    # ========== 3. 转换为256bit 16进制字符串（补前导0到64位） ==========
    # 确保是256bit无符号数，避免Python整数的符号位问题
    cmd_code_256bit = cmd_code & ((1 << 256) - 1)
    # 转换为16进制字符串（64位，大写，无0x前缀）
    hex_str = f"{cmd_code_256bit:064X}"

    return hex_str


# ========== 示例：使用脚本生成cmd code ==========
if __name__ == "__main__":
    # 1. 填写需要的字段值（未填的字段默认0）
    op_dec = 0x3        # 8bit
    dep_cnt = 0x2       # 8bit
    succ_cnt = 0x0      # 8bit
    dep_id_0 = 0x0    # 16bit
    dep_id_1 = 0x1 
    word_0 = 0x0  # 32bit
    padding = 0x0       # 8bit

    # 2. 生成256bit cmd code
    cmd_hex = generate_256bit_cmd_code(
        op_dec=op_dec,
        dep_cnt=dep_cnt,
        succ_cnt=succ_cnt,
        dep_id_0=dep_id_0,
        dep_id_1=dep_id_1,
        word_0=word_0,
        padding=padding
    )

    # 3. 打印结果
    print("=" * 80)
    print("256bit CMD Code 生成结果（64位16进制）：")
    print(f"0x{cmd_hex}")
    print("=" * 80)
    # 可选：拆分打印各字段（验证位域是否正确）
    print("\n各字段位域验证：")
    print(f"OP_DEC [7:0]    : 0x{op_dec:02X} (对应结果最后2位：{cmd_hex[-2:]})")
    print(f"DEP_CNT [15:8]  : 0x{dep_cnt:02X} (对应结果倒数4-3位：{cmd_hex[-4:-2]})")
    print(f"SUCC_CNT [23:16]: 0x{succ_cnt:02X} (对应结果倒数6-5位：{cmd_hex[-6:-4]})")
    print(f"PADDING [231:224]: 0x{padding:02X} (对应结果第58-59位：{cmd_hex[56:58]})")