# gemm_utils.py
import numpy as np

def generate_gemm_data(M, K, N, bit_width=8, signed=True,seed=None):
    """
    生成GEMM测试数据：A(M×K)、B(K×N)，计算参考C=A×B
    :param M/N/K: GEMM维度
    :param bit_width: 输入数据位宽
    :param signed: 是否有符号（匹配硬件UInt/SInt）
    :return: A, B, C_ref（numpy数组）
    """
    # 生成符合位宽的随机数
    if seed is not None:
        np.random.seed(seed)  # 固定全局种子
        # 进阶：使用独立的RandomState，避免影响全局随机状态
        # rng = np.random.RandomState(seed)
    else:
        np.random.seed()  # 使用系统随机种子
    if signed:
        # 8bit有符号：-128~127
        #A = np.random.randint(-(1<<(bit_width-1)), (1<<(bit_width-1))-1, (M, K), dtype=np.int8)
        A = np.random.randint(0, (1<<bit_width)-1, (M, K), dtype=np.uint8)
        B = np.random.randint(-(1<<(bit_width-1)), (1<<(bit_width-1))-1, (K, N), dtype=np.int8)
    else:
        # 8bit无符号：0~255
        A = np.random.randint(0, (1<<bit_width)-1, (M, K), dtype=np.uint8)
        B = np.random.randint(0, (1<<bit_width)-1, (K, N), dtype=np.uint8)

    # 计算参考结果（32bit累加，匹配硬件PSUM位宽）
    # print(A)
    # print(B)
    # print(f"A.shape: {A.shape}, B.shape: {B.shape}")  # 应输出 (8,2) 和 (2,8)
    C_ref = np.dot(A.astype(np.int32), B.astype(np.int32))
    # print(f"C_ref.shape: {C_ref.shape}")
    # print(C_ref)

    return A, B, C_ref

def verify_gemm_result(C_dut, C_ref, tolerance=0):
    """
    校验TPU输出与参考结果是否一致
    :param C_dut: TPU输出的PSUM矩阵
    :param C_ref: 软件计算的参考矩阵
    :param tolerance: 允许的误差阈值（默认0，即完全匹配）
    :return: 是否通过校验
    """
    # 检查维度是否一致（新增：避免维度不匹配导致的报错）
    if C_dut.shape != C_ref.shape:
        print(f"❌ 维度不匹配！TPU输出维度：{C_dut.shape}，参考结果维度：{C_ref.shape}")
        return False

    # 判断所有元素是否在误差范围内
    if np.allclose(C_dut, C_ref, atol=tolerance):
        print(f"✅ GEMM pass！error≤{tolerance}")
        return True
    else:
        # 获取所有误差超过阈值的位置
        error_mask = np.abs(C_dut - C_ref) > tolerance
        error_idx = np.where(error_mask)
        # 将多维索引转换为(行,列)的元组列表，更易读
        error_positions = list(zip(
            [int(idx) for idx in error_idx[0]],  # 行索引转int
            [int(idx) for idx in error_idx[1]]   # 列索引转int
        ))

        print(f"\n❌ GEMM failed！ {len(error_positions)} errors：")
        # print(f"{'error id':<12} {'dut':<12} {'model':<12} (delta)")
        # print("-" * 50)  # 分隔线，增强可读性
        # 逐个打印错误位置（固定列宽对齐）
        for pos in error_positions:
            dut_val = C_dut[pos]
            ref_val = C_ref[pos]
            delta = abs(dut_val - ref_val)
            # 核心：用f-string对齐语法，<表示左对齐，数字表示列宽
            print(f"{str(pos):<6} dut = {dut_val:<10} model = {ref_val:<10} (delta={delta})")

        # 可选：打印完整的错误矩阵（便于批量分析）
        # print("\n📊 误差矩阵（TPU - 参考）：")
        # error_matrix = np.where(error_mask, C_dut - C_ref, 0)
        # print(error_matrix)

        return False