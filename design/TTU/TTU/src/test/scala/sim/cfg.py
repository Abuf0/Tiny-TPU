# cfg.py：对应硬件的TTConfig
class TTConfig:
    def __init__(self):
        self.TPU_H = 8       # IFM输入行数（8x8阵列行）
        self.TPU_W = 8       # KE输入列数（8x8阵列列）
        self.TPU_O = 8       # OFM输出数（8行结果）
        self.TPU_IDWMAX = 32  # 输入数据位宽（8bit）
        self.TPU_ODWMAX = 32 # 输出PSUM位宽（32bit）
