import spinal.core._
import spinal.lib._
import config._
import config.TTU_ISA._
import scala.collection.mutable.ArrayBuffer

// todo : WAIT cmd stall fetch
case class scoreboard(cfg: TTConfig) extends Component {
  val io = new Bundle {
    val dec_in = slave Stream (dec_map(cfg))
    val last_cmd_id = slave Flow(UInt(cfg.CMD_AW bits))
    val all_task_done = out Bool()
    // 2-Level dispatch
    val dma_dispatch = master Stream(dma_dp(cfg))
    val dma_agu_ctrl = master Stream(Bits(cfg.CMD_AW bits))
    val agu_dma_dispatch = master Stream(agu_dma_dp(cfg))
    val agu_compute_ifm_dispatch = master Stream(agu_compute_dp(cfg))
    val agu_compute_ker_dispatch = master Stream(agu_compute_dp(cfg))
    val loop_ctrl_dispatch = master Stream(loop_ctrl_dp(cfg))
    val tensor_dispatch = master Stream(tensor_dp(cfg))
    val alu_int_dispatch = master Stream(alu_int_dp(cfg))
    val alu_fp_dispatch = master Stream(alu_fp_dp(cfg))
    val store_engine_dispatch = master Stream(store_dp(cfg))
    val dma_back = slave Stream(alu_back(cfg))
    val agu_dma_back = slave Stream(alu_back(cfg))
    val agu_compute_ifm_back = slave Stream(alu_back(cfg))
    val agu_compute_ker_back = slave Stream(alu_back(cfg))
    val tensor_back = slave Stream(alu_back(cfg))
    val alu_int_back = slave Stream(alu_back(cfg))
    val alu_fp_back = slave Stream(alu_back(cfg))
    val store_engine_back = slave Stream(alu_back(cfg))
  }

  object SCB_STATE extends SpinalEnum {
    val SCB_IDLE, SCB_INIT, SCB_ON, SCB_DONE, SCB_DBH/*done but hold*/, SCB_TIMEOUT, SCB_ERR = newElement()
  }

  val flow_init = Flow(dec_map(cfg))
  flow_init.valid := False
  flow_init.payload.assignFromBits(B(0,dec_map(cfg).getBitsWidth bits))

  val dec_flow = RegNextWhen(io.dec_in.asFlow, io.dec_in.ready) init(flow_init)

  val scb = Vec.fill(cfg.SCB_DEEPTH)(Reg(dec_map(cfg)) init(flow_init.payload))
  val scb_state = Vec.fill(cfg.SCB_DEEPTH)(Reg(SCB_STATE()) init(SCB_STATE.SCB_IDLE))
  val scb_state_next = Vec.fill(cfg.SCB_DEEPTH)(SCB_STATE())
  scb_state := scb_state_next
  scb_state_next := scb_state

  // todo: scb_id_list can be replaced by fifo
  val scb_id_list = Vec(
    (0 until cfg.SCB_DEEPTH).map { i =>
      Reg(UInt(log2Up(cfg.SCB_DEEPTH) bits)) init(i)
    }
  )
  val scb_id_head = Reg(UInt(log2Up(cfg.SCB_DEEPTH) bits)) init(0)
  val scb_id_tail = Reg(UInt(log2Up(cfg.SCB_DEEPTH) bits)) init(cfg.SCB_DEEPTH-1)
  val scb_id_count = UInt(log2Up(cfg.SCB_DEEPTH)+1 bits)
  val scb_update_id = Reg(UInt(log2Up(cfg.SCB_DEEPTH) bits)) init(0)
  when(io.dec_in.valid && io.dec_in.ready){
    scb_id_head := (scb_id_head + 1).resized
    scb_update_id := scb_id_list(scb_id_head)  //scb_id_head
  }
  val scb_done = Reg(Bool()) init(False)
  val scb_done_id = Reg(UInt(log2Up(cfg.SCB_DEEPTH) bits)) init(0)
  val scb_done_vec = Vec.fill(cfg.SCB_DEEPTH)(Bool()) //Vec(scb_state.map(_ === SCB_STATE.SCB_DONE))
  scb_done_vec.map(_.setAsReg() init(False))
  val scb_done_oh    = OHMasking.first(scb_done_vec)
  scb_done := scb_done_vec.orR
  scb_done_id := OHToUInt(scb_done_oh)

  val scb_near_done_vec = Vec.fill(cfg.SCB_DEEPTH)(Reg(Bool()) init(False)) //Vec(scb_state.map(_ === SCB_STATE.SCB_DONE))


  when(scb_done){
    scb_id_tail := (scb_id_tail + 1).resized
    scb_id_list((scb_id_tail + 1).resized) := scb_done_id
    scb_state_next(scb_done_id) := SCB_STATE.SCB_IDLE
  }

  when(scb_id_head > scb_id_tail){
    scb_id_count := cfg.SCB_DEEPTH + scb_id_tail - scb_id_head
  } .otherwise{
    scb_id_count := (scb_id_tail - scb_id_head).resized
  }

  io.dec_in.ready := (scb_id_count > 0) // todo

  when(dec_flow.valid){
    scb(scb_update_id) := dec_flow.payload
    scb_state_next(scb_update_id) := SCB_STATE.SCB_INIT
  }

  // deal with dependence
  val issue_flow = Reg(Flow(dec_map(cfg))) init(flow_init)
  val dep_vec = Vec.fill(cfg.SCB_DEEPTH)(Reg(Bool()) init(False))
  val dep_hit_vec = OHMasking.first(dep_vec)
  val dep_hit = Reg(Bool()) init(False)
  val dep_hit_id = Reg(UInt(log2Up(cfg.SCB_DEEPTH) bits)) init(0)
  dep_hit := dep_hit_vec.orR
  dep_hit_id := OHToUInt(dep_hit_vec)
  val succ_hit = Vec.fill(cfg.SCB_DEEPTH)(Bool())

  when(dep_hit && scb_state(dep_hit_id) === SCB_STATE.SCB_INIT){
    issue_flow.valid := True
    issue_flow.payload := scb(dep_hit_id)
  } .otherwise{
    issue_flow.valid := False
  }

  def matchDepId(depIds: Seq[Bits], cmdId: Bits): Bool = {
    depIds.map(_ === cmdId).orR
  }

  for (i <- 0 until cfg.SCB_DEEPTH) {
    succ_hit(i) := matchDepId(
      Seq(
        issue_flow.payload.DEP_ID_0,
        issue_flow.payload.DEP_ID_1,
        issue_flow.payload.DEP_ID_2,
        issue_flow.payload.DEP_ID_3
      ),
      scb(i).CMD_ID
    )
  }
  // dependence: ALU + dep_id done + (tensor addr)
  val alu_idle_vec = Vec.fill(cfg.SCB_DEEPTH)(Bool())
  for (i <- 0 until cfg.SCB_DEEPTH) {
    when(scb_state(i) === SCB_STATE.SCB_INIT){  // to issue
      when(alu_idle_vec(i)){  // alu idle
        when(scb(i).DEP_CNT === 0){ // no dep to do
          dep_vec(i) := True
        } .otherwise{
          dep_vec(i) := False
        }
      } .otherwise{
        dep_vec(i) := False
      }
    } .otherwise{
      dep_vec(i) := False
    }
  }

  for (i <- 0 until cfg.SCB_DEEPTH) {
    alu_idle_vec(i) := True
    switch(scb(i).OP_DEC) {
      is(AGU_DMA) {
        alu_idle_vec(i) := io.agu_dma_dispatch.ready
      }
      is(AGU_COMPUTE_IFM) {
        alu_idle_vec(i) := io.agu_compute_ifm_dispatch.ready
      }
      is(AGU_COMPUTE_KER) {
        alu_idle_vec(i) := io.agu_compute_ker_dispatch.ready
      }
      is(DMA_NORMAL, STORE) {
        alu_idle_vec(i) := io.dma_dispatch.ready
      }
      is(DMA_AGU) {
        alu_idle_vec(i) := io.dma_agu_ctrl.ready
      }
      is(CONV_TILE) {
        alu_idle_vec(i) := io.tensor_dispatch.ready && io.loop_ctrl_dispatch.ready
      }
      is(VINT) {
        alu_idle_vec(i) := io.alu_int_dispatch.ready
      }
      is(VFP) {
        alu_idle_vec(i) := io.alu_fp_dispatch.ready
      }
    }
  }

  for (i <- 0 until cfg.SCB_DEEPTH) {
    when(scb_state(i) === SCB_STATE.SCB_INIT && scb(i).DEP_CNT =/= 0) {
      for (j <- 0 until cfg.SCB_DEEPTH) {
        when(scb_near_done_vec(j) || scb_done_vec(j)) {
          when(scb(j).CMD_ID === scb(i).DEP_ID_0 || scb(j).CMD_ID === scb(i).DEP_ID_1 || scb(j).CMD_ID === scb(i).DEP_ID_2 || scb(j).CMD_ID === scb(i).DEP_ID_3) {
            scb(i).DEP_CNT := scb(i).DEP_CNT - 1
          }
        }
      }
    }
    when(issue_flow.valid && succ_hit(i)){
      scb(i).SUCC_CNT := scb(i).SUCC_CNT - 1
    }
  }

  for (i <- 0 until cfg.SCB_DEEPTH) {
    switch(scb_state(i)){
      is(SCB_STATE.SCB_INIT){
        when(dep_hit && dep_hit_id === i){
          scb_state_next(i) := SCB_STATE.SCB_ON
        }
      }
      is(SCB_STATE.SCB_ON){
        switch(scb(i).OP_DEC){
          is(AGU_DMA){
            when(io.agu_dma_back.valid && io.agu_dma_back.payload.CMD_ID === scb(i).CMD_ID) {
              scb_near_done_vec(i) := True
              when(scb(i).SUCC_CNT === 0){
                scb_state_next(i) := SCB_STATE.SCB_DONE
              } .otherwise{
                scb_state_next(i) := SCB_STATE.SCB_DBH
              }
            }
          }
          is(AGU_COMPUTE_IFM){
            when(io.agu_compute_ifm_back.valid && io.agu_compute_ifm_back.payload.CMD_ID === scb(i).CMD_ID) {
              scb_near_done_vec(i) := True
              when(scb(i).SUCC_CNT === 0){
                scb_state_next(i) := SCB_STATE.SCB_DONE
              } .otherwise{
                scb_state_next(i) := SCB_STATE.SCB_DBH
              }
            }
          }
          is(AGU_COMPUTE_KER){
            when(io.agu_compute_ker_back.valid && io.agu_compute_ker_back.payload.CMD_ID === scb(i).CMD_ID) {
              scb_near_done_vec(i) := True
              when(scb(i).SUCC_CNT === 0){
                scb_state_next(i) := SCB_STATE.SCB_DONE
              } .otherwise{
                scb_state_next(i) := SCB_STATE.SCB_DBH
              }
            }
          }
          is(DMA_NORMAL,DMA_AGU){
            when(io.dma_back.valid && io.dma_back.payload.CMD_ID === scb(i).CMD_ID) {
              scb_near_done_vec(i) := True
              when(scb(i).SUCC_CNT === 0){
                scb_state_next(i) := SCB_STATE.SCB_DONE
              } .otherwise{
                scb_state_next(i) := SCB_STATE.SCB_DBH
              }
            }
          }
          is(CONV_TILE){
            when(io.tensor_back.valid && io.tensor_back.payload.CMD_ID === scb(i).CMD_ID) {
              scb_near_done_vec(i) := True
              when(scb(i).SUCC_CNT === 0){
                scb_state_next(i) := SCB_STATE.SCB_DONE
              } .otherwise{
                scb_state_next(i) := SCB_STATE.SCB_DBH
              }
            }
          }
          is(VINT){
            when(io.alu_int_back.valid && io.alu_int_back.payload.CMD_ID === scb(i).CMD_ID) {
              scb_near_done_vec(i) := True
              when(scb(i).SUCC_CNT === 0){
                scb_state_next(i) := SCB_STATE.SCB_DONE
              } .otherwise{
                scb_state_next(i) := SCB_STATE.SCB_DBH
              }
            }
          }
          is(VFP){
            when(io.alu_fp_back.valid && io.alu_fp_back.payload.CMD_ID === scb(i).CMD_ID) {
              scb_near_done_vec(i) := True
              when(scb(i).SUCC_CNT === 0){
                scb_state_next(i) := SCB_STATE.SCB_DONE
              } .otherwise{
                scb_state_next(i) := SCB_STATE.SCB_DBH
              }
            }
          }
          is(STORE){
            when(io.store_engine_back.valid && io.store_engine_back.payload.CMD_ID === scb(i).CMD_ID) {
              scb_near_done_vec(i) := True
              when(scb(i).SUCC_CNT === 0){
                scb_state_next(i) := SCB_STATE.SCB_DONE
              } .otherwise{
                scb_state_next(i) := SCB_STATE.SCB_DBH
              }
            }
          }
          // todo other ALU back, todo back ready
        }
      }
      is(SCB_STATE.SCB_DBH){
        scb_near_done_vec(i) := False
        when(scb(i).SUCC_CNT === 0){
          scb_state_next(i) := SCB_STATE.SCB_DONE
        }

      }
      is(SCB_STATE.SCB_DONE){
        scb_near_done_vec(i) := False
        when(scb_done_vec(i)){
          scb_done_vec(i) := False
        } .elsewhen(!scb_done){  // at least 1-cycle bubble between 2-commit
          scb_done_vec(i) := True
        }

      }
      //is(SCB_STATE.SCB_TIMEOUT,SCB_STATE.SCB_ERR){  } // todo
    }
  }

  io.dma_back.ready := True
  io.agu_dma_back.ready := True
  io.agu_compute_ifm_back.ready := True
  io.agu_compute_ker_back.ready := True
  io.tensor_back.ready := True
  io.alu_int_back.ready := True
  io.alu_fp_back.ready := True
  io.store_engine_back.ready := True

  // issue_flow -> dispatch
  io.agu_dma_dispatch.valid.setAsReg() init(False)
  io.agu_dma_dispatch.payload.setAsReg()
  io.agu_compute_ifm_dispatch.valid.setAsReg() init(False)
  io.agu_compute_ifm_dispatch.payload.setAsReg()
  io.agu_compute_ker_dispatch.valid.setAsReg() init(False)
  io.agu_compute_ker_dispatch.payload.setAsReg()
  io.dma_dispatch.valid.setAsReg() init(False)
  io.dma_dispatch.payload.setAsReg()
  io.dma_agu_ctrl.valid.setAsReg() init(False)
  io.dma_agu_ctrl.payload.setAsReg()
  io.loop_ctrl_dispatch.valid.setAsReg() init(False)
  io.loop_ctrl_dispatch.payload.setAsReg()
  io.tensor_dispatch.valid.setAsReg() init(False)
  io.tensor_dispatch.payload.setAsReg()
  io.alu_int_dispatch.valid.setAsReg() init(False)
  io.alu_int_dispatch.payload.setAsReg()
  io.alu_fp_dispatch.valid.setAsReg() init(False)
  io.alu_fp_dispatch.payload.setAsReg()
  io.store_engine_dispatch.valid.setAsReg() init(False)
  io.store_engine_dispatch.payload.setAsReg()

  switch(issue_flow.payload.OP_DEC){
    io.agu_dma_dispatch.valid := False
    io.agu_compute_ifm_dispatch.valid := False
    io.agu_compute_ker_dispatch.valid := False
    io.dma_dispatch.valid := False
    io.loop_ctrl_dispatch.valid := False
    io.tensor_dispatch.valid := False
    io.alu_int_dispatch.valid := False
    io.alu_fp_dispatch.valid := False
    io.store_engine_dispatch.valid := False
    is(AGU_DMA){
      io.agu_dma_dispatch.valid := issue_flow.valid
      io.agu_dma_dispatch.payload.CMD_ID := issue_flow.CMD_ID
      io.agu_dma_dispatch.payload.OP_DEC := issue_flow.OP_DEC
      io.agu_dma_dispatch.payload.WORD_0 := issue_flow.WORD_0
      io.agu_dma_dispatch.payload.WORD_1 := issue_flow.WORD_1
      io.agu_dma_dispatch.payload.WORD_2 := issue_flow.WORD_2
      io.agu_dma_dispatch.payload.WORD_3 := issue_flow.WORD_3
    }
    is(AGU_COMPUTE_IFM){
      io.agu_compute_ifm_dispatch.valid := issue_flow.valid
      io.agu_compute_ifm_dispatch.payload.CMD_ID := issue_flow.CMD_ID
      io.agu_compute_ifm_dispatch.payload.BASE := issue_flow.WORD_0.resize(cfg.L1_AW)

      // WORD_1
      io.agu_compute_ifm_dispatch.payload.ELEM_BYTES  := issue_flow.WORD_1(2 downto 0)
      io.agu_compute_ifm_dispatch.payload.LAYOUT_TYPE := issue_flow.WORD_1(4 downto 3)
      io.agu_compute_ifm_dispatch.payload.H_IN        := issue_flow.WORD_1(16 downto 5)
      io.agu_compute_ifm_dispatch.payload.W_IN        := issue_flow.WORD_1(28 downto 17)
      io.agu_compute_ifm_dispatch.payload.C_IN(2 downto 0) := issue_flow.WORD_1(31 downto 29)

      // WORD_2
      io.agu_compute_ifm_dispatch.payload.C_IN(11 downto 3) := issue_flow.WORD_2(8 downto 0)
      io.agu_compute_ifm_dispatch.payload.K_H        := issue_flow.WORD_2(12 downto 9)
      io.agu_compute_ifm_dispatch.payload.K_W        := issue_flow.WORD_2(16 downto 13)
      io.agu_compute_ifm_dispatch.payload.STRIDE_H   := issue_flow.WORD_2(19 downto 17)
      io.agu_compute_ifm_dispatch.payload.STRIDE_W   := issue_flow.WORD_2(22 downto 20)
      io.agu_compute_ifm_dispatch.payload.DILATION_H := issue_flow.WORD_2(26 downto 23)
      io.agu_compute_ifm_dispatch.payload.DILATION_W := issue_flow.WORD_2(30 downto 27)

      // WORD_3
      io.agu_compute_ifm_dispatch.payload.PAD_TOP    := issue_flow.WORD_3(5 downto 0)
      io.agu_compute_ifm_dispatch.payload.PAD_LEFT   := issue_flow.WORD_3(11 downto 6)
      io.agu_compute_ifm_dispatch.payload.TILE_H0    := issue_flow.WORD_3(23 downto 12)
      io.agu_compute_ifm_dispatch.payload.TILE_W0(7 downto 0) := issue_flow.WORD_3(31 downto 24)

      // WORD_4
      io.agu_compute_ifm_dispatch.payload.TILE_W0(11 downto 8) := issue_flow.WORD_4(3 downto 0)
      io.agu_compute_ifm_dispatch.payload.TILE_C0   := issue_flow.WORD_4(15 downto 4)
    }
    is(AGU_COMPUTE_KER){
      io.agu_compute_ker_dispatch.valid := issue_flow.valid
      io.agu_compute_ker_dispatch.payload.CMD_ID := issue_flow.CMD_ID
      io.agu_compute_ker_dispatch.payload.BASE := issue_flow.WORD_0.resize(cfg.L1_AW)

      // WORD_1
      io.agu_compute_ker_dispatch.payload.ELEM_BYTES  := issue_flow.WORD_1(2 downto 0)
      io.agu_compute_ker_dispatch.payload.LAYOUT_TYPE := issue_flow.WORD_1(4 downto 3)
      io.agu_compute_ker_dispatch.payload.H_IN        := issue_flow.WORD_1(16 downto 5)
      io.agu_compute_ker_dispatch.payload.W_IN        := issue_flow.WORD_1(28 downto 17)
      io.agu_compute_ker_dispatch.payload.C_IN(2 downto 0) := issue_flow.WORD_1(31 downto 29)

      // WORD_2
      io.agu_compute_ker_dispatch.payload.C_IN(11 downto 3) := issue_flow.WORD_2(8 downto 0)
      io.agu_compute_ker_dispatch.payload.K_H        := issue_flow.WORD_2(12 downto 9)
      io.agu_compute_ker_dispatch.payload.K_W        := issue_flow.WORD_2(16 downto 13)
      io.agu_compute_ker_dispatch.payload.STRIDE_H   := issue_flow.WORD_2(19 downto 17)
      io.agu_compute_ker_dispatch.payload.STRIDE_W   := issue_flow.WORD_2(22 downto 20)
      io.agu_compute_ker_dispatch.payload.DILATION_H := issue_flow.WORD_2(26 downto 23)
      io.agu_compute_ker_dispatch.payload.DILATION_W := issue_flow.WORD_2(30 downto 27)

      // WORD_3
      io.agu_compute_ker_dispatch.payload.PAD_TOP    := issue_flow.WORD_3(5 downto 0)
      io.agu_compute_ker_dispatch.payload.PAD_LEFT   := issue_flow.WORD_3(11 downto 6)
      //io.agu_compute_ker_dispatch.payload.TILE_H0    := issue_flow.WORD_3(23 downto 12)
      io.agu_compute_ker_dispatch.payload.C_OUT   := issue_flow.WORD_3(23 downto 12)
      io.agu_compute_ker_dispatch.payload.TILE_W0(7 downto 0) := issue_flow.WORD_3(31 downto 24)

      // WORD_4
      io.agu_compute_ker_dispatch.payload.TILE_W0(11 downto 8) := issue_flow.WORD_4(3 downto 0)
      io.agu_compute_ker_dispatch.payload.TILE_C0   := issue_flow.WORD_4(15 downto 4)
      io.agu_compute_ker_dispatch.payload.OC_TILE   := issue_flow.WORD_4(27 downto 16)
    }
    is(DMA_AGU){  // todo AGU
      io.dma_agu_ctrl.valid := issue_flow.valid
      io.dma_agu_ctrl.payload := issue_flow.CMD_ID
    }
    is(DMA_NORMAL){
      io.dma_dispatch.valid := issue_flow.valid
      io.dma_dispatch.payload.CMD_ID := issue_flow.CMD_ID
      io.dma_dispatch.payload.dma_cfg.isLoad := issue_flow.WORD_0(31)
      io.dma_dispatch.payload.dma_cfg.extAddr := issue_flow.WORD_0(30 downto 0).resize(cfg.memAxi.addressWidth).asUInt
      io.dma_dispatch.payload.dma_cfg.l1Addr := issue_flow.WORD_1.resize(cfg.memAxi.addressWidth).asUInt
      io.dma_dispatch.payload.dma_cfg.bytes := issue_flow.WORD_2.asUInt
      io.dma_dispatch.payload.dma_cfg.trans_len := issue_flow.WORD_3.resize(8).asUInt
      io.dma_dispatch.payload.is_last_trans := True // single-trans, different from DMA_AGU
    }
    is(CONV_TILE){
      io.loop_ctrl_dispatch.valid := issue_flow.valid
      io.loop_ctrl_dispatch.CMD_ID := issue_flow.CMD_ID
      io.tensor_dispatch.valid := issue_flow.valid
      io.tensor_dispatch.payload.CMD_ID := issue_flow.CMD_ID
      // ---------------- WORD_0 ----------------
      io.loop_ctrl_dispatch.payload.TILE_OH0 := issue_flow.WORD_0(11 downto 0)
      io.loop_ctrl_dispatch.payload.TILE_OW0 := issue_flow.WORD_0(23 downto 12)

      // ---------------- WORD_1 ----------------
      io.loop_ctrl_dispatch.payload.TILE_OC0 := issue_flow.WORD_1(11 downto 0)
      io.loop_ctrl_dispatch.payload.OH_TILE  := issue_flow.WORD_1(23 downto 12)

      // ---------------- WORD_2 ----------------
      io.loop_ctrl_dispatch.payload.OW_TILE  := issue_flow.WORD_2(11 downto 0)
      io.loop_ctrl_dispatch.payload.OC_TILE  := issue_flow.WORD_2(23 downto 12)
      io.loop_ctrl_dispatch.payload.ORDER    := issue_flow.WORD_2(25 downto 24)

      // ---------------- WORD_3 ----------------
      io.tensor_dispatch.payload.K_TILE := issue_flow.WORD_3(11 downto 0)
      io.tensor_dispatch.payload.M_TILE := issue_flow.WORD_3(23 downto 12)

      // ---------------- WORD_4 ----------------
      io.tensor_dispatch.payload.N_TILE := issue_flow.WORD_4(11 downto 0)

    }
    is(VINT){
      io.alu_int_dispatch.valid := issue_flow.valid
      io.alu_int_dispatch.payload.CMD_ID := issue_flow.CMD_ID
      io.alu_int_dispatch.payload.OP_DEC := issue_flow.OP_DEC
      io.alu_int_dispatch.payload.WORD_0 := issue_flow.WORD_0
      io.alu_int_dispatch.payload.WORD_1 := issue_flow.WORD_1
      io.alu_int_dispatch.payload.WORD_2 := issue_flow.WORD_2
      io.alu_int_dispatch.payload.WORD_3 := issue_flow.WORD_3
    }
    is(VFP){
      io.alu_fp_dispatch.valid := issue_flow.valid
      io.alu_fp_dispatch.payload.CMD_ID := issue_flow.CMD_ID
      io.alu_fp_dispatch.payload.OP_DEC := issue_flow.OP_DEC
      io.alu_fp_dispatch.payload.WORD_0 := issue_flow.WORD_0
      io.alu_fp_dispatch.payload.WORD_1 := issue_flow.WORD_1
      io.alu_fp_dispatch.payload.WORD_2 := issue_flow.WORD_2
      io.alu_fp_dispatch.payload.WORD_3 := issue_flow.WORD_3
    }
    is(STORE){
      io.store_engine_dispatch.valid := issue_flow.valid
      io.store_engine_dispatch.payload.CMD_ID := issue_flow.CMD_ID
      io.store_engine_dispatch.payload.OP_DEC := issue_flow.OP_DEC
      io.store_engine_dispatch.payload.WORD_0 := issue_flow.WORD_0
      io.store_engine_dispatch.payload.WORD_1 := issue_flow.WORD_1
      io.store_engine_dispatch.payload.WORD_2 := issue_flow.WORD_2
      io.store_engine_dispatch.payload.WORD_3 := issue_flow.WORD_3
    }
  }

  val flow_last_cmd_init = Flow(UInt(cfg.CMD_AW bits))
  flow_last_cmd_init.valid := False
  flow_last_cmd_init.payload := 0
  val last_cmd_id_flow = RegNext(io.last_cmd_id) init(flow_last_cmd_init)
  val last_cmd_cmt = Reg(Bool()) init(False)

  val scb_idle = Vec.fill(cfg.SCB_DEEPTH)(Reg(Bool()) init(False))
  val scb_all_done = scb_idle.andR && last_cmd_cmt

  for (i <- 0 until cfg.SCB_DEEPTH) {
    when(scb_state(i) === SCB_STATE.SCB_INIT){
      scb_idle(i) := False
    } .elsewhen(scb_done_vec(i)){
      scb_idle(i) := True
    }
  }
  when(scb_all_done){
    last_cmd_cmt := False
  }.elsewhen(last_cmd_id_flow.valid && scb(scb_done_id).CMD_ID.asUInt === last_cmd_id_flow.payload && scb_done){
    last_cmd_cmt := True
  }

  io.all_task_done.setAsReg() init(False)
  io.all_task_done := scb_all_done


}