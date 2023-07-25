/**************************************************************************************
* Copyright (c) 2020 Institute of Computing Technology, CAS
* Copyright (c) 2020 University of Chinese Academy of Sciences
* 
* NutShell is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2. 
* You may obtain a copy of Mulan PSL v2 at:
*             http://license.coscl.org.cn/MulanPSL2 
* 
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER 
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR 
* FIT FOR A PARTICULAR PURPOSE.  
*
* See the Mulan PSL v2 for more details.  
***************************************************************************************/

package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import bus.simplebus._
import top.Settings
import difftest._

class EXU(implicit val p: NutCoreConfig) extends NutCoreModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new DecodeIO))
    val out = Decoupled(new CommitIO)
    val flush = Input(Bool())
    val dmem = new SimpleBusUC(addrBits = VAddrBits, userBits = dmmuUserBits)
    val forward = new ForwardIO
    val memMMU = Flipped(new MemMMUIO)
  })

  val src1 = io.in.bits.data.src1(XLEN-1,0)
  val src2 = io.in.bits.data.src2(XLEN-1,0)

  val (fuType, fuOpType) = (io.in.bits.ctrl.fuType, io.in.bits.ctrl.fuOpType)

  val fuValids = Wire(Vec(FuType.num, Bool()))
  (0 until FuType.num).map (i => fuValids(i) := (fuType === i.U) && io.in.valid && !io.flush)

  val alu = if (IsLa32r) Module(new La32rALU(hasBru = true)) else Module(new ALU(hasBru = true))
  val aluOut = alu.access(valid = fuValids(FuType.alu), src1 = src1, src2 = src2, func = fuOpType)
  alu.io.cfIn := io.in.bits.cf
  alu.io.offset := io.in.bits.data.imm
  alu.io.out.ready := true.B

  def isBru(func: UInt) = func(4)

  val lsu = if(IsLa32r) Module(new La32rUnpipelinedLSU) else Module(new UnpipelinedLSU)
  val lsuTlbPF = WireInit(false.B)
  val lsuOut = lsu.access(valid = fuValids(FuType.lsu), src1 = src1, src2 = io.in.bits.data.imm, func = fuOpType, dtlbPF = lsuTlbPF)
  lsu.io.wdata := src2
  lsu.io.instr := io.in.bits.cf.instr
  lsu.io.pc := io.in.bits.cf.pc
  io.out.bits.isMMIO := lsu.io.isMMIO
  io.dmem <> lsu.io.dmem
  io.out.bits.storeCheck := lsu.io.storeCheck
  lsu.io.out.ready := true.B

  val mdu = Module(new MDU)
  val mduOut = mdu.access(valid = fuValids(FuType.mdu), src1 = src1, src2 = src2, func = fuOpType)
  mdu.io.out.ready := true.B

  // val csr = if (Settings.get("MmodeOnly")) Module(new CSR_M) else Module(new CSR)
  val csr = if (IsLa32r) Module(new La32rCSR()) else Module(new CSR)
  val csrOut = csr.access(valid = fuValids(FuType.csr), src1 = src1, src2 = src2, func = fuOpType)
  csr.io.cfIn := io.in.bits.cf // TODO : this connect does not support ooo exec
  csr.io.la32rLSUExcp := lsu.io.la32rExcp
  if (!IsLa32r) {
    csr.io.cfIn.exceptionVec(loadAddrMisaligned) := lsu.io.loadAddrMisaligned
    csr.io.cfIn.exceptionVec(storeAddrMisaligned) := lsu.io.storeAddrMisaligned
  }
  csr.io.instrValid := io.in.valid && !io.flush && !alu.io.redirect.valid
  csr.io.isBackendException := false.B
  io.out.bits.intrNO := csr.io.intrNO
  csr.io.isBackendException := false.B
  csr.io.out.ready := true.B
  io.out.bits.difftestExceptionSkip := csr.io.difftestExceptionSkip
  io.out.bits.tlbModifyInst := csr.io.tlbModifyInst

  csr.io.imemMMU <> io.memMMU.imem
  csr.io.dmemMMU <> io.memMMU.dmem

  val mou = Module(new MOU)
  // mou does not write register
  mou.access(valid = fuValids(FuType.mou), src1 = src1, src2 = src2, func = fuOpType)
  mou.io.cfIn := io.in.bits.cf
  mou.io.out.ready := true.B
  
  io.out.bits.decode := DontCare
  (io.out.bits.decode.ctrl, io.in.bits.ctrl) match { case (o, i) =>
    // TODO : actually do not need loadAddrMisaligned and storeAddrMisaligned when la32r enable
    o.rfWen := i.rfWen && (!lsu.io.la32rExcp.hasExcp || !fuValids(FuType.lsu)) && !(csr.io.wenFix && fuValids(FuType.csr)) // TODO : csr cond may has bug
    o.rfDest := i.rfDest
    o.fuType := i.fuType
  }
  io.out.bits.decode.cf.pc := io.in.bits.cf.pc
  io.out.bits.decode.cf.instr := io.in.bits.cf.instr
  io.out.bits.decode.cf.runahead_checkpoint_id := io.in.bits.cf.runahead_checkpoint_id
  io.out.bits.decode.cf.isBranch := io.in.bits.cf.isBranch
  // NOTE : need make branch mispred redirect piority higher than csr,
  // when csr redirect priority higher than alu & is interrupt or excp attach on mispred control flow inst, it go wrong
  io.out.bits.decode.cf.redirect <>
    Mux(alu.io.redirect.valid, alu.io.redirect,
      Mux(csr.io.redirect.valid, csr.io.redirect, mou.io.redirect))
  
  Debug(mou.io.redirect.valid || csr.io.redirect.valid || alu.io.redirect.valid, "[REDIRECT] mou %x csr %x alu %x \n", mou.io.redirect.valid, csr.io.redirect.valid, alu.io.redirect.valid)
  Debug(mou.io.redirect.valid || csr.io.redirect.valid || alu.io.redirect.valid, "[REDIRECT] flush: %d mou %x csr %x alu %x\n", io.flush, mou.io.redirect.target, csr.io.redirect.target, alu.io.redirect.target)

  // FIXME: should handle io.out.ready == false
  // TODO : may has perf bug
  io.out.valid := io.in.valid && MuxLookup(fuType, true.B, List(
    FuType.lsu -> lsu.io.out.valid,
    FuType.mdu -> mdu.io.out.valid
  ))

  io.out.bits.commits(FuType.alu) := aluOut
  io.out.bits.commits(FuType.lsu) := lsuOut
  io.out.bits.commits(FuType.csr) := csrOut
  io.out.bits.commits(FuType.mdu) := mduOut
  io.out.bits.commits(FuType.mou) := 0.U

  io.in.ready := !io.in.valid || io.out.fire()

  io.forward.valid := io.in.valid
  io.forward.wb.rfWen := io.in.bits.ctrl.rfWen
  io.forward.wb.rfDest := io.in.bits.ctrl.rfDest
  io.forward.wb.rfData := Mux(alu.io.out.fire(), aluOut, lsuOut)
  io.forward.fuType := io.in.bits.ctrl.fuType

  val isBru = ALUOpType.isBru(fuOpType)
  BoringUtils.addSource(alu.io.out.fire() && !isBru, "perfCntCondMaluInstr")
  BoringUtils.addSource(alu.io.out.fire() && isBru, "perfCntCondMbruInstr")
  BoringUtils.addSource(lsu.io.out.fire(), "perfCntCondMlsuInstr")
  BoringUtils.addSource(mdu.io.out.fire(), "perfCntCondMmduInstr")
  BoringUtils.addSource(csr.io.out.fire(), "perfCntCondMcsrInstr")

  if (!p.FPGAPlatform) {
    val cycleCnt = WireInit(0.U(64.W))
    val instrCnt = WireInit(0.U(64.W))
    val nutcoretrap = io.in.bits.ctrl.isNutCoreTrap && io.in.valid

    BoringUtils.addSink(cycleCnt, "simCycleCnt")
    BoringUtils.addSink(instrCnt, "simInstrCnt")
    BoringUtils.addSource(nutcoretrap, "nutcoretrap")

    val difftest = Module(new DifftestTrapEvent)
    difftest.io.clock    := clock
    difftest.io.coreid   := 0.U // TODO: nutshell does not support coreid auto config
    difftest.io.valid    := nutcoretrap
    difftest.io.code     := 0.U
    difftest.io.pc       := io.in.bits.cf.pc
    difftest.io.cycleCnt := cycleCnt
    difftest.io.instrCnt := instrCnt
  }
}
