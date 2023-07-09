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
import difftest._
import nutcore.LA32R_LSUInstr.InstrStore

abstract class AbstractDecoder(implicit val p: NutCoreConfig) extends NutCoreModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new CtrlFlowIO))
    val out = Decoupled(new DecodeIO)
    val isWFI = Output(Bool()) // require NutCoreSim to advance mtime when wfi to reduce the idle time in Linux, do not use in la32r
    val isBranch = Output(Bool())
  })
}

class Decoder(implicit override val p: NutCoreConfig) extends AbstractDecoder with HasInstrType {

  val hasIntr = Wire(Bool())
  val instr = io.in.bits.instr
  val decodeList = ListLookup(instr, Instructions.DecodeDefault, Instructions.DecodeTable)
  val instrType :: fuType :: fuOpType :: Nil = // insert Instructions.DecodeDefault when interrupt comes
    Instructions.DecodeDefault.zip(decodeList).map{case (intr, dec) => Mux(hasIntr || io.in.bits.exceptionVec(instrPageFault) || io.out.bits.cf.exceptionVec(instrAccessFault), intr, dec)}
  // val instrType :: fuType :: fuOpType :: Nil = ListLookup(instr, Instructions.DecodeDefault, Instructions.DecodeTable)
  val isRVC = if (HasCExtension) instr(1,0) =/= "b11".U else false.B
  val rvcImmType :: rvcSrc1Type :: rvcSrc2Type :: rvcDestType :: Nil =
    ListLookup(instr, CInstructions.DecodeDefault, CInstructions.CExtraDecodeTable) 

  io.out.bits := DontCare

  io.out.bits.ctrl.fuType := fuType
  io.out.bits.ctrl.fuOpType := fuOpType

  val SrcTypeTable = List(
    InstrI -> (SrcType.reg, SrcType.imm),
    InstrR -> (SrcType.reg, SrcType.reg),
    InstrS -> (SrcType.reg, SrcType.reg),
    InstrSA-> (SrcType.reg, SrcType.reg),
    InstrB -> (SrcType.reg, SrcType.reg),
    InstrU -> (SrcType.pc , SrcType.imm),
    InstrJ -> (SrcType.pc , SrcType.imm),
    InstrN -> (SrcType.pc , SrcType.imm)
  )
  val src1Type = LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._1)))
  val src2Type = LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._2)))

  val (rs, rt, rd) = (instr(19, 15), instr(24, 20), instr(11, 7))
  // see riscv-spec vol1, Table 16.1: Compressed 16-bit RVC instruction formats.
  val rs1       = instr(11,7)
  val rs2       = instr(6,2)
  val rs1p      = LookupTree(instr(9,7), RVCInstr.RVCRegNumTable.map(p => (p._1, p._2)))
  val rs2p      = LookupTree(instr(4,2), RVCInstr.RVCRegNumTable.map(p => (p._1, p._2)))
  val rvc_shamt = Cat(instr(12),instr(6,2)) 
  // val rdp_rs1p = LookupTree(instr(9,7), RVCRegNumTable)
  // val rdp      = LookupTree(instr(4,2), RVCRegNumTable)

  val RegLookUpTable = List(
    RVCInstr.DtCare   -> 0.U,
    RVCInstr.REGrs    -> rs,
    RVCInstr.REGrt    -> rt,
    RVCInstr.REGrd    -> rd,
    RVCInstr.REGrs1   -> rs1,
    RVCInstr.REGrs2   -> rs2,
    RVCInstr.REGrs1p  -> rs1p,
    RVCInstr.REGrs2p  -> rs2p,
    RVCInstr.REGx1    -> 1.U,
    RVCInstr.REGx2    -> 2.U
  )

  val rvc_src1 = LookupTree(rvcSrc1Type, RegLookUpTable.map(p => (p._1, p._2)))
  val rvc_src2 = LookupTree(rvcSrc2Type, RegLookUpTable.map(p => (p._1, p._2)))
  val rvc_dest =  LookupTree(rvcDestType, RegLookUpTable.map(p => (p._1, p._2)))

  val rfSrc1 = Mux(isRVC, rvc_src1, rs)
  val rfSrc2 = Mux(isRVC, rvc_src2, rt)
  val rfDest = Mux(isRVC, rvc_dest, rd)
  // TODO: refactor decode logic
  // make non-register addressing to zero, since isu.sb.isBusy(0) === false.B
  io.out.bits.ctrl.rfSrc1 := Mux(src1Type === SrcType.pc, 0.U, rfSrc1)
  io.out.bits.ctrl.rfSrc2 := Mux(src2Type === SrcType.reg, rfSrc2, 0.U)
  io.out.bits.ctrl.rfWen  := isrfWen(instrType)
  io.out.bits.ctrl.rfDest := Mux(isrfWen(instrType), rfDest, 0.U)

  io.out.bits.data := DontCare
  val imm = LookupTree(instrType, List(
    InstrI  -> SignExt(instr(31, 20), XLEN),
    InstrS  -> SignExt(Cat(instr(31, 25), instr(11, 7)), XLEN),
    InstrSA -> SignExt(Cat(instr(31, 25), instr(11, 7)), XLEN),
    InstrB  -> SignExt(Cat(instr(31), instr(7), instr(30, 25), instr(11, 8), 0.U(1.W)), XLEN),
    InstrU  -> SignExt(Cat(instr(31, 12), 0.U(12.W)), XLEN),//fixed
    InstrJ  -> SignExt(Cat(instr(31), instr(19, 12), instr(20), instr(30, 21), 0.U(1.W)), XLEN)
  ))
  val immrvc = LookupTree(rvcImmType, List(
    // InstrIW -> Cat(Fill(20+32, instr(31)), instr(31, 20)),//fixed
    RVCInstr.ImmNone  -> 0.U(XLEN.W),
    RVCInstr.ImmLWSP  -> ZeroExt(Cat(instr(3,2), instr(12), instr(6,4), 0.U(2.W)), XLEN),
    RVCInstr.ImmLDSP  -> ZeroExt(Cat(instr(4,2), instr(12), instr(6,5), 0.U(3.W)), XLEN),
    RVCInstr.ImmSWSP  -> ZeroExt(Cat(instr(8,7), instr(12,9), 0.U(2.W)), XLEN),
    RVCInstr.ImmSDSP  -> ZeroExt(Cat(instr(9,7), instr(12,10), 0.U(3.W)), XLEN),
    RVCInstr.ImmSW    -> ZeroExt(Cat(instr(5), instr(12,10), instr(6), 0.U(2.W)), XLEN),
    RVCInstr.ImmSD    -> ZeroExt(Cat(instr(6,5), instr(12,10), 0.U(3.W)), XLEN),
    RVCInstr.ImmLW    -> ZeroExt(Cat(instr(5), instr(12,10), instr(6), 0.U(2.W)), XLEN),
    RVCInstr.ImmLD    -> ZeroExt(Cat(instr(6,5), instr(12,10), 0.U(3.W)), XLEN),
    RVCInstr.ImmJ     -> SignExt(Cat(instr(12), instr(8), instr(10,9), instr(6), instr(7), instr(2), instr(11), instr(5,3), 0.U(1.W)), XLEN),
    RVCInstr.ImmB     -> SignExt(Cat(instr(12), instr(6,5), instr(2), instr(11,10), instr(4,3), 0.U(1.W)), XLEN),
    RVCInstr.ImmLI    -> SignExt(Cat(instr(12), instr(6,2)), XLEN),
    RVCInstr.ImmLUI   -> SignExt(Cat(instr(12), instr(6,2), 0.U(12.W)), XLEN),
    RVCInstr.ImmADDI  -> SignExt(Cat(instr(12), instr(6,2)), XLEN),
    RVCInstr.ImmADDI16SP-> SignExt(Cat(instr(12), instr(4,3), instr(5), instr(2), instr(6), 0.U(4.W)), XLEN),
    RVCInstr.ImmADD4SPN-> ZeroExt(Cat(instr(10,7), instr(12,11), instr(5), instr(6), 0.U(2.W)), XLEN),
    RVCInstr.ImmCBREAK -> 1.U(XLEN.W)
    // ImmFLWSP  -> 
    // ImmFLDSP  -> 
  ))
  io.out.bits.data.imm  := Mux(isRVC, immrvc, imm)

  when (fuType === FuType.bru) {
    def isLink(reg: UInt) = (reg === 1.U || reg === 5.U)
    when (isLink(rfDest) && fuOpType === ALUOpType.jal) { io.out.bits.ctrl.fuOpType := ALUOpType.call }
    when (fuOpType === ALUOpType.jalr) {
      when (isLink(rfSrc1)) { io.out.bits.ctrl.fuOpType := ALUOpType.ret }
      when (isLink(rfDest)) { io.out.bits.ctrl.fuOpType := ALUOpType.call }
    }
  }
  // fix LUI
  io.out.bits.ctrl.src1Type := Mux(instr(6,0) === "b0110111".U, SrcType.reg, src1Type)
  io.out.bits.ctrl.src2Type := src2Type

  val NoSpecList = Seq(
    FuType.csr
  )

  val BlockList = Seq(
    FuType.mou
  )

  io.out.bits.ctrl.isNutCoreTrap := (instr(31,0) === NutCoreTrap.TRAP) && io.in.valid
  io.out.bits.ctrl.noSpecExec := NoSpecList.map(j => io.out.bits.ctrl.fuType === j).reduce(_ || _)
  io.out.bits.ctrl.isBlocked :=
  (
    io.out.bits.ctrl.fuType === FuType.lsu && LSUOpType.isAtom(io.out.bits.ctrl.fuOpType) ||
    BlockList.map(j => io.out.bits.ctrl.fuType === j).reduce(_ || _)
  )

  //output signals
  io.out.valid := io.in.valid
  io.in.ready := !io.in.valid || io.out.fire() && !hasIntr
  io.out.bits.cf <> io.in.bits
  // fix c_break


  Debug(io.out.fire(), "issue: pc %x npc %x instr %x\n", io.out.bits.cf.pc, io.out.bits.cf.pnpc, io.out.bits.cf.instr)

  val intrVec = WireInit(0.U(13.W))
  BoringUtils.addSink(intrVec, "intrVecIDU")
  io.out.bits.cf.intrVec.zip(intrVec.asBools).map{ case(x, y) => x := y }
  hasIntr := intrVec.orR

  val vmEnable = WireInit(false.B)
  BoringUtils.addSink(vmEnable, "DTLBENABLE")

  io.out.bits.cf.exceptionVec.map(_ := false.B)
  io.out.bits.cf.exceptionVec(illegalInstr) := (instrType === InstrN && !hasIntr) && io.in.valid
  io.out.bits.cf.exceptionVec(instrPageFault) := io.in.bits.exceptionVec(instrPageFault)
  if (VAddrBits > PAddrBits) {
    io.out.bits.cf.exceptionVec(instrAccessFault) := io.in.bits.pc(VAddrBits - 1, PAddrBits).orR && !vmEnable
  } else {
    io.out.bits.cf.exceptionVec(instrAccessFault) := false.B
  }

  io.out.bits.ctrl.isNutCoreTrap := (instr === NutCoreTrap.TRAP) && io.in.valid
  io.isWFI := (instr === Priviledged.WFI) && io.in.valid
  io.isBranch := VecInit(RV32I_BRUInstr.table.map(i => i._2.tail(1) === fuOpType)).asUInt.orR && fuType === FuType.bru

}

class La32rDecoder(implicit override val p: NutCoreConfig) extends AbstractDecoder with HasLa32rInstrType {

  val hasIntr = Wire(Bool())
  val instr = io.in.bits.instr
  val decodeList = ListLookup(instr, La32rInstructions.DecodeDefault, La32rInstructions.DecodeTable)
  val instrType :: fuType :: fuOpType :: isrfWen :: Nil =
    La32rInstructions.DecodeDefault.zip(decodeList).map { case (intr, dec) => Mux(hasIntr, intr, dec) }


  val outFuOpType = io.out.bits.ctrl.fuOpType // TODO : this is dirty implementation to pass firrtl compile
  io.out.bits := DontCare
  io.isBranch := La32rALUOpType.isBranch(outFuOpType) && fuType === FuType.bru // TODO : this signal may be useless
  io.isWFI := DontCare

  io.out.bits.ctrl.fuType := fuType
  io.out.bits.ctrl.fuOpType := fuOpType

  // use SrcType.imm when we do not need reg source
  // rj is src1, rk is src2, rd is dest
  // but when it is branch inst, rj is src1, rd is src2
  val SrcTypeTable = List(
    Instr2R     -> (SrcType.reg, SrcType.imm),
    Instr3R     -> (SrcType.reg, SrcType.reg),
    Instr2RI8   -> (SrcType.reg, SrcType.imm),
    Instr2RI12  -> (SrcType.reg, SrcType.imm),
    Instr2RI14  -> (SrcType.reg, SrcType.imm),
    Instr2RI16  -> (SrcType.reg, SrcType.imm),
    Instr1RI21  -> (SrcType.reg, SrcType.imm),
    InstrI26    -> (SrcType.imm, SrcType.imm),
    Instr1RI20  -> (SrcType.imm, SrcType.imm),
    Instr2RI5   -> (SrcType.reg, SrcType.imm),
    InstrBranch -> (SrcType.reg, SrcType.reg),
    InstrStore  -> (SrcType.reg, SrcType.reg),
    Instr2RI12ZEXT -> (SrcType.reg, SrcType.imm),
    InstrN      -> (SrcType.imm, SrcType.imm),
    InstrCODE15 -> (SrcType.imm, SrcType.imm),
  )

  val src1Type = LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._1)))
  val src2Type = LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._2)))

  val (rk, rj, rd) = (instr(14, 10), instr(9, 5), instr(4, 0))
  val rfSrc1 = rj
  val rfSrc2 = Mux(fuType === FuType.bru && La32rALUOpType.isBranch(outFuOpType)
    || fuType === FuType.lsu && La32rLSUOpType.isStore(outFuOpType), rd, rk)
  val rfDest = Mux(fuType === FuType.bru && (fuOpType === La32rALUOpType.call), 1.U ,rd)

  io.out.bits.ctrl.rfSrc1 := Mux(src1Type === SrcType.imm , 0.U, rfSrc1)
  io.out.bits.ctrl.rfSrc2 := Mux(src2Type === SrcType.imm , 0.U, rfSrc2)
  io.out.bits.ctrl.rfWen := isrfWen
  io.out.bits.ctrl.rfDest := Mux(isrfWen.asBool, rfDest, 0.U)


  val imm = LookupTree(instrType, List(
    Instr2RI8     -> SignExt(instr(17, 10), XLEN),
    Instr2RI12    -> SignExt(instr(21, 10), XLEN),
    Instr2RI14    -> SignExt(instr(23, 10), XLEN),
    Instr2RI16    -> SignExt(instr(25, 10), XLEN),
    Instr1RI21    -> SignExt(Cat(instr(4, 0), instr(25, 10)), XLEN),
    InstrI26      -> SignExt(Cat(instr(9, 0), instr(25, 10)), XLEN),
    Instr1RI20    -> SignExt(instr(24, 5), XLEN),
    Instr2RI5     -> ZeroExt(instr(14, 10), XLEN),
    InstrBranch   -> SignExt(instr(25, 10), XLEN),
    InstrStore    -> SignExt(instr(21, 10), XLEN),
    Instr2RI12ZEXT -> ZeroExt(instr(21, 10), XLEN),
    InstrCODE15   -> ZeroExt(instr(14, 0), XLEN),
  ))
  io.out.bits.data.imm := imm

  io.out.bits.ctrl.src1Type := src1Type
  io.out.bits.ctrl.src2Type := src2Type

  when (fuType === FuType.bru) {
    // see la32r spec page 17 JIRL description
    when (fuOpType === La32rALUOpType.jirl && rd === 0.U && rj === 1.U && instr(25, 10) === 0.U) {
      io.out.bits.ctrl.fuOpType := La32rALUOpType.ret
    }
  }

  // fix csr access inst
  when (fuType === FuType.csr && fuOpType === La32rCSROpType.csracc) {
    val csrrj = instr(9, 5)
    when (csrrj === 0.U) {
      io.out.bits.ctrl.fuOpType := La32rCSROpType.csrrd
      io.out.bits.ctrl.src1Type := SrcType.imm
      io.out.bits.ctrl.src2Type := SrcType.imm
    }.elsewhen(csrrj === 1.U) {
      io.out.bits.ctrl.fuOpType := La32rCSROpType.csrwr
      io.out.bits.ctrl.rfSrc2 := rd
      io.out.bits.ctrl.src1Type := SrcType.imm
      io.out.bits.ctrl.src2Type := SrcType.reg
    }.otherwise {
      io.out.bits.ctrl.fuOpType := La32rCSROpType.csrxchg
      io.out.bits.ctrl.rfSrc1 := rj
      io.out.bits.ctrl.rfSrc2 := rd
      io.out.bits.ctrl.src1Type := SrcType.reg
      io.out.bits.ctrl.src2Type := SrcType.reg
    }
  }

  io.out.valid := io.in.valid
  io.in.ready := !io.in.valid || io.out.fire() && !hasIntr
  io.out.bits.cf <> io.in.bits

  LADebug(io.out.fire, "[IDU] pc=0x%x,instr=0x%x,fuType=%d,fuOpType=%d,rfSrc1=%d,rfSrc2=%d,imm=0x%x,src1Type=%d,src2Type=%d,rd=%d,rj=%d,rk=%d\n",
    io.in.bits.pc, io.in.bits.instr, io.out.bits.ctrl.fuType, io.out.bits.ctrl.fuOpType, io.out.bits.ctrl.rfSrc1,
    io.out.bits.ctrl.rfSrc2, io.out.bits.data.imm, io.out.bits.ctrl.src1Type, io.out.bits.ctrl.src2Type, rd, rj, rk)

  val intrVec = WireInit(0.U(13.W))
  BoringUtils.addSink(intrVec, "intrVecIDU")
  io.out.bits.cf.intrVec.zip(intrVec.asBools).map { case (x, y) => x := y }
  hasIntr := intrVec.orR

  io.out.bits.cf.exceptionVec := io.in.bits.exceptionVec
  io.out.bits.cf.exceptionVec(INT) := io.in.valid && hasIntr
  io.out.bits.cf.exceptionVec(INE) := (instrType === InstrN && !hasIntr) && io.in.valid

  io.out.bits.ctrl.isNutCoreTrap := (instr === La32rNutCoreTrap.TRAP) && io.in.valid
  // TODO : just for debug
  when (io.out.bits.cf.exceptionVec(illegalInstr) === true.B) {
    LADebug(true.B, "decode detecte illegal instr=0x%x\n", instr)
//    assert(false.B)
  }


}

class IDU(implicit val p: NutCoreConfig) extends NutCoreModule with HasInstrType {
  val io = IO(new Bundle {
    val in = Vec(2, Flipped(Decoupled(new CtrlFlowIO)))
    val out = Vec(2, Decoupled(new DecodeIO))
  })
  val decoder1  = if (IsLa32r) Module(new La32rDecoder) else Module(new Decoder)
  val decoder2  = if (IsLa32r) Module(new La32rDecoder) else Module(new Decoder)
  io.in(0) <> decoder1.io.in
  io.in(1) <> decoder2.io.in
  io.out(0) <> decoder1.io.out
  io.out(1) <> decoder2.io.out
  if(!EnableMultiIssue){
    io.in(1).ready := false.B
    decoder2.io.in.valid := false.B
  }

  val checkpoint_id = RegInit(0.U(64.W))

  // debug runahead
  val runahead = Module(new DifftestRunaheadEvent)
  runahead.io.clock         := clock
  runahead.io.coreid        := 0.U
  runahead.io.valid         := io.out(0).fire()
  runahead.io.branch        := decoder1.io.isBranch
  runahead.io.pc            := io.out(0).bits.cf.pc
  runahead.io.checkpoint_id := checkpoint_id
  when(runahead.io.valid && runahead.io.branch) {
    checkpoint_id := checkpoint_id + 1.U // allocate a new checkpoint_id
  }
  io.out(0).bits.cf.isBranch := decoder1.io.isBranch
  io.out(0).bits.cf.runahead_checkpoint_id := checkpoint_id
  // when(runahead.io.valid) {
  //   printf("fire pc %x branch %x inst %x\n", runahead.io.pc, runahead.io.branch, io.out(0).bits.cf.instr)
  // }

  if (!p.FPGAPlatform && !IsLa32r) {
    BoringUtils.addSource(decoder1.io.isWFI | decoder2.io.isWFI, "isWFI")
  }
}
