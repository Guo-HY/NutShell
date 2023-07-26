package nutcore

import chisel3._
import chisel3.util._

object LA32R_ALUInstr extends HasLa32rInstrType with HasNutCoreParameter {
  def ADDW    = BitPat("b0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0_?????_?????_?????")
  def SUBW    = BitPat("b0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0_?????_?????_?????")
  def SLT     = BitPat("b0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0_?????_?????_?????")
  def SLTU    = BitPat("b0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 1_?????_?????_?????")
  def NOR     = BitPat("b0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0_?????_?????_?????")
  def AND     = BitPat("b0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 1_?????_?????_?????")
  def OR      = BitPat("b0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0_?????_?????_?????")
  def XOR     = BitPat("b0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 1_?????_?????_?????")
  def SLLW    = BitPat("b0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 1 0_?????_?????_?????")
  def SRLW    = BitPat("b0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 1 1_?????_?????_?????")
  def SRAW    = BitPat("b0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0_?????_?????_?????")
  def SLLIW   = BitPat("b0 0 0 0 0 0 0 0 0 1 0 0 0 0_001_?????_?????_?????")
  def SRLIW   = BitPat("b0 0 0 0 0 0 0 0 0 1 0 0 0 1_001_?????_?????_?????")
  def SRAIW   = BitPat("b0 0 0 0 0 0 0 0 0 1 0 0 1 0_001_?????_?????_?????")
  def SLTI    = BitPat("b0 0 0 0 0 0 1 0 0 0_????????????_?????_?????")
  def SLTUI   = BitPat("b0 0 0 0 0 0 1 0 0 1_????????????_?????_?????")
  def ADDIW   = BitPat("b0 0 0 0 0 0 1 0 1 0_????????????_?????_?????")
  def ANDI    = BitPat("b0 0 0 0 0 0 1 1 0 1_????????????_?????_?????")
  def ORI     = BitPat("b0 0 0 0 0 0 1 1 1 0_????????????_?????_?????")
  def XORI    = BitPat("b0 0 0 0 0 0 1 1 1 1_????????????_?????_?????")
  def LU12IW  = BitPat("b0 0 0 1 0 1 0_????????????????????_?????")
  def PCADDU12I = BitPat("b0 0 0 1 1 1 0_????????????????????_?????")

  val table = Array( // (instrType, FuType, FuOpType, isrfWen)
    ADDW        -> List(Instr3R, FuType.alu, La32rALUOpType.add, true.B),
    SUBW        -> List(Instr3R, FuType.alu, La32rALUOpType.sub, true.B),
    ADDIW       -> List(Instr2RI12, FuType.alu, La32rALUOpType.add, true.B),
    SLT         -> List(Instr3R, FuType.alu, La32rALUOpType.slt, true.B),
    SLTU        -> List(Instr3R, FuType.alu, La32rALUOpType.sltu, true.B),
    NOR         -> List(Instr3R, FuType.alu, La32rALUOpType.nor, true.B),
    AND         -> List(Instr3R, FuType.alu, La32rALUOpType.and, true.B),
    OR          -> List(Instr3R, FuType.alu, La32rALUOpType.or, true.B),
    XOR         -> List(Instr3R, FuType.alu, La32rALUOpType.xor, true.B),
    SLLW        -> List(Instr3R, FuType.alu, La32rALUOpType.sll, true.B),
    SRLW        -> List(Instr3R, FuType.alu, La32rALUOpType.srl, true.B),
    SRAW        -> List(Instr3R, FuType.alu, La32rALUOpType.sra, true.B),
    SLLIW       -> List(Instr2RI5, FuType.alu, La32rALUOpType.sll, true.B),
    SRLIW       -> List(Instr2RI5, FuType.alu, La32rALUOpType.srl, true.B),
    SRAIW       -> List(Instr2RI5, FuType.alu, La32rALUOpType.sra, true.B),
    SLTI        -> List(Instr2RI12, FuType.alu, La32rALUOpType.slt, true.B),
    SLTUI       -> List(Instr2RI12, FuType.alu, La32rALUOpType.sltu, true.B),
    ANDI        -> List(Instr2RI12ZEXT, FuType.alu, La32rALUOpType.and, true.B),
    ORI         -> List(Instr2RI12ZEXT, FuType.alu, La32rALUOpType.or, true.B),
    XORI        -> List(Instr2RI12ZEXT, FuType.alu, La32rALUOpType.xor, true.B),
    LU12IW      -> List(Instr1RI20, FuType.alu, La32rALUOpType.lu12i, true.B),
    PCADDU12I   -> List(Instr1RI20, FuType.alu, La32rALUOpType.pcaddu12i, true.B),
  )

}

object LA32R_MDUInstr extends HasLa32rInstrType with HasNutCoreParameter {
  def MULW    = BitPat("b0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0_?????_?????_?????")
  def MULHW   = BitPat("b0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 1_?????_?????_?????")
  def MULHWU  = BitPat("b0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 1 0_?????_?????_?????")
  def DIVW    = BitPat("b0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0_?????_?????_?????")
  def MODW    = BitPat("b0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1_?????_?????_?????")
  def DIVWU   = BitPat("b0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0_?????_?????_?????")
  def MODWU   = BitPat("b0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 1_?????_?????_?????")

  val table = Array( // (instrType, FuType, FuOpType, isrfWen)
    MULW      -> List(Instr3R, FuType.mdu, MDUOpType.mul, true.B),
    MULHW     -> List(Instr3R, FuType.mdu, MDUOpType.mulh, true.B),
    MULHWU    -> List(Instr3R, FuType.mdu, MDUOpType.mulhu, true.B),
    DIVW      -> List(Instr3R, FuType.mdu, MDUOpType.div, true.B),
    DIVWU     -> List(Instr3R, FuType.mdu, MDUOpType.divu, true.B),
    MODW      -> List(Instr3R, FuType.mdu, MDUOpType.rem, true.B),
    MODWU     -> List(Instr3R, FuType.mdu, MDUOpType.remu, true.B),
  )
}

object LA32R_BRUInstr extends HasLa32rInstrType with HasNutCoreParameter {
  def JIRL  = BitPat("b0 1 0 0 1 1_????????????????_?????_?????")
  def B     = BitPat("b0 1 0 1 0 0_????????????????_??????????")
  def BL    = BitPat("b0 1 0 1 0 1_????????????????_??????????")
  def BEQ   = BitPat("b0 1 0 1 1 0_????????????????_?????_?????")
  def BNE   = BitPat("b0 1 0 1 1 1_????????????????_?????_?????")
  def BLT   = BitPat("b0 1 1 0 0 0_????????????????_?????_?????")
  def BGE   = BitPat("b0 1 1 0 0 1_????????????????_?????_?????")
  def BLTU  = BitPat("b0 1 1 0 1 0_????????????????_?????_?????")
  def BGEU  = BitPat("b0 1 1 0 1 1_????????????????_?????_?????")

  val table = Array( // (instrType, FuType, FuOpType, isrfWen)
    JIRL      -> List(Instr2RI16, FuType.bru, La32rALUOpType.jirl, true.B),
    B         -> List(InstrI26, FuType.bru, La32rALUOpType.b, false.B),
    BL        -> List(InstrI26, FuType.bru, La32rALUOpType.call, true.B),
    BEQ       -> List(InstrBranch, FuType.bru, La32rALUOpType.beq, false.B),
    BNE       -> List(InstrBranch, FuType.bru, La32rALUOpType.bne, false.B),
    BLT       -> List(InstrBranch, FuType.bru, La32rALUOpType.blt, false.B),
    BGE       -> List(InstrBranch, FuType.bru, La32rALUOpType.bge, false.B),
    BLTU      -> List(InstrBranch, FuType.bru, La32rALUOpType.bltu, false.B),
    BGEU      -> List(InstrBranch, FuType.bru, La32rALUOpType.bgeu, false.B),
  )

}

object LA32R_LSUInstr extends HasLa32rInstrType with HasNutCoreParameter {
  def LDB   = BitPat("b0 0 1 0 1 0 0 0 0 0_????????????_?????_?????")
  def LDH   = BitPat("b0 0 1 0 1 0 0 0 0 1_????????????_?????_?????")
  def LDW   = BitPat("b0 0 1 0 1 0 0 0 1 0_????????????_?????_?????")
  def STB   = BitPat("b0 0 1 0 1 0 0 1 0 0_????????????_?????_?????")
  def STH   = BitPat("b0 0 1 0 1 0 0 1 0 1_????????????_?????_?????")
  def STW   = BitPat("b0 0 1 0 1 0 0 1 1 0_????????????_?????_?????")
  def LDBU  = BitPat("b0 0 1 0 1 0 1 0 0 0_????????????_?????_?????")
  def LDHU  = BitPat("b0 0 1 0 1 0 1 0 0 1_????????????_?????_?????")
  def LLW = BitPat("b0 0 1 0 0 0 0 0_??????????????_?????_?????")
  def SCW = BitPat("b0 0 1 0 0 0 0 1_??????????????_?????_?????")

  val table = Array( // (instrType, FuType, FuOpType, isrfWen)
    LDB       -> List(Instr2RI12, FuType.lsu, La32rLSUOpType.lb, true.B),
    LDH       -> List(Instr2RI12, FuType.lsu, La32rLSUOpType.lh, true.B),
    LDW       -> List(Instr2RI12, FuType.lsu, La32rLSUOpType.lw, true.B),
    STB       -> List(InstrStore, FuType.lsu, La32rLSUOpType.sb, false.B),
    STH       -> List(InstrStore, FuType.lsu, La32rLSUOpType.sh, false.B),
    STW       -> List(InstrStore, FuType.lsu, La32rLSUOpType.sw, false.B),
    LDBU      -> List(Instr2RI12, FuType.lsu, La32rLSUOpType.lbu, true.B),
    LDHU      -> List(Instr2RI12, FuType.lsu, La32rLSUOpType.lhu, true.B),
    LLW       -> List(Instr2R, FuType.lsu, La32rLSUOpType.llw, true.B),
    SCW       -> List(InstrStore, FuType.lsu, La32rLSUOpType.scw, true.B),
  )
}

object LA32R_PRIVInstr extends HasLa32rInstrType with HasNutCoreParameter {

}

object LA32R_MISCInstr extends HasLa32rInstrType with HasNutCoreParameter {

  def PRELD     = BitPat("b0 0 1 0 1 0 1 0 1 1_????????????_?????_?????")
  def DBAR      = BitPat("b0 0 1 1 1 0 0 0 0 1 1 1 0 0 1 0 0_???????????????")
  def IBAR      = BitPat("b0 0 1 1 1 0 0 0 0 1 1 1 0 0 1 0 1_???????????????")

}

object LA32R_CSRInstr extends HasLa32rInstrType with HasNutCoreParameter {
  def CSR       = BitPat("b0 0 0 0 0 1 0 0_??????????????_?????_?????") // need distinguish CSRRD,CSRWR,CSRXCHG by (9,5)
  def RDCNTID   = BitPat("b0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0_?????_00000")
  def RDCNTVL   = BitPat("b0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0_00000_?????")
  def RDCNTVH   = BitPat("b0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1_00000_?????")
  def BREAK     = BitPat("b0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 0_???????????????")
  def SYSCALL   = BitPat("b0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 1 0_???????????????")
  def ERTN      = BitPat("b0 0 0 0 0 1 1 0 0 1 0 0 1 0 0 0 0 0 1 1 1 0_0 0 0 0 0_0 0 0 0 0")
  def TLBSRCH = BitPat("b0 0 0 0 0 1 1 0 0 1 0 0 1 0 0 0 0 0 1 0 1 0_0 0 0 0 0_0 0 0 0 0")
  def TLBRD = BitPat("b0 0 0 0 0 1 1 0 0 1 0 0 1 0 0 0 0 0 1 0 1 1_0 0 0 0 0_0 0 0 0 0")
  def TLBWR = BitPat("b0 0 0 0 0 1 1 0 0 1 0 0 1 0 0 0 0 0 1 1 0 0_0 0 0 0 0_0 0 0 0 0")
  def TLBFILL = BitPat("b0 0 0 0 0 1 1 0 0 1 0 0 1 0 0 0 0 0 1 1 0 1_0 0 0 0 0_0 0 0 0 0")
  def INVTLB = BitPat("b0 0 0 0 0 1 1 0 0 1 0 0 1 0 0 1 1_?????_?????_?????")
  def CACOP     = BitPat("b0 0 0 0 0 1 1 0 0 0_????????????_?????_?????")
  def IDLE      = BitPat("b0 0 0 0 0 1 1 0 0 1 0 0 1 0 0 0 1_???????????????")

  val table = Array(// (instrType, FuType, FuOpType, isrfWen)
    CSR     -> List(Instr2R, FuType.csr, La32rCSROpType.csracc, true.B),
    RDCNTID -> List(Instr1RI20, FuType.csr, La32rCSROpType.rdcntid, true.B),
    RDCNTVL -> List(Instr1RI20, FuType.csr, La32rCSROpType.rdcntvl, true.B),
    RDCNTVH -> List(Instr1RI20, FuType.csr, La32rCSROpType.rdcntvh, true.B),
    BREAK   -> List(InstrCODE15, FuType.csr, La32rCSROpType.break, false.B),
    SYSCALL -> List(InstrCODE15, FuType.csr, La32rCSROpType.syscall, false.B),
    ERTN    -> List(InstrCODE15, FuType.csr, La32rCSROpType.ertn, false.B),
    TLBSRCH -> List(InstrCODE15, FuType.csr, La32rCSROpType.tlbsrch, false.B),
    TLBRD   -> List(InstrCODE15, FuType.csr, La32rCSROpType.tlbrd, false.B),
    TLBWR   -> List(InstrCODE15, FuType.csr, La32rCSROpType.tlbwr, false.B),
    TLBFILL -> List(InstrCODE15, FuType.csr, La32rCSROpType.tlbfill, false.B),
    INVTLB  -> List(Instr3R, FuType.csr, La32rCSROpType.invtlb, false.B),
    CACOP   -> List(Instr2R, FuType.csr, La32rCSROpType.cacop, false.B),
    IDLE    -> List(InstrCODE15, FuType.csr, La32rCSROpType.idle, false.B),
  )
}