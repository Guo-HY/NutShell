package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import bus.simplebus._
import bus.axi4._
import chisel3.experimental.IO
import utils._
import top.Settings

sealed case class La32rMMUConfig (
  name: String = "mmu",
  userBits: Int = 0,
)

trait HasLa32rMMUConst {
  implicit val la32rMMUConfig: La32rMMUConfig

  val userBits = la32rMMUConfig.userBits
}

trait HasLa32rMMUIO extends HasNutCoreParameter with HasLa32rMMUConst {
  class La32rMMUIO extends Bundle {
    val in = Flipped(new SimpleBusUC(userBits = userBits, addrBits = VAddrBits))
    val out = new SimpleBusUC(userBits = userBits, addrBits = PAddrBits)
    val memoryAccessType = Output(UInt(2.W))
  }
  val io = IO(new La32rMMUIO)
}


class La32rMMU(implicit val la32rMMUConfig: La32rMMUConfig) extends NutCoreModule with HasLa32rMMUIO with HasLa32rCSRConst {
  val isIMMU = name == "immu" // otherwise is dmmu

  val CRMD = WireInit(0.U(32.W))
  val DMW0 = WireInit(0.U(32.W))
  val DMW1 = WireInit(0.U(32.W))

  BoringUtils.addSink(CRMD, "CRMD")
  BoringUtils.addSink(DMW0, "DMW0")
  BoringUtils.addSink(DMW1, "DMW1")

  val crmdStruct = CRMD.asTypeOf(new CRMDStruct)
  val dmw0Struct = DMW0.asTypeOf(new DMWStruct)
  val dmw1Struct = DMW1.asTypeOf(new DMWStruct)

  io.out <> io.in

  val isDAT = crmdStruct.DA === 1.U && crmdStruct.PG === 0.U // is Direct Address Translation
  val DATPaddr = io.in.req.bits.addr(PAddrBits - 1, 0)
  val DATMAT = if (isIMMU) crmdStruct.DATF else crmdStruct.DATM // Direct Address Translation's Memory Access Type

  val PLVoh = UIntToOH(crmdStruct.PLV)
  val DMWVec = Seq(dmw0Struct, dmw1Struct)
  val DMWHitVec = DMWVec.map(dmw => io.in.req.bits.addr(31, 29) === dmw.VSEG && (PLVoh & Cat(dmw.PLV3, dmw.pad2, dmw.PLV0)).orR)
  val isDMAT = DMWHitVec.reduce(_||_) // is Direct Mapped Address Translation

  val DMATPaddr = Mux(DMWHitVec(0), Cat(DMWVec(0).PSEG, io.in.req.bits.addr(28, 0))(PAddrBits - 1, 0),Cat(DMWVec(1).PSEG, io.in.req.bits.addr(28, 0))(PAddrBits - 1, 0))
  val DMATMAT = Mux(DMWHitVec(0), DMWVec(0).MAT, DMWVec(1).MAT)

  val isTLB = !isDAT && !isDMAT
  assert(isDAT || isDMAT) // TODO : add tlb support

  io.out.req.bits.addr := Mux(isDAT, DATPaddr, DMATPaddr)
  io.memoryAccessType := Mux(isDAT, DATMAT, DMATMAT)

}

class La32rMMU_fake(implicit val la32rMMUConfig: La32rMMUConfig) extends NutCoreModule with HasLa32rMMUIO {
  val CRMD = WireInit(0.U(32.W))
  val DMW0 = WireInit(0.U(32.W))
  val DMW1 = WireInit(0.U(32.W))

  BoringUtils.addSink(CRMD, "CRMD")
  BoringUtils.addSink(DMW0, "DMW0")
  BoringUtils.addSink(DMW1, "DMW1")

  io.out <> io.in
  io.memoryAccessType := 0.U
}

object La32rMMU {
  def apply(in: SimpleBusUC, enable: Boolean = true)(implicit la32rMMUConfig: La32rMMUConfig) = {
    val mmu = if(enable) {
      Module(new La32rMMU())
    } else {
      Module(new La32rMMU_fake())
    }
    mmu.io.in <> in
    mmu
  }
}