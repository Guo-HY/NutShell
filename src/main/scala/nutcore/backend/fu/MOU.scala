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

// memory order unit
object MOUOpType {
  def fence  = "b00".U
  def fencei = "b01".U
  def sfence_vma = "b10".U
}

object La32rMOUOpType {
  def dbar = "b00".U
  def ibar = "b01".U
}

class MOUIO extends FunctionUnitIO {
  val cfIn = Flipped(new CtrlFlowIO)
  val redirect = new RedirectIO
}

abstract class AbstractMOU(implicit val p: NutCoreConfig)extends NutCoreModule {
  val io = IO(new MOUIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)

  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }
}

class MOU(implicit override val p: NutCoreConfig) extends AbstractMOU {

  io.redirect.target := io.cfIn.pc + 4.U
  io.redirect.valid := valid
  io.redirect.rtype := 0.U
  val flushICache = valid && (func === MOUOpType.fencei)
  BoringUtils.addSource(flushICache, "MOUFlushICache")
  Debug(flushICache, "Flush I$ at %x\n", io.cfIn.pc)

  val flushTLB = valid && (func === MOUOpType.sfence_vma)
  BoringUtils.addSource(flushTLB, "MOUFlushTLB")
  Debug(flushTLB, "Sfence.vma at %x\n", io.cfIn.pc)

  io.out.bits := 0.U
  io.in.ready := true.B
  io.out.valid := valid
}

class La32rMOU(implicit override val p: NutCoreConfig) extends AbstractMOU {

  val s_idle :: s_dcacheReq :: s_dcacheResp :: s_icacheReq :: s_icacheResp :: s_finish :: Nil = Enum(6)
  val state = RegInit(s_idle)

  val flushICache = WireInit(false.B)
  val flushDCache = WireInit(false.B)
  val dcacheFlushDone = WireInit(false.B)
  val icacheFlushDone = WireInit(false.B)
  BoringUtils.addSource(flushICache, "FLUSH_ICACHE")
  BoringUtils.addSource(flushDCache, "FLUSH_DCACHE")
  BoringUtils.addSink(dcacheFlushDone, "DCACHE_FLUSH_DONE")
  BoringUtils.addSink(icacheFlushDone, "ICACHE_FLUSH_DONE")

  val holdPc = HoldUnless(io.cfIn.pc, io.in.fire)

  switch (state) {
    is (s_idle) {
      when (valid && func === La32rMOUOpType.ibar) { state := s_dcacheReq }
      when (valid && func === La32rMOUOpType.dbar) { state := s_finish }
    }
    is (s_dcacheReq) {
      state := s_dcacheResp
    }
    is (s_dcacheResp) {
      when (dcacheFlushDone) { state := s_icacheReq }
    }
    is (s_icacheReq) {
      state := s_icacheResp
    }
    is (s_icacheResp) {
      when (icacheFlushDone) { state := s_finish }
    }
    is (s_finish) {
      when (io.out.fire) { state := s_idle }
    }
  }

  flushDCache := state === s_dcacheReq
  flushICache := state === s_icacheReq

  io.in.ready := state === s_idle
  io.out.valid := state === s_finish

  io.redirect.valid := state === s_finish
  io.redirect.target := holdPc + 4.U
  io.redirect.rtype := 0.U

  io.out.bits := 0.U


}
