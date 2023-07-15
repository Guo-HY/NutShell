package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utils._
import bus.simplebus._
import difftest.DifftestStoreEvent
import sim.DeviceSpace
import top.Settings

object La32rLSUOpType {
  // Op(1,0) = 00 : byte operation
  // Op(1,0) = 01 : half operation
  // Op(1,0) = 10 : word operation
  // Op(3) = 1 : unsigned load
  // Op(4) = 1 : load, 0 : store
  //
  def lb  = "b0010000".U
  def lh  = "b0010001".U
  def lw  = "b0010010".U
  def lbu = "b0011000".U
  def lhu = "b0011001".U
  def sb  = "b0000000".U
  def sh  = "b0000001".U
  def sw  = "b0000010".U

  def isLoad(func: UInt) = func(4)
  def isStore(func: UInt) = !func(4)
  def isUnsignedLoad(func: UInt) = func(4) && func(3)
  def isByteOp(func: UInt) = func(1, 0) === "b00".U
  def isHalfOp(func: UInt) = func(1, 0) === "b01".U
  def isWordOp(func: UInt) = func(1, 0) === "b10".U
}

// for load & store operation , vaddr = src1 + SignExt(src2(11, 0), 32)
// for store operation, use wdata as store data
class La32rUnpipelinedLSU(implicit override val p: NutCoreConfig) extends AbstractUnpipelinedLSU with HasMemAccessMaster {

  def genWmask(addr: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> 0x1.U, //0001 << addr(1:0)
      "b01".U -> 0x3.U, //0011
      "b10".U -> 0xf.U //1111
    )) << addr(1, 0)
  }

  def genWdata(data: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> Fill(4, data(7, 0)),
      "b01".U -> Fill(2, data(15, 0)),
      "b10".U -> data
    ))
  }

  // because UnpipelinedLSU is a multi cycle unit, so we need "hold" input to ensure
  // input data does not change during processing
  val holdSrc1 = HoldUnless(src1, io.in.fire)
  val holdSrc2 = HoldUnless(src2, io.in.fire)
  val holdFunc = HoldUnless(func, io.in.fire)
  val holdWdata = HoldUnless(io.wdata, io.in.fire)

  val vaddr = holdSrc1 + holdSrc2
  val dmem = io.dmem
  val isStore = La32rLSUOpType.isStore(holdFunc)
  val partialLoad = La32rLSUOpType.isLoad(holdFunc) && (holdFunc =/= La32rLSUOpType.lw)

  val s_idle :: s_wait_resp :: s_partialLoad :: Nil = Enum(3)
  val state = RegInit(s_idle)

  switch(state) {
    is(s_idle) {
      when(dmem.req.fire) { state := s_wait_resp }
    }
    is(s_wait_resp) {
      when(dmem.resp.fire) { state := Mux(partialLoad, s_partialLoad, s_idle) }
    }
    is(s_partialLoad) { when (io.out.fire) { state := s_idle } }
  }

  val size = holdFunc(1, 0)

  val reqUserBits = Wire(new DmmuUserBundle)
  reqUserBits.isDeviceLoad := !isStore
  reqUserBits.memAccessMaster := Mux(isStore, STORE, LOAD)
  reqUserBits.tlbExcp := 0.U.asTypeOf(reqUserBits.tlbExcp)
  reqUserBits.paddr := 0.U
  reqUserBits.isInvalidPaddr := false.B

  dmem.req.bits.apply(
    addr = vaddr,
    size = size,
    wdata = genWdata(holdWdata, size),
    wmask = genWmask(vaddr, size),
    cmd = Mux(isStore, SimpleBusCmd.write, SimpleBusCmd.read),
    user = reqUserBits.asUInt())
  dmem.req.valid := valid && (state === s_idle)
  dmem.resp.ready := partialLoad || io.out.ready

  io.in.ready := (state === s_idle) && dmem.req.ready
  io.out.valid := Mux(partialLoad, state === s_partialLoad, dmem.resp.fire && (state === s_wait_resp))

  val rdata = dmem.resp.bits.rdata
  val rdataDelay1 = RegNext(rdata)

  val rdataSel = LookupTree(vaddr(1, 0), List(
    "b00".U -> rdataDelay1(31, 0),
    "b01".U -> rdataDelay1(31, 8),
    "b10".U -> rdataDelay1(31, 16),
    "b11".U -> rdataDelay1(31, 24)
  ))

  val rdataPartialLoad = LookupTree(holdFunc, List(
    La32rLSUOpType.lb   -> SignExt(rdataSel(7, 0), XLEN),
    La32rLSUOpType.lh   -> SignExt(rdataSel(15, 0), XLEN),
    La32rLSUOpType.lw   -> SignExt(rdataSel(31, 0), XLEN),
    La32rLSUOpType.lbu  -> ZeroExt(rdataSel(7, 0), XLEN),
    La32rLSUOpType.lhu  -> ZeroExt(rdataSel(15, 0), XLEN)
  ))

  io.out.bits := Mux(partialLoad, rdataPartialLoad, rdata(XLEN - 1, 0))

  val addrAligned = LookupTree(holdFunc(1, 0), List(
    "b00".U -> true.B,
    "b01".U -> (vaddr(0) === 0.U),
    "b10".U -> (vaddr(1, 0) === 0.U)
  ))

  val respUserBits = HoldUnless(dmem.resp.bits.user.get.asTypeOf(new DmmuUserBundle), dmem.resp.fire)

  io.la32rExcp.hasExcp := (io.la32rExcp.ale | respUserBits.tlbExcp.asUInt().orR) && io.out.valid
  io.la32rExcp.ale := io.out.valid && !addrAligned
  io.la32rExcp.badv := vaddr
  io.la32rExcp.tlbExcp := respUserBits.tlbExcp


  val isReadDevice = HoldReleaseLatch(valid=dmem.resp.valid && respUserBits.isDeviceLoad.asBool(), release=io.out.fire, flush=false.B)

  io.isMMIO := isReadDevice

  Debug(io.la32rExcp.ale, "misaligned addr detected\n")

  LADebug(dmem.req.fire, "[LSUREQ]pc=0x%x instr=0x%x vaddr=0x%x size=%d wdata=0x%x wmask=0x%x cmd=%d\n",
    io.pc, io.instr, vaddr, size, dmem.req.bits.wdata, dmem.req.bits.wmask, dmem.req.bits.cmd)

  LADebug(io.out.fire, "[LSURESP]pc=0x%x instr=0x%x rdata=0x%x\n", io.pc, io.instr, io.out.bits)

  // below is temp code to pass firrtl compile
  val lr = WireInit(Bool(), false.B)
  BoringUtils.addSink(lr, "lr")
  val lrAddr = WireInit(UInt(AddrBits.W), DontCare)
  BoringUtils.addSink(lrAddr, "lr_addr")
  io.loadAddrMisaligned := DontCare
  io.storeAddrMisaligned := DontCare
  val dtlbFinish = WireInit(false.B)
  val dtlbEnable = WireInit(false.B)
  if (Settings.get("HasDTLB")) {
    BoringUtils.addSink(dtlbFinish, "DTLBFINISH")
    BoringUtils.addSink(dtlbEnable, "DTLBENABLE")
  }
  io.dtlbPF := DontCare



  // storeData format need align with la32r-nemu,(see NEMU/src/memory/paddr.c : store_commit_queue_push)
  io.storeCheck.valid := HoldReleaseLatch(valid=dmem.req.fire && isStore,release=io.out.fire, flush = false.B) && !respUserBits.isInvalidPaddr
  val offset = vaddr(1, 0)
  io.storeCheck.storeAddr := respUserBits.paddr
  io.storeCheck.storeData := Mux(La32rLSUOpType.isByteOp(holdFunc), (holdWdata & 0xff.U) << (offset << 3),
        Mux(La32rLSUOpType.isHalfOp(holdFunc), (holdWdata & 0xffff.U) << (offset << 3), holdWdata))

}
