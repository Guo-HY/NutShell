package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import bus.simplebus._
import bus.axi4._
import chisel3.experimental.IO
import utils._
import top.Settings
import sim.DeviceSpace

trait HasLa32rCacheIO {
  implicit val cacheConfig: CacheConfig
  class La32rCacheIO(implicit val cacheConfig: CacheConfig) extends Bundle with HasNutCoreParameter with HasCacheConst {
    val in = Flipped(new SimpleBusUC(userBits = userBits, idBits = idBits))
    val flush = Input(UInt(2.W))
    val out = new SimpleBusC
    val mmio = new SimpleBusUC
    val mat = Input(UInt(2.W))
  }
  val io = IO(new La32rCacheIO())
}

class La32rCache_fake(implicit val cacheConfig: CacheConfig) extends CacheModule with HasLa32rCacheIO with HasLa32rCSRConst {
  assert(cacheName == "icache" || cacheName == "dcache")
  val s_idle :: s_memReq :: s_memResp :: s_mmioReq :: s_mmioResp :: s_wait_resp :: Nil = Enum(6)
  val state = RegInit(s_idle)

  val ismmio = io.mat === StronglyOrderedUncached
  val ismmioRec = RegEnable(ismmio, io.in.req.fire())

  val hasTlbExcp = WireInit(false.B)

  val needFlush = RegInit(false.B)
  when (io.flush(0) && (state =/= s_idle)) { needFlush := true.B }
  when (state === s_idle && needFlush) { needFlush := false.B }

  val alreadyOutFire = RegEnable(true.B, init = false.B, io.in.resp.fire())

  val isInvalidAddr = false.B// !PMA.isValidAddr(io.in.req.bits.addr)

  switch (state) {
    is (s_idle) {
      alreadyOutFire := false.B
      when (io.in.req.fire() && !io.flush(0)) { state := Mux(isInvalidAddr, s_wait_resp, Mux(ismmio, s_mmioReq, s_memReq)) }
    }
    is (s_memReq) {
      when (hasTlbExcp) { state := s_wait_resp }
      .elsewhen (io.out.mem.req.fire()) { state := s_memResp }
    }
    is (s_memResp) {
      when (io.out.mem.resp.fire()) { state := s_wait_resp }
    }
    is (s_mmioReq) {
      when (hasTlbExcp) { state := s_wait_resp }
      .elsewhen (io.mmio.req.fire()) { state := s_mmioResp }
    }
    is (s_mmioResp) {
      when (io.mmio.resp.fire() || alreadyOutFire) { state := s_wait_resp }
    }
    is (s_wait_resp) {
      when (io.in.resp.fire() || needFlush || alreadyOutFire) { state := s_idle }
    }
  }

  val reqaddr = RegEnable(io.in.req.bits.addr, io.in.req.fire())
  val cmd = RegEnable(io.in.req.bits.cmd, io.in.req.fire())
  val size = RegEnable(io.in.req.bits.size, io.in.req.fire())
  val wdata = RegEnable(io.in.req.bits.wdata, io.in.req.fire())
  val wmask = RegEnable(io.in.req.bits.wmask, io.in.req.fire())

  io.in.req.ready := (state === s_idle)
  io.in.resp.valid := (state === s_wait_resp) && (!needFlush)

  val mmiordata = RegEnable(io.mmio.resp.bits.rdata, io.mmio.resp.fire())
  val mmiocmd = RegEnable(io.mmio.resp.bits.cmd, io.mmio.resp.fire())
  val memrdata = RegEnable(io.out.mem.resp.bits.rdata, io.out.mem.resp.fire())
  val memcmd = RegEnable(io.out.mem.resp.bits.cmd, io.out.mem.resp.fire())

  io.in.resp.bits.rdata := Mux(ismmioRec, mmiordata, memrdata)
  io.in.resp.bits.cmd := Mux(ismmioRec, mmiocmd, memcmd)

  val memuser = RegEnable(io.in.req.bits.user.getOrElse(0.U), io.in.req.fire())
  io.in.resp.bits.user.zip(if (userBits > 0) Some(memuser) else None).map { case (o,i) => o := i }

  if (cacheName == "icache") {
    hasTlbExcp := memuser.asTypeOf(new ImmuUserBundle).tlbExcp.asUInt().orR
  } else {
    hasTlbExcp := memuser.asTypeOf(new DmmuUserBundle).tlbExcp.asUInt().orR
  }

  io.out.mem.req.bits.apply(addr = reqaddr,
    cmd = cmd, size = size,
    wdata = wdata, wmask = wmask)
  io.out.mem.req.valid := (state === s_memReq && !hasTlbExcp)
  io.out.mem.resp.ready := true.B

  io.mmio.req.bits.apply(addr = reqaddr,
    cmd = cmd, size = size,
    wdata = wdata, wmask = wmask)
  io.mmio.req.valid := (state === s_mmioReq && !hasTlbExcp)
  io.mmio.resp.ready := true.B

  io.out.coh := DontCare

  val cacopValid = WireInit(false.B)
  val cacopCode = WireInit(0.U(5.W))
  val cacopVA = WireInit(0.U(VAddrBits.W))
  val cacopPA = WireInit(0.U(PAddrBits.W))
  BoringUtils.addSink(cacopValid, "CACOP_VALID")
  BoringUtils.addSink(cacopCode, "CACOP_CODE")
  BoringUtils.addSink(cacopVA, "CACOP_VA")
  BoringUtils.addSink(cacopPA, "CACOP_PA")

  // ibar
  val flushICache = WireInit(false.B)
  val flushDCache = WireInit(false.B)
  val dcacheFlushDone = WireInit(false.B)
  val icacheFlushDone = WireInit(false.B)
  BoringUtils.addSink(flushICache, "FLUSH_ICACHE")
  BoringUtils.addSink(flushDCache, "FLUSH_DCACHE")
  if (cacheName == "icache") {
    BoringUtils.addSource(icacheFlushDone, "ICACHE_FLUSH_DONE")
  } else {
    BoringUtils.addSource(dcacheFlushDone, "DCACHE_FLUSH_DONE")
  }

  val flush = Mux((cacheName == "icache").asBool(), flushICache, flushDCache)
  val s_flush_idle :: s_flush_doing :: s_flush_done :: Nil = Enum(3)
  val flush_state = RegInit(s_flush_idle)
  val flush_counter = RegInit(0.U(5.W))
  switch (flush_state) {
    is (s_flush_idle) {
      when (flush) {
        flush_state := s_flush_doing
        flush_counter := 0.U
      }
    }
    is (s_flush_doing) {
      flush_counter := flush_counter + 1.U // just for simulate flush time delay
      when (flush_counter === 16.U) { flush_state := s_flush_done }
    }
    is (s_flush_done) { flush_state := s_flush_idle }
  }
  dcacheFlushDone := flush_state === s_flush_done
  icacheFlushDone := flush_state === s_flush_done


  Debug(io.in.req.fire(), p"in.req: ${io.in.req.bits}\n")
  Debug(io.out.mem.req.fire(), p"out.mem.req: ${io.out.mem.req.bits}\n")
  Debug(io.out.mem.resp.fire(), p"out.mem.resp: ${io.out.mem.resp.bits}\n")
  Debug(io.in.resp.fire(), p"in.resp: ${io.in.resp.bits}\n")
}

object La32rCache {
  def apply(in: SimpleBusUC, mmio: SimpleBusUC, flush: UInt, mat: UInt, enable: Boolean = false)(implicit cacheConfig: CacheConfig) = {
    assert(enable == false)
    val cache = Module(new La32rCache_fake())
    cache.io.in <> in
    cache.io.flush := flush
    mmio <> cache.io.mmio
    cache.io.mat := mat
    cache
  }
}