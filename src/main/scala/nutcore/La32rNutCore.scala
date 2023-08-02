package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import bus.simplebus._
import bus.axi4._
import utils._
import top.Settings

class La32rNutCore(implicit val p: NutCoreConfig) extends NutCoreModule {
  val io = IO(new Bundle() {
    val cachedMem = new SimpleBusUC(addrBits = PAddrBits)
    val uncachedMem = new SimpleBusUC(addrBits = PAddrBits)
    val ipi = Input(Bool()) // inter-core interrupt for la32r
    val hwi = Input(UInt(8.W)) // hardware interrupt for la32r
  })

  println("dmmuUserBits=", dmmuUserBits)
  println("immuUserBits=", immuUserBits)

  val frontend = Module(new Frontend_embedded)

  val backend = Module(new Backend_inorder)

  PipelineVector2Connect(new DecodeIO, frontend.io.out(0), frontend.io.out(1), backend.io.in(0), backend.io.in(1), frontend.io.flushVec(1), 4)

  val cachedXbar = Module(new SimpleBusCrossbarNto1(2))
  val uncachedXbar = Module(new SimpleBusCrossbarNto1(2))

  val immu = La32rMMU(in = frontend.io.imem, enable = HasIMMU)(La32rMMUConfig(name = "immu", userBits = immuUserBits, tlbEntryNum = Settings.getInt("TlbEntryNum"), FPGAPlatform = p.FPGAPlatform))

  val icache = La32rCache(in = immu.io.out, mmio = uncachedXbar.io.in(1), flush = Fill(2, frontend.io.flushVec(0) | frontend.io.bpFlush), enable = HasIcache)(La32rCacheConfig(ro = true, name = "icache", userBits = immuUserBits, totalSize = Settings.getInt("IcacheSize")))

  val dmmu = La32rMMU(in = backend.io.dmem, enable = HasDMMU)(La32rMMUConfig(name = "dmmu", userBits = dmmuUserBits, tlbEntryNum = Settings.getInt("TlbEntryNum"), FPGAPlatform = p.FPGAPlatform))

  val dcache = La32rCache(in = dmmu.io.out, mmio = uncachedXbar.io.in(0), flush = "b00".U, enable = HasDcache)(La32rCacheConfig(ro = false, name = "dcache", userBits = dmmuUserBits, totalSize = Settings.getInt("DcacheSize")))

  cachedXbar.io.in(0) <> dcache.io.out.mem
  cachedXbar.io.in(1) <> icache.io.out.mem

  io.cachedMem <> cachedXbar.io.out
  io.uncachedMem <> uncachedXbar.io.out

  icache.io.out.coh <> DontCare
  dcache.io.out.coh <> DontCare

  // redirect
  frontend.io.redirect <> backend.io.redirect
  backend.io.flush := frontend.io.flushVec(3, 2)

  frontend.io.ipf := DontCare
  backend.io.memMMU <> DontCare

  BoringUtils.addSource(io.ipi, "ipi")
  BoringUtils.addSource(io.hwi, "hwi")

  val s_idle :: s_icache :: s_dcache :: Nil = Enum(3)
  val cacheSemaphore = RegInit(s_idle)
  val icacheTryGetSem = WireInit(false.B)
  val dcacheTryGetSem = WireInit(false.B)
  val sendICacheSem = WireInit(false.B)
  val sendDCacheSem = WireInit(false.B)
  val icacheReleaseSem = WireInit(false.B)
  val dcacheReleaseSem = WireInit(false.B)
  BoringUtils.addSink(icacheTryGetSem, "icacheTryGetSem")
  BoringUtils.addSink(dcacheTryGetSem, "dcacheTryGetSem")
  BoringUtils.addSource(sendICacheSem, "sendICacheSem")
  BoringUtils.addSource(sendDCacheSem, "sendDCacheSem")
  BoringUtils.addSink(icacheReleaseSem, "icacheReleaseSem")
  BoringUtils.addSink(dcacheReleaseSem, "dcacheReleaseSem")

  sendICacheSem := (cacheSemaphore === s_idle) && icacheTryGetSem && !dcacheTryGetSem
  sendDCacheSem := (cacheSemaphore === s_idle) && dcacheTryGetSem
  switch (cacheSemaphore) {
    is (s_idle) {
      when (sendICacheSem) {
        cacheSemaphore := s_icache
      }
      when (sendDCacheSem) {
        cacheSemaphore := s_dcache
      }
    }
    is (s_icache) {
      when (icacheReleaseSem) { cacheSemaphore := s_idle }
    }
    is (s_dcache) {
      when (dcacheReleaseSem) { cacheSemaphore := s_idle }
    }
  }

}
