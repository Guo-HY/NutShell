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
    val mem = new SimpleBusUC(addrBits = PAddrBits)
  })

  val frontend = Module(new Frontend_embedded)

  val backend = Module(new Backend_inorder)

  PipelineVector2Connect(new DecodeIO, frontend.io.out(0), frontend.io.out(1), backend.io.in(0), backend.io.in(1), frontend.io.flushVec(1), 4)

  val memXbar = Module(new SimpleBusCrossbarNto1(4))

  val immu = La32rMMU(in = frontend.io.imem, enable = HasIMMU)(La32rMMUConfig(name = "immu", userBits = immuUserBits))

  val icache = La32rCache(in = immu.io.out, mmio = memXbar.io.in(2), flush = Fill(2, frontend.io.flushVec(0) | frontend.io.bpFlush), mat = immu.io.memoryAccessType, enable = HasIcache)(CacheConfig(ro = true, name = "icache", userBits = ICacheUserBundleWidth))

  val dmmu = La32rMMU(in = backend.io.dmem, enable = HasDMMU)(La32rMMUConfig(name = "dmmu", userBits = dmmuUserBits))

  val dcache = La32rCache(in = dmmu.io.out, mmio = memXbar.io.in(0), flush = "b00".U, mat = dmmu.io.memoryAccessType, enable = HasDcache)(CacheConfig(ro = false, name = "dcache", userBits = dmmuUserBits))

  memXbar.io.in(1) <> dcache.io.out.mem
  memXbar.io.in(3) <> icache.io.out.mem

  io.mem <> memXbar.io.out

  icache.io.out.coh <> DontCare
  dcache.io.out.coh <> DontCare

  // redirect
  frontend.io.redirect <> backend.io.redirect
  backend.io.flush := frontend.io.flushVec(3, 2)

  frontend.io.ipf := DontCare
  backend.io.memMMU <> DontCare

}
