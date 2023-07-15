package sim

import system._
import nutcore.NutCoreConfig
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import bus.axi4._
import device.{AXI4Confreg, AXI4RAM}
import nutcore._
import _root_.utils.GTimer
import bus.simplebus._
import difftest._
import nutcore.AddressSpace.PAddrBits
import top.Settings

object DeviceSpace extends HasNutCoreParameter {
  // (start, size)
  def device = List(
    (Settings.getLong("ConfregBase1"), Settings.getLong("ConfregSize")), // confreg
  )

  def isDevice(addr: UInt) = device.map(range => {
    require(isPow2(range._2))
    val bits = log2Up(range._2)
    (addr ^ range._1.U)(PAddrBits-1, bits) === 0.U
  }).reduce(_ || _)
}

class SimTop extends Module  {
  val io = IO(new Bundle{
    val logCtrl = new LogCtrlIO
    val perfInfo = new PerfInfoIO
    val uart = new UARTIO
    val timer = Output(UInt(64.W))
  })

  lazy val config = NutCoreConfig(FPGAPlatform = false)
  val core = Module(new La32rNutCore()(config))
  val mem = Module(new AXI4RAM(memByte = (Settings.getLong("RAMSize") - Settings.getLong("RAMBase")).toInt, useBlackBox = true))
  // Be careful with the commit checking of emu.
  // A large delay will make emu incorrectly report getting stuck.
  val memdelay = Module(new AXI4Delayer(0))

  val confreg = Module(new AXI4Confreg)

  val addrSpace = DeviceSpace.device ++ List( // need set device before ram for memXbar select priority
    (Settings.getLong("RAMBase"), Settings.getLong("RAMSize")),
  )

  val memXbar = Module(new SimpleBusCrossbar1toN(addrSpace))
  memXbar.io.in <> core.io.mem

  memdelay.io.in <> memXbar.io.out(1).toAXI4()
  mem.io.in <> memdelay.io.out

  confreg.io.in <> memXbar.io.out(0).toAXI4Lite()

  val log_begin, log_end, log_level = WireInit(0.U(64.W))
  log_begin := io.logCtrl.log_begin
  log_end := io.logCtrl.log_end
  log_level := io.logCtrl.log_level

  assert(log_begin <= log_end)
  BoringUtils.addSource((GTimer() >= log_begin) && (GTimer() < log_end), "DISPLAY_ENABLE")

  // make BoringUtils not report boring exception when EnableDebug is set to false
  val dummyWire = WireInit(false.B)
  BoringUtils.addSink(dummyWire, "DISPLAY_ENABLE")

  BoringUtils.addSource(io.logCtrl.log_level, "DISPLAY_LOG_LEVEL")

  io.uart <> confreg.io.extra.get

  io.timer := GTimer()

  // TODO : support outer interrupt sim
  val ipi = WireInit(false.B) // inter-core interrupt for la32r
  val hwi = WireInit(0.U(8.W)) // hardware interrupt for la32r
  BoringUtils.addSource(ipi, "ipi")
  BoringUtils.addSource(hwi, "hwi")
}
