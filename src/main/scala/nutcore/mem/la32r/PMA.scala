package nutcore

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import bus.simplebus._
import bus.axi4._
import chisel3.experimental.IO
import nutcore.HasNutCoreParameter
import utils._
import top.Settings
import sim.DeviceSpace

object PMA extends HasNutCoreParameter {
  // (start, size)
  def addrSpace = List(
    (Settings.getLong("RAMBase"), Settings.getLong("RAMSize")),
  ) ++ DeviceSpace.device

  def isValidAddr(addr: UInt) = addrSpace.map(range => {
    require(isPow2(range._2))
    val bits = log2Up(range._2)
    (addr ^ range._1.U)(PAddrBits - 1, bits) === 0.U
  }).reduce(_ || _)

}
