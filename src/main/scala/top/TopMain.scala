package top

import eulacore.{EulaCore, EulaCoreConfig}
import sim.SimTop
import chisel3._
import chisel3.stage._
import bus.simplebus._
import bus.axi4._
import chisel3.util.experimental.{BoringUtils, forceName}


class Top extends RawModule {
  override val desiredName = "mycpu_top"
  val io = IO(new Bundle() {
    val ext_int = Input(UInt(8.W))
    val clock = Input(Clock())
    val reset = Input(Bool())

    val arid = Output(UInt(4.W))
    val araddr = Output(UInt(32.W))
    val arlen = Output(UInt(4.W)) // TODO : 4 OR 8 ?
    val arsize = Output(UInt(3.W))
    val arburst = Output(UInt(2.W))
    val arlock = Output(UInt(2.W))
    val arcache = Output(UInt(4.W))
    val arprot = Output(UInt(3.W))
    val arvalid = Output(Bool())
    val arready = Input(Bool())

    val rid = Input(UInt(4.W))
    val rdata = Input(UInt(32.W))
    val rresp = Input(UInt(2.W))
    val rlast = Input(Bool())
    val rvalid = Input(Bool())
    val rready = Output(Bool())

    val awid = Output(UInt(4.W))
    val awaddr = Output(UInt(32.W))
    val awlen = Output(UInt(4.W)) // TODO : 4 OR 8 ?
    val awsize = Output(UInt(3.W))
    val awburst = Output(UInt(2.W))
    val awlock = Output(UInt(2.W))
    val awcache = Output(UInt(4.W))
    val awprot = Output(UInt(3.W))
    val awvalid = Output(Bool())
    val awready = Input(Bool())

    val wid = Output(UInt(4.W))
    val wdata = Output(UInt(32.W))
    val wstrb = Output(UInt(4.W))
    val wlast = Output(Bool())
    val wvalid = Output(Bool())
    val wready = Input(Bool())

    val bid = Input(UInt(4.W))
    val bresp = Input(UInt(2.W))
    val bvalid = Input(Bool())
    val bready = Output(Bool())

    val debug_wb_pc = Output(UInt(32.W))
    val debug_wb_rf_wen = Output(UInt(4.W))
    val debug_wb_rf_wnum = Output(UInt(5.W))
    val debug_wb_rf_wdata = Output(UInt(32.W))
  })

  withClockAndReset(io.clock, !io.reset) {
    val memXbar = Module(new SimpleBusCrossbarNto1(2))
    val core = Module(new EulaCore()(EulaCoreConfig()))

    core.io.ipi := false.B
    core.io.hwi := io.ext_int

    memXbar.io.in(0) <> core.io.uncachedMem
    memXbar.io.in(1) <> core.io.cachedMem
    val axi4mem = SimpleBus2AXI4Converter(in = memXbar.io.out, outType = new AXI4, isFromCache = true)

        io.arid := axi4mem.ar.bits.id
        io.araddr := axi4mem.ar.bits.addr
        io.arlen := axi4mem.ar.bits.len
        io.arsize := axi4mem.ar.bits.size
        io.arburst := axi4mem.ar.bits.burst
        io.arlock := axi4mem.ar.bits.lock
        io.arcache := axi4mem.ar.bits.cache
        io.arprot := axi4mem.ar.bits.prot
        io.arvalid := axi4mem.ar.valid
        axi4mem.ar.ready := io.arready

        axi4mem.r.bits.id := io.rid
        axi4mem.r.bits.data := io.rdata
        axi4mem.r.bits.resp := io.rresp
        axi4mem.r.bits.last := io.rlast
        axi4mem.r.valid := io.rvalid
        io.rready := axi4mem.r.ready

        io.awid := axi4mem.aw.bits.id
        io.awaddr := axi4mem.aw.bits.addr
        io.awlen := axi4mem.aw.bits.len
        io.awsize := axi4mem.aw.bits.size
        io.awburst := axi4mem.aw.bits.burst
        io.awlock := axi4mem.aw.bits.lock
        io.awcache := axi4mem.aw.bits.cache
        io.awprot := axi4mem.aw.bits.prot
        io.awvalid := axi4mem.aw.valid
        axi4mem.aw.ready := io.awready

        io.wid := 0.U
        io.wdata := axi4mem.w.bits.data
        io.wstrb := axi4mem.w.bits.strb
        io.wlast := axi4mem.w.bits.last
        io.wvalid := axi4mem.w.valid
        axi4mem.w.ready := io.wready

        axi4mem.b.bits.id := io.bid
        axi4mem.b.bits.resp := io.bresp
        axi4mem.b.valid := io.bvalid
        io.bready := axi4mem.b.ready

    axi4mem.r.bits.user := 0.U
    axi4mem.b.bits.user := 0.U

    val w_debug_wb_rf_wen = WireInit(0.U(4.W))
    val w_debug_wb_pc = WireInit(0.U(32.W))
    val w_debug_wb_rf_wdata = WireInit(0.U(32.W))
    val w_debug_wb_rf_wnum = WireInit(0.U(5.W))
    BoringUtils.addSink(w_debug_wb_rf_wen, "DEBUG_WB_RF_WEN")
    BoringUtils.addSink(w_debug_wb_pc, "DEBUG_WB_PC")
    BoringUtils.addSink(w_debug_wb_rf_wdata, "DEBUG_WB_RF_WDATA")
    BoringUtils.addSink(w_debug_wb_rf_wnum, "DEBUG_WB_RF_WNUM")

    io.debug_wb_rf_wen := w_debug_wb_rf_wen
    io.debug_wb_pc := w_debug_wb_pc
    io.debug_wb_rf_wdata := w_debug_wb_rf_wdata
    io.debug_wb_rf_wnum := w_debug_wb_rf_wnum

  }
  forceName(io.ext_int, "ext_int")
  forceName(io.clock, "aclk")
  forceName(io.reset, "aresetn")

  forceName(io.arid, "arid")
  forceName(io.araddr, "araddr")
  forceName(io.arlen, "arlen")
  forceName(io.arsize, "arsize")
  forceName(io.arburst, "arburst")
  forceName(io.arlock, "arlock")
  forceName(io.arcache, "arcache")
  forceName(io.arprot, "arprot")
  forceName(io.arvalid, "arvalid")
  forceName(io.arready, "arready")

  forceName(io.rid, "rid")
  forceName(io.rdata, "rdata")
  forceName(io.rresp, "rresp")
  forceName(io.rlast, "rlast")
  forceName(io.rvalid, "rvalid")
  forceName(io.rready, "rready")

  forceName(io.awid, "awid")
  forceName(io.awaddr, "awaddr")
  forceName(io.awlen, "awlen")
  forceName(io.awsize, "awsize")
  forceName(io.awburst, "awburst")
  forceName(io.awlock, "awlock")
  forceName(io.awcache, "awcache")
  forceName(io.awprot, "awprot")
  forceName(io.awvalid, "awvalid")
  forceName(io.awready, "awready")

  forceName(io.wid, "wid")
  forceName(io.wdata, "wdata")
  forceName(io.wstrb, "wstrb")
  forceName(io.wlast, "wlast")
  forceName(io.wvalid, "wvalid")
  forceName(io.wready, "wready")

  forceName(io.bid, "bid")
  forceName(io.bresp, "bresp")
  forceName(io.bvalid, "bvalid")
  forceName(io.bready, "bready")

  forceName(io.debug_wb_pc, "debug_wb_pc")
  forceName(io.debug_wb_rf_wen, "debug_wb_rf_wen")
  forceName(io.debug_wb_rf_wnum, "debug_wb_rf_wnum")
  forceName(io.debug_wb_rf_wdata, "debug_wb_rf_wdata")

}

object TopMain extends App {
  def parseArgs(info: String, args: Array[String]): String = {
    var target = ""
    for (arg <- args) { if (arg.startsWith(info + "=") == true) { target = arg } }
    require(target != "")
    target.substring(info.length()+1)
  }
  val board = parseArgs("BOARD", args)
  val core = parseArgs("CORE", args)
  
  val s = (board match {
    case "sim"    => Nil
    case "loongsonfpga" => Nil
  } ) ++ ( core match {
    case "la32r" => La32rSettings()
  } )
  s.foreach{Settings.settings += _} // add and overwrite DefaultSettings
  println("====== Settings = (" + board + ", " +  core + ") ======")
  Settings.settings.toList.sortBy(_._1)(Ordering.String).foreach {
    case (f, v: Long) =>
      println(f + " = 0x" + v.toHexString)
    case (f, v) =>
      println(f + " = " + v)
  }
  if (board == "sim") {
    (new ChiselStage).execute(args, Seq(
      ChiselGeneratorAnnotation(() => new SimTop))
    )
  } else {
    (new ChiselStage).execute(args, Seq(
      ChiselGeneratorAnnotation(() => new Top))
    )
  }
}
