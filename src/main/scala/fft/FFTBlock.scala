// See LICENSE for license details.

package fft

import chisel3._
import dsptools._
import dsptools.numbers._
import dspjunctions._
import dspblocks._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._

class FFTBlock[T <: Data : Real](val config: FFTConfig[T])(implicit p: Parameters) extends TLDspBlock with TLHasCSR {
  val streamNode = AXI4StreamIdentityNode()
  def csrAddress = AddressSet(0x2000, 0x0fff)
  def beatBytes = 8
  def devname = "tlfft"
  def devcompat = Seq("ucb-art", "fft")
  val device = new SimpleDevice(devname, devcompat) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping)
    }
  }
  override val mem = Some(TLRegisterNode(address = Seq(csrAddress), device = device, beatBytes = beatBytes))

  lazy val module = new LazyModuleImp(this) {
    val (in, inP) = streamNode.in.head
    val (out, outP) = streamNode.out.head

    val module = Module(new FFT[T](config))

    val dataSetEndClear = RegInit(0.U(64.W))
    module.io.data_set_end_clear := dataSetEndClear

    in.ready := true.B
    module.io.in.valid := in.valid
    if (config.quadrature) {
      module.io.in.bits := in.bits.data.asTypeOf(module.io.in.bits)
    } else {
      val inAsReal = in.bits.data.asTypeOf(Vec(config.lanes, config.genIn.real))
      module.io.in.bits.zip(inAsReal).foreach { case (mod, in) =>
        mod.real := in
        mod.imag := Real[T].zero
      }
    }
    module.io.in.sync := in.bits.last

    assert(out.ready)
    out.valid := module.io.out.valid
    out.bits.data := module.io.out.bits.asUInt
    out.bits.last := module.io.out.sync

    regmap(
      0x0 -> Seq(RegField.r(8, module.io.data_set_end_status)),
      0x8 -> Seq(RegField(8, dataSetEndClear)),
    )
  }
}
