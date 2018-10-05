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

abstract class FFTBlock[T <: Data : Real](val config: FFTConfig[T])(implicit p: Parameters) extends TLDspBlock with TLHasCSR {
  val streamNode = AXI4StreamIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    val (in, inP) = streamNode.in.head
    val (out, outP) = streamNode.out.head

    val module = Module(new FFT[T](config))

    val dataSetEndClear = RegInit(0.U(64.W))

    in.ready := true.B
    module.io.in.valid := in.valid
    module.io.in.bits := in.bits.data.asTypeOf(module.io.in.bits)
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
