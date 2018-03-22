// See LICENSE for license details.

package fft

import cde.Parameters
import chisel3._
import dsptools._
import dsptools.numbers._
import dspjunctions._
import dspblocks._

class FFTBlock[T <: Data : Real]()(implicit p: Parameters) extends DspBlock()(p) {
  def controls = Seq()
  def statuses = Seq()

  lazy val module = new FFTBlockModule[T](this)

  addStatus("Data_Set_End_Status")
  addControl("Data_Set_End_Clear", 0.U)
}

class FFTBlockModule[T <: Data : Real](outer: DspBlock)(implicit p: Parameters)
  extends GenDspBlockModule[T, T](outer)(p) {
  val module = Module(new FFT[T])
  val config = p(FFTKey(p(DspBlockId)))
  
  if (config.real) {
    val temp = unpackInput(lanesIn, genIn().asInstanceOf[DspComplex[T]].real.cloneType)
    module.io.in.sync := temp.sync
    module.io.in.valid := temp.valid
    val bitsin = Wire(Vec(lanesIn, genIn()))
    for (i <- 0 until bitsin.size) {
      val c = bitsin(i)
      val r = temp.bits(i)
      c.asInstanceOf[DspComplex[T]].real := r
      c.asInstanceOf[DspComplex[T]].imag := Real[T].zero
    }
    //bitsin.zip(temp.bits).foreach { case (c, r) => c := DspComplex(r, Real[T].zero) }
    module.io.in.bits := bitsin
  } else {
    module.io.in <> unpackInput(lanesIn, genIn())
  }
  unpackOutput(lanesOut, genOut()) <> module.io.out

  status("Data_Set_End_Status") := module.io.data_set_end_status
  module.io.data_set_end_clear := control("Data_Set_End_Clear")

  IPXactComponents._ipxactComponents += DspIPXact.makeDspBlockComponent(baseAddr, uuid, this.name)
}
