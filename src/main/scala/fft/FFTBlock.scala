// See LICENSE for license details.

package fft

import cde.Parameters
import chisel3._
import dsptools._
import dsptools.numbers._
import dspjunctions._

class LazyFFTBlock[T <: Data : Real]()(implicit p: Parameters) extends LazyDspBlock()(p) {
  def controls = Seq()
  def statuses = Seq()

  lazy val module = Module(new FFTBlock[T](this))

}

class FFTBlock[T <: Data : Real](outer: LazyDspBlock)(implicit p: Parameters)
  extends GenDspBlock[T, T](outer)(p) {
  val baseAddr = BigInt(0)

  val module = Module(new FFT[T])
  
  module.io.in <> unpackInput(lanesIn, genIn())
  unpackOutput(lanesOut, genOut()) <> module.io.out

}
