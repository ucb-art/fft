// See LICENSE for license details.

// Author: Stevo Bailey (stevo.bailey@berkeley.edu)

package fft

import chisel3.util.{Counter, ShiftRegister, log2Up}
import chisel3.{Bool, Bundle, Data, Module, Reg, UInt, Vec, Wire, when}
import dsptools.numbers.{DspComplex, Real}
import dsptools.numbers.implicits._
import scala.math._

// fast fourier transform io
// twiddle factors are direct inputs to allow for split fft
class DirectFFTIO[T<:Data:Real](genIn: => DspComplex[T], genOut: => Option[DspComplex[T]] = None, genTwiddle: => Option[DspComplex[T]] = None,
  val config: FFTConfig) extends Bundle {

  val data_in = Vec(config.p, genIn).flip
  val data_out = Vec(config.p, genOut.getOrElse(genIn))
  val twiddle = Vec(config.p-1, genTwiddle.getOrElse(genIn)).flip
}

// fast fourier transform - cooley-tukey algorithm, decimation-in-time
// direct form version
// note, this is always a p-point FFT, though the twiddle factors will be different if p < n
class DirectFFT[T<:Data:Real](genIn: => DspComplex[T], genOut: => Option[DspComplex[T]] = None, genTwiddle: => Option[DspComplex[T]] = None,
  val config: FFTConfig = FFTConfig()) extends Module {

  val io = new DirectFFTIO(genIn, genOut, genTwiddle, config)

  // p-point decimation-in-time direct form FFT with inputs in normal order (outputs bit reversed)
  // TODO: change type? should it all be genIn?
  val stage_outputs = List.fill(log2Up(config.p)+1)(List.fill(config.p)(Wire(genIn)))
  io.data_in.zip(stage_outputs(0)).foreach { case(in, out) => out := in }

  // indices to the twiddle Vec input
  var indices = List(List(0,1),List(0,2))
  for (i <- 0 until log2Up(config.p)-2) {
    indices = indices.map(x => x.map(y => y+1))
    val indices_max = indices.foldLeft(0)((b,a) => max(b,a.foldLeft(0)((d,c) => max(c,d))))
    indices = indices ++ indices.map(x => x.map(y => y+indices_max))
    indices = indices.map(x => 0 +: x)
  }

  // create the FFT hardware
  for (i <- 0 until log2Up(config.p)) {
    for (j <- 0 until config.p/2) {

      val skip = pow(2,log2Up(config.p/2)-i).toInt
      val start = ((j % skip) + floor(j/skip) * skip*2).toInt

      // hook it up
      List(stage_outputs(i+1)(start), stage_outputs(i+1)(start+skip)).zip(Butterfly(List(stage_outputs(i)(start), stage_outputs(i)(start+skip)), io.twiddle(indices(j)(i)))).foreach { x =>
        x._1 := ShiftRegister(x._2, config.d_pipe_amts(i))
      }

    }
  }

  // wire up top-level outputs
  io.data_out := stage_outputs(log2Up(config.p))
}
