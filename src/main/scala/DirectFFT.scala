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
class DirectFFTIO[T<:Data:Real](genIn: => DspComplex[T],
                                genOut: => Option[DspComplex[T]] = None,
                                genTwiddle: => Option[DspComplex[T]] = None,
                                val config: FFTConfig
                               ) extends Bundle {
  val data_in = Vec(config.p, genIn.asInput)
  val data_out = Vec(config.p, genOut.getOrElse(genIn).asOutput)
  val twiddle = Vec(config.p/2, genTwiddle.getOrElse(genIn).asInput)
}

// fast fourier transform - cooley-tukey algorithm, decimation-in-time
// direct form version
// note, this is always a p-point FFT, though the twiddle factors will be different if p < n
class DirectFFT[T<:Data:Real](genIn: => DspComplex[T],
                genOut: => Option[DspComplex[T]] = None,
                genTwiddle: => Option[DspComplex[T]] = None,
                val config: FFTConfig = FFTConfig()
               ) extends Module {
  val io = new DirectFFTIO(genIn, genOut, genTwiddle, config)

  // N-point decimation-in-time FFT with inputs in normal order (outputs bit reversed)
  val stage_outputs = Vec(log2Up(config.p), Vec(config.p+1, genIn))
  stage_outputs(0) := io.data_in

  for (i <- 0 until log2Up(config.p)) {
    for (j <- 0 until config.p/2) {

      val old_exp = (pow(2,i)-1+floor(j/pow(2,log2Up(config.p/2)-i))).toInt
      val exp = (floor((old_exp+1)/2.0)).toInt
      val flip = old_exp % 2 == 0 && old_exp != 0
      val skip = pow(2,log2Up(config.p/2)-i).toInt
      val start = ((j % skip) + floor(j/skip) * skip*2).toInt

      // hook it up
      val twiddle = {if (flip) DspComplex.wire(io.twiddle(exp).imaginary, -io.twiddle(exp).real) else io.twiddle(exp)}
      List(stage_outputs(i+1)(start), stage_outputs(i+1)(start+skip)).zip(Butterfly(List(stage_outputs(i)(start), stage_outputs(i)(start+skip)), twiddle)).foreach { x =>
        x._1 := ShiftRegister(x._2, config.d_pipe_amts(i))
      }

    }
  }

  // wire up top-level outputs
  io.data_out := stage_outputs(log2Up(config.p))
}
