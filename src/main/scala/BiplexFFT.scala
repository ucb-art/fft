// See LICENSE for license details.

// Author: Stevo Bailey (stevo.bailey@berkeley.edu)

package fft

import chisel3.util.{Counter, ShiftRegister, log2Up}
import chisel3.{Bool, Bundle, Data, Module, Reg, UInt, Vec, Wire, when}
import dsptools.numbers.{DspComplex, Real}
import dsptools.numbers.implicits._
import scala.math._

// fast fourier transform io
class BiplexFFTIO[T<:Data:Real](genIn: => DspComplex[T], genOut: => Option[DspComplex[T]] = None, genTwiddle: => Option[DspComplex[T]] = None,
  val config: FFTConfig) extends Bundle {

  val data_in = Vec(2, genIn).flip
  val data_out = Vec(2, genOut.getOrElse(genIn))
  val twiddle = Vec(log2Up(config.bp), genTwiddle.getOrElse(genIn)).flip
  val mux_sel = Vec(log2Up(config.bp), UInt(width=1)).flip
}

// fast fourier transform - cooley-tukey algorithm, decimation-in-time
// biplex pipelined version
// note, this is always a bp-point FFT
class BiplexFFT[T<:Data:Real](genIn: => DspComplex[T], genOut: => Option[DspComplex[T]] = None, genTwiddle: => Option[DspComplex[T]] = None,
  val config: FFTConfig = FFTConfig()) extends Module {

  val io = new BiplexFFTIO(genIn, genOut, genTwiddle, config)

  // bp-point decimation-in-time biplex pipelined FFT with outputs in bit-reversed order
  // TODO: change type? should it all be genIn?
  val stage_outputs = List.fill(log2Up(config.bp)+1)(List.fill(2)(Wire(genIn)))
  io.data_in.zip(stage_outputs(0)).foreach { case(in, out) => out := in }

  // create the FFT hardware
  for (i <- 0 until log2Up(config.bp)) {

    // hook it up
    val stage_delay = (config.bp/pow(2,i+1)).toInt
    val mux_out = BarrelShifter(Vec(stage_outputs(i)(0), ShiftRegisterMem(stage_outputs(i)(1), stage_delay)), io.mux_sel(i))
    stage_outputs(i+1).zip(Butterfly(List(ShiftRegisterMem(mux_out(0), stage_delay), mux_out(1)), io.twiddle(i))).foreach { x => 
      x._1 := ShiftRegister(x._2, config.b_pipe_amts(i))
    }

  }

}
