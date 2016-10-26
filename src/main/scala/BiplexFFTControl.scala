// See LICENSE for license details.

// Author: Stevo Bailey (stevo.bailey@berkeley.edu)

package fft

import chisel3.util.{Counter, ShiftRegister, log2Up}
import chisel3.{Bool, Bundle, Data, Module, Reg, UInt, Vec, Wire, when}
import dsptools.numbers.{DspComplex, Real}
import dsptools.numbers.implicits._
import scala.math._
import FFTFunctions._

class BiplexFFTControlIO[T<:Data:Real](genTwiddle: => DspComplex[T], val config: FFTConfig) extends Bundle {
  val sync_in = UInt(log2Up(config.bp)).flip
  val twiddle = Vec(log2Up(config.bp), genTwiddle)
  val mux_sel = Vec(log2Up(config.bp), UInt(width=1))
  val sync_out = UInt(log2Up(config.bp))
}

// biplex FFT controller
// generates twiddle factors for biplex FFTs
// also passes synchronization signal
class BiplexFFTControl[T<:Data:Real](genTwiddle: => DspComplex[T], val config: FFTConfig = FFTConfig()) extends Module {
  val io = new DirectFFTControlIO(genTwiddle, config)

  // twiddle factors
  val twiddle = (0 until config.n/4).map(x => Array(cos(2*Pi/config.n*x),-sin(2*Pi/config.n*x)))
  val sync = List.fill(log2Up(config.bp)+1)(UInt())
  sync(0) := io.sync_in

  val twiddle_rom = Vec( (0 until config.n/4).map(x => new ComplexSInt(SInt(twiddle(x)(0), ow), SInt(twiddle(x)(1), ow))) )

  for (i <- 0 until log2Up(config.bp)) {

    // sync
    val stage_delay = (config.bp/pow(2,i+1)).toInt
    sync(i) := ShiftRegister(sync(i-1), config.b_pipe_delays(i))

    io.mux_sel(i) := 
    // other stages
    else {
      val pipe_amt = pipelines_per_stage + {if (stages_to_pipeline contains i) 1 else 0}
      pipe_sum = pipe_sum + pipe_amt
      val add_amt = (((pow(2, log2Up(n)-i-1)-pipe_sum+1)%n+n)%n).toInt
      //val add_amt = (((pow(2, log2Up(n)-i-1)-pipe_sum)%n+n)%n).toInt
      if (i == log2Up(n)-1) {
        idx(i) := Reverse( (io.sync_in + UInt(add_amt))(log2Up(n)-2,log2Up(n)-1-i) )
      }
      else {
        idx(i) := Cat(Reverse( (io.sync_in + UInt(add_amt))(log2Up(n)-2,log2Up(n)-1-i) ), UInt(0, log2Up(n)-i-1))
      }
    }
    io.twiddle(i) := Reg(next=twiddle_rom(idx(i))(ow-1, ow-stage_widths(i)))
    //io.twiddle(i) := twiddle_rom(idx(i))(ow-1, ow-stage_widths(i))
  }

  if (pipe > 0) {
    io.sync_out := ShiftRegister(io.sync_in, pipe)
  }
  else {
    io.sync_out := io.sync_in
  }
}
