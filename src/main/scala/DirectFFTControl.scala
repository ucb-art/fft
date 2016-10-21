// See LICENSE for license details.

// Author: Stevo Bailey (stevo.bailey@berkeley.edu)

package fft

import chisel3.util.{Counter, ShiftRegister, log2Up}
import chisel3.{Bool, Bundle, Data, Module, Reg, UInt, Vec, Wire, when, Mux}
import dsptools.numbers.{DspComplex, Real}
import dsptools.numbers.implicits._
import scala.math._

class DirectFFTControlIO[T<:Data:Real](genTwiddle: => DspComplex[T],
                                       val config: FFTConfig
                                      ) extends Bundle {
  val sync_in = UInt(log2Up(config.bp)) // input
  val twiddle = Vec(config.p, genTwiddle)
  val sync_out = UInt(log2Up(config.bp)) //output
}

// direct FFT controller
// generates twiddle factors for direct FFTs
class DirectFFTControl[T<:Data:Real](genTwiddle: => DspComplex[T],
                                     val config: FFTConfig = FFTConfig()
                                    ) extends Module {
  val io = new DirectFFTControlIO(genTwiddle, config)

  //var pipe_sum = 0
  //var pipe_delays = Array.fill[Integer](log2Up(config.p))(0)
  //for (i <- 0 until log2Up(config.p)) {
  //  pipe_delays(i) = pipe_sum-1 // pipeline the twiddle factors generation before they're fed into butterflies
  //  pipe_sum = pipe_sum + config.d_pipe_amts(i)
  //}

  // bit reverse a value
  def bit_reverse(in: Integer, width: Integer): Integer = {
    var test = in
    var out = 0
    for (i <- 0 until width) {
      if (test / pow(2, width-i-1) >= 1) {
        out += pow(2,i).toInt
        test -= pow(2,width-i-1).toInt
      }
    }
    out
  }

  // twiddle factors for in-place ffts
  val twiddle = (0 until config.n/2).map(x => Array(cos(2*Pi/config.n*x),-sin(2*Pi/config.n*x)))
  var exp = new Array[Integer](config.n/2+1)
  var idx = 0
  var new_idx = 0
  //val idx_count = (0 until n/p).map(x => if (x*2 < n/p) x*2 else (x*2+1)%(n/p))
  val idx_count = (0 until config.bp).map(x => if (x*2 < config.bp) x*2 else (x*2+ (if (config.real) 3 else 1) )%(config.bp))
  for (h <- 0 until config.bp) {
    for (i <- 0 until log2Up(config.p)) {
      for (j <- 0 until pow(2,i).toInt) {
        val stage = i+(log2Up(config.n)-log2Up(config.p))
        val delay = config.d_pipe_delays(i)
        val position = idx_count.indexOf(h)
        val new_position = ((position-delay)%(config.bp)+config.bp)%(config.bp)
        val row = j+idx_count(new_position)*pow(2,i)
        exp(new_idx) = bit_reverse((floor(row*config.n/2/pow(2,stage)/pow(2,log2Up(config.n/2)-stage))).toInt, log2Up(config.n/2))
        if ((idx % (config.p-1)) == 0 || (idx % (config.p-1)) % 2 == 1) {
          new_idx = new_idx + 1
        }
        idx = idx + 1
      }
    }
  }
  //val twiddle_rom = Vec((0 until config.bp).map(y => Vec((0 until config.p/2).map(x => DspComplex.wire(Real[T].fromDouble(twiddle(exp(x+y*(config.p/2)))(0)), Real[T].fromDouble(twiddle(exp(x+y*(config.p/2)))(1)))))))
  val twiddle_rom = Vec((0 until config.bp).map(y => Vec((0 until config.p/2).map(x => DspComplex.wire(Real[T].fromDouble(twiddle(exp(x+y*(config.p/2)))(0)), Real[T].fromDouble(twiddle(exp(x+y*(config.p/2)))(1)))))))
  val tw_count = UInt(width=log2Up(config.bp))
  val tw_idx = UInt(width=log2Up(config.bp))
  val rom_pipeline_depth = 1
  if (config.real) {
    tw_count := io.sync_in - UInt(config.bp/4-1-rom_pipeline_depth) 
    //tw_idx := Mux(tw_count(log2Up(n/p)-1), tw_count << UInt(1), UInt(1))
    tw_idx := Mux(tw_count(log2Up(config.bp)-1), tw_count << UInt(1), (tw_count << UInt(1)) + UInt(3))
  }
  else {
    tw_count := io.sync_in + UInt(1-rom_pipeline_depth)
    tw_idx := Mux(tw_count(log2Up(config.bp)-1), tw_count << UInt(1), (tw_count << UInt(1)) + UInt(1))
  }

  // output
  for (i <- 0 until config.p/2) {
    val new_i = max(0, i*2-1)
    io.twiddle(i) := ShiftRegister(twiddle_rom(tw_idx)(i), rom_pipeline_depth+1)
  }
  
  if (config.direct_pipe > 0 && config.real) {
    io.sync_out := ShiftRegister(io.sync_in, config.direct_pipe) + UInt(config.bp/4)
  }
  else if (config.direct_pipe > 0) {
    io.sync_out := ShiftRegister(io.sync_in, config.direct_pipe) + UInt(config.bp/2)
  }
  else {
    io.sync_out := io.sync_in
  }
}
