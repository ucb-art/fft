// See LICENSE for license details.

// Author: Stevo Bailey (stevo.bailey@berkeley.edu)

package fft

import chisel3.util._
import chisel3.{Bool, Bundle, Data, Module, Reg, UInt, Vec, Wire, when, Mux}
import dsptools.numbers.{DspComplex, Real}
import dsptools.numbers.implicits._
import scala.math._
import FFTFunctions._

class DirectFFTControlIO[T<:Data:Real](genTwiddle: => DspComplex[T], val config: FFTConfig) extends Bundle {
  val sync_in = UInt(log2Up(config.bp)).flip
  val twiddle = Vec(config.p-1, genTwiddle)
  val sync_out = UInt(log2Up(config.bp))
}

// direct FFT controller
// generates twiddle factors for direct FFTs
// also passes synchronization signal
class DirectFFTControl[T<:Data:Real](genTwiddle: => DspComplex[T], val config: FFTConfig = FFTConfig()) extends Module {
  val io = new DirectFFTControlIO(genTwiddle, config)

  // twiddle factors for in-place fft
  val twiddle = (0 until config.n/4).map(x => Array(cos(2*Pi/config.n*x),-sin(2*Pi/config.n*x)))

  // indicies to the twiddle factors calculated above
  var indices = Array.fill(log2Up(config.n))(0)
  var prev = Array.fill(log2Up(config.n))(0)
  for (i <- 1 until config.n/2) {
    val next = (0 until log2Up(config.n)).map(x => floor(i/pow(2,x)).toInt).reverse
    prev.zip(next).foreach{case(p,n) => {if (n != p) indices = indices :+ n}}
    prev = next.toArray
  }
  indices = indices.map(x => bit_reverse(x, log2Up(config.n)-1))

  // take subset of indices if this is part of a larger FFT
  var p = config.n
  var temp = Array(indices)
  while (p > config.p) {
    temp = temp.map(x => x.drop(1).splitAt((x.size-1)/2)).flatMap(x => Array(x._1, x._2))
    p = p/2
  }
  indices = temp.flatten
  println(indices.deep.mkString("\n"))

  // wire up twiddles
  val twiddle_rom = Vec(twiddle.map(x => DspComplex.wire(implicitly[Real[T]].fromDouble(x(0)), implicitly[Real[T]].fromDouble(x(1)))))
  val indices_rom = Vec(indices.map(x => UInt(x)))
  // TODO: make this not a multiply
  val start = io.sync_in*UInt(config.p-1)
  // special case when n = 4, because the pattern breaks down
  if (config.n == 4) {
    io.twiddle := (0 until config.p-1).map(x => Mux(indices_rom(start+UInt(x))(log2Ceil(config.n/4)), DspComplex.wire(twiddle_rom(0).imaginary, -twiddle_rom(0).real), twiddle_rom(0)))
  } else {
    io.twiddle := (0 until config.p-1).map(x => Mux(indices_rom(start+UInt(x))(log2Ceil(config.n/4)), 
      DspComplex.wire(twiddle_rom(indices_rom(start+UInt(x))(log2Ceil(config.n/4-1), 0)).imaginary, -twiddle_rom(indices_rom(start+UInt(x))(log2Ceil(config.n/4-1), 0)).real), 
      twiddle_rom(indices_rom(start+UInt(x)))))
  }

  // sync
  io.sync_out := ShiftRegister(io.sync_in, config.direct_pipe)
}
