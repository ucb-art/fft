// See LICENSE for license details.

// Author: Stevo Bailey (stevo.bailey@berkeley.edu)

package fft

import chisel3.util._
import chisel3._
import dsptools.numbers.{DspComplex, Real}
import dsptools.numbers.implicits._
import dsptools.junctions._
import dsptools.counters._
import scala.math._
import FFTFunctions._

// fast fourier transform io
class DirectFFTIO[T<:Data:Real](genIn: => DspComplex[T], genOut: => Option[DspComplex[T]] = None,
  val config: FFTConfig = FFTConfig()) extends Bundle {

  val in = Input(ValidWithSync(Vec(config.p, genIn)))
  val out = Output(ValidWithSync(Vec(config.p, genOut.getOrElse(genIn))))
}

// fast fourier transform - cooley-tukey algorithm, decimation-in-time
// direct form version
// note, this is always a p-point FFT, though the twiddle factors will be different if p < n
class DirectFFT[T<:Data:Real](genIn: => DspComplex[T], genOut: => Option[DspComplex[T]] = None, genTwiddle: => Option[DspComplex[T]] = None,
  val config: FFTConfig = FFTConfig()) extends Module {

  val io = IO(new DirectFFTIO(genIn, genOut, config))

  val sync = CounterWithReset(io.in.valid, config.bp, io.in.sync && io.in.valid)._1

  // indicies to the twiddle factors
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

  // wire up twiddles
  val twiddle = Vec.fill(config.p-1)(Wire(genTwiddle.getOrElse(genIn)))
  val twiddle_rom = Vec(config.twiddle.map(x => DspComplex.wire(implicitly[Real[T]].fromDouble(x(0)), implicitly[Real[T]].fromDouble(x(1)))))
  val indices_rom = Vec(indices.map(x => UInt(x)))
  // TODO: make this not a multiply
  val start = sync*UInt(config.p-1)
  // special case when n = 4, because the pattern breaks down
  // TODO: switch this to multiplyByJ
  if (config.n == 4) {
    twiddle := (0 until config.p-1).map(x => Mux(indices_rom(start+UInt(x))(log2Ceil(config.n/4)), DspComplex.wire(twiddle_rom(0).imaginary, -twiddle_rom(0).real), twiddle_rom(0)))
  } else {
    twiddle := (0 until config.p-1).map(x => Mux(indices_rom(start+UInt(x))(log2Ceil(config.n/4)), 
      DspComplex.wire(twiddle_rom(indices_rom(start+UInt(x))(log2Ceil(config.n/4)-1, 0)).imaginary, -twiddle_rom(indices_rom(start+UInt(x))(log2Ceil(config.n/4)-1, 0)).real), 
      twiddle_rom(indices_rom(start+UInt(x)))))
  }

  // sync
  io.out.sync := ShiftRegister(io.in.sync, config.direct_pipe)
  io.out.valid := ShiftRegister(io.in.valid, config.direct_pipe)

  // p-point decimation-in-time direct form FFT with inputs in normal order (outputs bit reversed)
  // TODO: change type? should it all be genIn?
  val stage_outputs = List.fill(log2Up(config.p)+1)(List.fill(config.p)(Wire(genIn)))
  io.in.bits.zip(stage_outputs(0)).foreach { case(in, out) => out := in }

  // indices to the twiddle Vec input
  var vindices = List(List(0,1),List(0,2))
  for (i <- 0 until log2Up(config.p)-2) {
    vindices = vindices.map(x => x.map(y => y+1))
    val indices_max = vindices.foldLeft(0)((b,a) => max(b,a.foldLeft(0)((d,c) => max(c,d))))
    vindices = vindices ++ vindices.map(x => x.map(y => y+indices_max))
    vindices = vindices.map(x => 0 +: x)
  }

  // create the FFT hardware
  for (i <- 0 until log2Up(config.p)) {
    for (j <- 0 until config.p/2) {

      val skip = pow(2,log2Up(config.p/2)-i).toInt
      val start = ((j % skip) + floor(j/skip) * skip*2).toInt

      // hook it up
      List(stage_outputs(i+1)(start), stage_outputs(i+1)(start+skip)).zip(Butterfly(List(stage_outputs(i)(start), stage_outputs(i)(start+skip)), twiddle(vindices(j)(i)))).foreach { x =>
        x._1 := ShiftRegister(x._2, config.d_pipe_amts(i), io.in.valid)
      }

    }
  }

  // wire up top-level outputs
  io.out.bits := stage_outputs(log2Up(config.p))
}
