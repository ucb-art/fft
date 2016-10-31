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

// fast fourier transform io
class FFTIO[T<:Data:Real](genIn: => DspComplex[T], genOut: => Option[DspComplex[T]] = None,
  val config: FFTConfig = FFTConfig()) extends Bundle {

  val in = Input(ValidWithSync(Vec(config.p, genIn)))
  val out = Output(ValidWithSync(Vec(config.p, genOut.getOrElse(genIn))))
}

// fast fourier transform - cooley-tukey algorithm, decimation-in-time
// direct form version
// note, this is always a p-point FFT, though the twiddle factors will be different if p < n
class DirectFFT[T<:Data:Real](genIn: => DspComplex[T], genOut: => Option[DspComplex[T]] = None, genTwiddle: => Option[DspComplex[T]] = None,
  val config: FFTConfig = FFTConfig()) extends Module {

  val io = IO(new FFTIO(genIn, genOut, config))

  // synchronize
  val sync = CounterWithReset(io.in.valid, config.bp, io.in.sync && io.in.valid)._1
  io.out.sync := ShiftRegister(io.in.sync, config.direct_pipe)
  io.out.valid := ShiftRegister(io.in.valid, config.direct_pipe)

  // wire up twiddles
  val twiddle = Vec.fill(config.p-1)(Wire(genTwiddle.getOrElse(genIn)))
  val twiddle_rom = Vec(config.twiddle.map(x => DspComplex.wire(implicitly[Real[T]].fromDouble(x(0)), implicitly[Real[T]].fromDouble(x(1)))))
  val indices_rom = Vec(config.dindices.map(x => UInt(x)))
  // TODO: make this not a multiply
  val start = sync*UInt(config.p-1)
  // special case when n = 4, because the pattern breaks down
  if (config.n == 4) {
    twiddle := (0 until config.p-1).map(x => Mux(indices_rom(start+UInt(x))(log2Ceil(config.n/4)), DspComplex.divideByJ(twiddle_rom(0)), twiddle_rom(0)))
  } else {
    twiddle := (0 until config.p-1).map(x => Mux(indices_rom(start+UInt(x))(log2Ceil(config.n/4)), DspComplex.divideByJ(twiddle_rom(indices_rom(start+UInt(x))(log2Ceil(config.n/4)-1, 0))), twiddle_rom(indices_rom(start+UInt(x)))))
  }

  // p-point decimation-in-time direct form FFT with inputs in normal order (outputs bit reversed)
  // TODO: change type? should it all be genIn?
  val stage_outputs = List.fill(log2Up(config.p)+1)(List.fill(config.p)(Wire(genIn)))
  io.in.bits.zip(stage_outputs(0)).foreach { case(in, out) => out := in }

  // indices to the twiddle Vec
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
      List(stage_outputs(i+1)(start), stage_outputs(i+1)(start+skip)).zip(Butterfly(List(stage_outputs(i)(start), stage_outputs(i)(start+skip)), twiddle(indices(j)(i)))).foreach { x =>
        x._1 := ShiftRegister(x._2, config.pipe(i+log2Ceil(config.bp)), io.in.valid)
      }

    }
  }

  // wire up top-level outputs
  io.out.bits := stage_outputs(log2Up(config.p))
}

//// fast fourier transform - cooley-tukey algorithm, decimation-in-time
//// biplex pipelined version
//// note, this is always a bp-point FFT
//class BiplexFFT[T<:Data:Real](genIn: => DspComplex[T], genOut: => Option[DspComplex[T]] = None, genTwiddle: => Option[DspComplex[T]] = None,
//  val config: FFTConfig = FFTConfig()) extends Module {
//
//  val io = IO(new FFTIO(genIn, genOut, config))
//
//  // synchronize
//  val sync = List.fill(log2Up(config.bp)+1)(Wire(UInt()))
//  sync(0) := CounterWithReset(io.in.valid, config.bp, io.in.sync && io.in.valid)._1
//  io.out.sync := sync(log2Up(config.bp)) === UInt(config.bp-1)
//  io.out.valid := ShiftRegister(io.in.valid, config.bp+config.biplex_pipe-1)
//
//  // wire up twiddles
//  val twiddle_rom = Vec(config.twiddle.map(x => DspComplex.wire(implicitly[Real[T]].fromDouble(x(0)), implicitly[Real[T]].fromDouble(x(1)))))
//  val indices_rom = Vec(config.bindices.map(x => UInt(x)))
//
//  // bp-point decimation-in-time biplex pipelined FFT with outputs in bit-reversed order
//  // TODO: change type? should it all be genIn?
//  val stage_outputs = List.fill(log2Up(config.bp)+1)(List.fill(config.p)(Wire(genIn)))
//  io.in.bits.zip(stage_outputs(0)).foreach { case(in, out) => out := in }
//
//  // create the FFT hardware
//  for (i <- 0 until log2Up(config.bp)) {
//    for (j <- 0 until config.p/2) {
//
//      val skip = 1
//      val start = j*2
//
//      // hook it up
//      val stage_delay = (config.bp/pow(2,i+2)).toInt
//      println(f"stage delay = $stage_delay")
//      sync(i+1) := ShiftRegister(sync(i), stage_delay, io.in.valid)
//      val twiddle = DspComplex.wire(implicitly[Real[T]].fromDouble(1.0), implicitly[Real[T]].fromDouble(0.0))//indices_rom(sync(i+1)*UInt(config.bp-1)
//      val mux_out = BarrelShifter(Vec(stage_outputs(i)(start), ShiftRegister(stage_outputs(i)(start+skip), stage_delay, io.in.valid)), sync(i+1)(log2Up(config.bp)-i-1))
//      List(stage_outputs(i+1)(start), stage_outputs(i+1)(start+skip)).zip(Butterfly(List(ShiftRegister(mux_out(0), stage_delay, io.in.valid), mux_out(1)), twiddle)).foreach { x =>
//        x._1 := ShiftRegister(x._2, config.pipe(i), io.in.valid)
//      }
//
//    }
//  }
//
//  // wire up top-level outputs
//  io.out.bits := stage_outputs(log2Up(config.p))
//
//}
