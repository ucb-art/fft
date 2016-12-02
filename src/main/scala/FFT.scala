// See LICENSE for license details.

// Author: Stevo Bailey (stevo.bailey@berkeley.edu)

package fft

import chisel3.util._
import chisel3._
import chisel3.core.ExplicitCompileOptions
import dsptools._
import dsptools.numbers.{DspComplex, Real}
import dsptools.numbers.implicits._
import dsptools.junctions._
import dsptools.counters._
import scala.math._
import rocketchip.PeripheryUtils
import junctions._
import cde._
import testchipip._
import uncore.tilelink._

// shouldn't be needed, but evidently it is
// import  ExplicitCompileOptions.NotStrict

// fast fourier transform io
class FFTIO[T<:Data:Real](genIn: => DspComplex[T], genOut: => Option[DspComplex[T]] = None,
  val config: FFTConfig = FFTConfig())(implicit val p: Parameters) extends Bundle {

  val in = Input(ValidWithSync(Vec(config.p, genIn)))
  val out = Output(ValidWithSync(Vec(config.p, genOut.getOrElse(genIn))))
}

// fast fourier transform - cooley-tukey algorithm, decimation-in-time
// direct form version
// note, this is always a p-point FFT, though the twiddle factors will be different if p < n
class DirectFFT[T<:Data:Real](genIn: => DspComplex[T], genOut: => Option[DspComplex[T]] = None, genTwiddle: => Option[DspComplex[T]] = None,
  val config: FFTConfig = FFTConfig())(implicit val p: Parameters) extends Module(){

  val io = IO(new FFTIO(genIn, genOut, config))

  // synchronize
  val sync = CounterWithReset(io.in.valid, config.bp, io.in.sync && io.in.valid)._1
  io.out.sync := ShiftRegisterMem(io.in.sync, config.direct_pipe, io.in.valid)
  io.out.valid := io.in.valid

  // wire up twiddles
  //val twiddle_rom = Vec(config.twiddle.map(x => DspComplex.wire(implicitly[Real[T]].fromDouble(x(0)), implicitly[Real[T]].fromDouble(x(1)))))
  // hack:
  val twiddle_rom = Wire(Vec(config.twiddle.size, genTwiddle.getOrElse(genIn)))
  twiddle_rom.zip(config.twiddle).foreach { case(rom, value) => rom := DspComplex.wire(implicitly[Real[T]].fromDouble(value(0)), implicitly[Real[T]].fromDouble(value(1))) }
  val indices_rom = Vec(config.dindices.map(x => UInt(x)))
  // TODO: make this not a multiply
  val start = sync*UInt(config.p-1)
  val twiddle = Vec.fill(config.p-1)(Wire(genTwiddle.getOrElse(genIn)))
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
    val indices_max = indices.foldLeft(0)((b,a) => max(b,a.reduceLeft((d,c) => max(c,d))))
    indices = indices ++ indices.map(x => x.map(y => y+indices_max))
    indices = indices.map(x => 0 +: x)
  }

  // create the FFT hardware
  for (i <- 0 until log2Up(config.p)) {
    for (j <- 0 until config.p/2) {

      val skip = pow(2,log2Up(config.n/2)-(i+log2Ceil(config.bp))).toInt
      val start = ((j % skip) + floor(j/skip) * skip*2).toInt

      // hook it up
      List(stage_outputs(i+1)(start), stage_outputs(i+1)(start+skip)).zip(Butterfly(List(stage_outputs(i)(start), stage_outputs(i)(start+skip)), ShiftRegisterMem(twiddle(indices(j)(i)), config.pipe.drop(log2Ceil(config.bp)).dropRight(log2Up(config.p)-i).foldLeft(0)(_+_), io.in.valid))).foreach { x =>
        x._1 := ShiftRegisterMem(x._2, config.pipe(i+log2Ceil(config.bp)), io.in.valid)
      }

    }
  }

  // wire up top-level outputs
  io.out.bits := stage_outputs(log2Up(config.p))
}

// fast fourier transform - cooley-tukey algorithm, decimation-in-time
// biplex pipelined version
// note, this is always a bp-point FFT
class BiplexFFT[T<:Data:Real](genIn: => DspComplex[T], genOut: => Option[DspComplex[T]] = None, genTwiddle: => Option[DspComplex[T]] = None,
  val config: FFTConfig = FFTConfig())(implicit p: Parameters) extends Module {

  val io = IO(new FFTIO(genIn, genOut, config))

  // synchronize
  val stage_delays = (0 until log2Up(config.bp)+1).map(x => { if (x == log2Up(config.bp)) config.bp/2 else (config.bp/pow(2,x+1)).toInt })
  val sync = List.fill(log2Up(config.bp)+1)(Wire(UInt(width=log2Up(config.bp))))
  sync(0) := CounterWithReset(io.in.valid, config.bp, io.in.sync && io.in.valid)._1
  sync.drop(1).zip(sync).zip(stage_delays).foreach { case ((next, prev), delay) => next := ShiftRegisterMem(prev, delay, io.in.valid) }
  io.out.sync := sync(log2Up(config.bp)) === UInt((config.bp/2-1+config.biplex_pipe)%config.bp)
  io.out.valid := io.in.valid

  // wire up twiddles
  //val twiddle_rom = Vec(config.twiddle.map(x => DspComplex.wire(implicitly[Real[T]].fromDouble(x(0)), implicitly[Real[T]].fromDouble(x(1)))))
  // hack:
  val twiddle_rom = Wire(Vec(config.twiddle.size, genTwiddle.getOrElse(genIn)))
  twiddle_rom.zip(config.twiddle).foreach { case(rom, value) => rom := DspComplex.wire(implicitly[Real[T]].fromDouble(value(0)), implicitly[Real[T]].fromDouble(value(1))) }
  val indices_rom = Vec(config.bindices.map(x => UInt(x)))
  val indices = (0 until log2Up(config.bp)).map(x => indices_rom(UInt((pow(2,x)-1).toInt) +& { if (x == 0) UInt(0) else ShiftRegisterMem(sync(x+1), config.pipe.dropRight(log2Up(config.n)-x).reduceRight(_+_), io.in.valid)(log2Up(config.bp)-2,log2Up(config.bp)-1-x) }))
  val twiddle = Vec.fill(log2Up(config.bp))(Wire(genTwiddle.getOrElse(genIn)))
  // special cases
  if (config.n == 4) {
    twiddle := Vec((0 until log2Up(config.bp)).map(x => Mux(indices(x)(log2Ceil(config.n/4)), DspComplex.divideByJ(twiddle_rom(0)), twiddle_rom(0))))
  } else if (config.bp == 2) {
    twiddle := Vec((0 until log2Up(config.bp)).map(x => twiddle_rom(indices(x))))
  } else {
    twiddle := Vec((0 until log2Up(config.bp)).map(x => Mux(indices(x)(log2Ceil(config.n/4)), DspComplex.divideByJ(twiddle_rom(indices(x)(log2Ceil(config.n/4)-1, 0))), twiddle_rom(indices(x)))))
  }

  // bp-point decimation-in-time biplex pipelined FFT with outputs in bit-reversed order
  // TODO: change type? should it all be genIn?
  val stage_outputs = List.fill(log2Up(config.bp)+2)(List.fill(config.p)(Wire(genIn)))
  io.in.bits.zip(stage_outputs(0)).foreach { case(in, out) => out := in }

  // create the FFT hardware
  for (i <- 0 until log2Up(config.bp)+1) {
    for (j <- 0 until config.p/2) {

      val skip = 1
      val start = j*2

      // hook it up
      // last stage just has one extra permutation, no butterfly
      val mux_out = BarrelShifter(Vec(stage_outputs(i)(start), ShiftRegisterMem(stage_outputs(i)(start+skip), stage_delays(i), io.in.valid)), ShiftRegisterMem(sync(i)(log2Up(config.bp)-1 - { if (i == log2Up(config.bp)) 0 else i }), {if (i == 0) 0 else config.pipe.dropRight(log2Up(config.n)-i).reduceRight(_+_)}, io.in.valid))
      if (i == log2Up(config.bp)) {
        List(stage_outputs(i+1)(start), stage_outputs(i+1)(start+skip)).zip(List(ShiftRegisterMem(mux_out(0), stage_delays(i), io.in.valid), mux_out(1))).foreach { x => x._1 := x._2 }
      } else {
        List(stage_outputs(i+1)(start), stage_outputs(i+1)(start+skip)).zip(Butterfly(List(ShiftRegisterMem(mux_out(0), stage_delays(i), io.in.valid), mux_out(1)), twiddle(i))).foreach { x => x._1 := ShiftRegisterMem(x._2, config.pipe(i), io.in.valid) }
      }

    }
  }

  // wire up top-level outputs
  io.out.bits := stage_outputs(log2Up(config.bp)+1)

}

// fast fourier transform - cooley-tukey algorithm, decimation-in-time
// mixed version
// note, this is always an n-point FFT
class FFTUnpacked[T<:Data:Real](genIn: => DspComplex[T], genOut: => Option[DspComplex[T]] = None, genTwiddle: => Option[DspComplex[T]] = None,
  val config: FFTConfig = FFTConfig())(implicit p: Parameters) extends Module {

  val io = IO(new FFTIO(genIn, genOut, config))
  
  val direct = Module(new DirectFFT(genIn, genOut, genTwiddle, config))
  io.out <> direct.io.out

  if (config.n != config.p) {
    val biplex = Module(new BiplexFFT(genIn, genOut, genTwiddle, config))
    direct.io.in := biplex.io.out
    biplex.io.in <> io.in
  } else {
    direct.io.in <> io.in
  }
}



// packing hack
class FFTIOPacked[T<:Data:Real](genIn: => DspComplex[T], genOut: => Option[DspComplex[T]] = None,
  val config: FFTConfig = FFTConfig()) extends Bundle {

  val in = Input(ValidWithSync(Wire(Vec(config.p, genIn)).asUInt))
  val out = Output(ValidWithSync(Wire(Vec(config.p, genOut.getOrElse(genIn))).asUInt))
}

class FFT[T<:Data:Real](genIn: => DspComplex[T], genOut: => Option[DspComplex[T]] = None, genTwiddle: => Option[DspComplex[T]] = None,
  val config: FFTConfig = FFTConfig())(implicit p: Parameters) extends Module {

  val io = IO(new FFTIOPacked(genIn, genOut, config))
  val fft = Module(new FFTUnpacked(genIn, genOut, genTwiddle, config))

  fft.io.in.bits := Vec(config.p, genIn).fromBits(io.in.bits)
  fft.io.in.valid := io.in.valid
  fft.io.in.sync := io.in.sync

  io.out.bits := fft.io.out.bits.asUInt
  io.out.valid := fft.io.out.valid
  io.out.sync := fft.io.out.sync
}

class FFT2[T<:Data:Real](genTwiddle: => Option[DspComplex[T]] = None)(implicit p: Parameters) 
  extends StreamBlock[DspComplex[T], DspComplex[T]]()(p) {
  val baseAddr = BigInt(0)
  val config = p(FFTKey)
  val fft = Module(new FFTUnpacked(genIn(), Some(genOut()), genTwiddle, config))

  addControl("fftControl", 3.U)
  addStatus("fftStatus")

  fft.io.in <> unpacked_input
  fft.io.in.sync := control("fftControl")(0)

  unpacked_output <> fft.io.out
  status("fftStatus") := unpacked_output.sync
}
