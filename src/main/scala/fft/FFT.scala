// See LICENSE for license details.

// Author: Stevo Bailey (stevo.bailey@berkeley.edu)

package fft

import chisel3.util._
import chisel3._
import chisel3.core.ExplicitCompileOptions
import dsptools._
import dsptools.numbers._
import dsptools.numbers.implicits._
import dspjunctions._
import dspblocks._
import scala.math._
import rocketchip.PeripheryUtils
import junctions._
import cde._
import testchipip._
import uncore.tilelink._

/**
  * IO Bundle for FFT
  * @tparam T
  */
class FFTIO[T<:Data:Real]()(implicit val p: Parameters) extends Bundle with HasGenParameters[DspComplex[T], DspComplex[T]] {

  val in = Input(ValidWithSync(Vec(lanesIn, genIn())))
  val out = Output(ValidWithSync(Vec(lanesOut, genOut())))
}

/**
  * fast fourier transform - cooley-tukey algorithm, decimation-in-time
  * direct form version
  * note, this is always a p-point FFT, though the twiddle factors will be different if p < n
  * @tparam T
  */
class DirectFFT[T<:Data:Real]()(implicit val p: Parameters) extends Module
    with HasFFTGenParameters[DspComplex[T]] {
  val config: FFTConfig = p(FFTKey(p(DspBlockId)))

  val io = IO(new FFTIO[T])

  // synchronize
  val valid_delay = Reg(next=io.in.valid)
  val sync = CounterWithReset(true.B, config.bp, io.in.sync, ~valid_delay & io.in.valid)._1
  io.out.sync := ShiftRegisterWithReset(io.in.sync, config.direct_pipe, 0.U, io.in.valid) // should valid keep sync from propagating?
  io.out.valid := ShiftRegisterWithReset(io.in.valid, config.direct_pipe, 0.U, true.B)

  // wire up twiddles
  val genTwiddleReal = genTwiddle.getOrElse(genOut()).real
  val genTwiddleImag = genTwiddle.getOrElse(genOut()).imag
  // This should work and would simplify the firrtl, but... it doesn't seem to work
  //val twiddle_rom = Vec(config.twiddle.map(x =>
  //  DspComplex(genTwiddleReal.fromDoubleWithFixedWidth(x(0)), genTwiddleImag.fromDoubleWithFixedWidth(x(1)))
  //))
  val twiddle_rom = Vec(config.twiddle.map( x => {
    val real = Wire(genTwiddleReal.cloneType)
    val imag = Wire(genTwiddleImag.cloneType)
    real := genTwiddleReal.fromDouble(x(0))
    imag := genTwiddleImag.fromDouble(x(1))
    val twiddle = Wire(DspComplex(genTwiddleReal, genTwiddleImag))
    twiddle.real := real
    twiddle.imag := imag
    // val a = DspComplex.protoWithFixedWidth(breeze.math.Complex(x(0), x(1)), genTwiddle.getOrElse(genOut()).real)
    println(s"Wire(${x(0)}, ${x(1)}) has widths (${twiddle.real.getWidth}, ${twiddle.imag.getWidth})")
    twiddle
  }))
  val indices_rom = Vec(config.dindices.map(x => UInt(x)))
  // TODO: make this not a multiply
  val start = sync*UInt(lanesIn-1)
  val twiddle = Vec.fill(lanesIn-1)(Wire(genTwiddle.getOrElse(genOut())))
  // special case when n = 4, because the pattern breaks down
  if (config.n == 4) {
    twiddle := Vec((0 until lanesIn-1).map(x => {
      val true_branch  = Wire(genTwiddle.getOrElse(genOut()))
      true_branch     := twiddle_rom(0).divj()
      val false_branch = Wire(genTwiddle.getOrElse(genOut()))
      false_branch    := twiddle_rom(0)
      Mux(
        indices_rom(start+UInt(x))(log2Ceil(config.n/4)),
        true_branch,
        false_branch
      )
    }))
  } else {
    twiddle := Vec((0 until lanesIn-1).map(x => {
      val true_branch  = Wire(genTwiddle.getOrElse(genOut()))
      true_branch     := twiddle_rom(indices_rom(start+UInt(x))(log2Ceil(config.n/4)-1, 0)).divj()
      val false_branch = Wire(genTwiddle.getOrElse(genOut()))
      false_branch    := twiddle_rom(indices_rom(start+UInt(x)))

      val index = indices_rom(start+UInt(x))
      Mux(index(log2Ceil(config.n/4)),
          true_branch,
          false_branch
      )
    }))
  }

  // p-point decimation-in-time direct form FFT with inputs in normal order
  // (outputs bit reversed)
  val stage_outputs = List.fill(log2Up(lanesIn)+1)(List.fill(lanesIn)(Wire(genOut())))
  io.in.bits.zip(stage_outputs(0)).foreach { case(in, out) => out := in }

  // indices to the twiddle Vec
  var indices = List(List(0,1),List(0,2))
  for (i <- 0 until log2Up(lanesIn)-2) {
    indices = indices.map(x => x.map(y => y+1))
    val indices_max = indices.foldLeft(0)((b,a) => max(b,a.reduceLeft((d,c) => max(c,d))))
    indices = indices ++ indices.map(x => x.map(y => y+indices_max))
    indices = indices.map(x => 0 +: x)
  }

  // create the FFT hardware
  for (i <- 0 until log2Up(lanesIn)) {
    for (j <- 0 until lanesIn/2) {

      val skip = pow(2,log2Up(config.n/2)-(i+log2Ceil(config.bp))).toInt
      val start = ((j % skip) + floor(j/skip) * skip*2).toInt

      // hook it up
      val outputs           = List(stage_outputs(i+1)(start), stage_outputs(i+1)(start+skip))
      val shr_delay         = config.pipe.drop(log2Ceil(config.bp)).dropRight(log2Up(lanesIn)-i).foldLeft(0)(_+_)
      val shr               = ShiftRegisterMem[DspComplex[T]](twiddle(indices(j)(i)), shr_delay, io.in.valid)
      val butterfly_outputs = Butterfly[T](Seq(stage_outputs(i)(start), stage_outputs(i)(start+skip)), shr)
      outputs.zip(butterfly_outputs).foreach { x =>
        x._1 := ShiftRegisterMem(x._2, config.pipe(i+log2Ceil(config.bp)), io.in.valid)
      }

    }
  }

  // wire up top-level outputs
  io.out.bits := stage_outputs(log2Up(lanesIn))
}

/**
  * fast fourier transform - cooley-tukey algorithm, decimation-in-time
  * biplex pipelined version
  * note, this is always a bp-point FFT
  * @tparam T
  */
class BiplexFFT[T<:Data:Real]()(implicit val p: Parameters) extends Module with HasFFTGenParameters[DspComplex[T]] {
  val config = p(FFTKey(p(DspBlockId)))

  val io = IO(new FFTIO[T])

  // synchronize
  val stage_delays = (0 until log2Up(config.bp)+1).map(x => { if (x == log2Up(config.bp)) config.bp/2 else (config.bp/pow(2,x+1)).toInt })
  val sync = List.fill(log2Up(config.bp)+1)(Wire(UInt(width=log2Up(config.bp))))
  val valid_delay = Reg(next=io.in.valid)
  sync(0) := CounterWithReset(true.B, config.bp, io.in.sync, ~valid_delay & io.in.valid)._1
  sync.drop(1).zip(sync).zip(stage_delays).foreach { case ((next, prev), delay) => next := ShiftRegisterWithReset(prev, delay, 0.U, true.B) }
  io.out.sync := sync(log2Up(config.bp)) === UInt((config.bp/2-1+config.biplex_pipe)%config.bp)
  io.out.valid := ShiftRegisterWithReset(io.in.valid, stage_delays.reduce(_+_) + config.biplex_pipe, 0.U, true.B)

  // wire up twiddles
  val genTwiddleReal = genTwiddle.getOrElse(genOut()).real
  val genTwiddleImag = genTwiddle.getOrElse(genOut()).imag
  val twiddle_rom = Vec(config.twiddle.map(x => {
    val real = Wire(genTwiddleReal.cloneType)
    val imag = Wire(genTwiddleImag.cloneType)
    real := genTwiddleReal.fromDouble(x(0))
    imag := genTwiddleImag.fromDouble(x(1))
    val twiddle = Wire(DspComplex(genTwiddleReal, genTwiddleImag))
    twiddle.real := real
    twiddle.imag := imag
    twiddle
  }))
  val indices_rom = Vec(config.bindices.map(x => UInt(x)))
  val indices = (0 until log2Up(config.bp)).map(x => indices_rom(UInt((pow(2,x)-1).toInt) +& { if (x == 0) UInt(0) else ShiftRegisterMem(sync(x+1), config.pipe.dropRight(log2Up(config.n)-x).reduceRight(_+_))(log2Up(config.bp)-2,log2Up(config.bp)-1-x) }))
  val twiddle = Vec.fill(log2Up(config.bp))(Wire(genTwiddle.getOrElse(genOut())))
  // special cases
  if (config.n == 4) {
    twiddle := Vec((0 until log2Up(config.bp)).map(x => {
      val true_branch  = Wire(genTwiddle.getOrElse(genOut()))
      val false_branch = Wire(genTwiddle.getOrElse(genOut()))
      true_branch     := twiddle_rom(0).divj()
      false_branch    := twiddle_rom(0)
      Mux(indices(x)(log2Ceil(config.n/4)), true_branch, false_branch)
    }))
  } else if (config.bp == 2) {
    twiddle := Vec((0 until log2Up(config.bp)).map(x =>
      twiddle_rom(indices(x))
    ))
  } else {
    twiddle := Vec((0 until log2Up(config.bp)).map(x => {
      val true_branch  = Wire(genTwiddle.getOrElse(genOut()))
      val false_branch = Wire(genTwiddle.getOrElse(genOut()))
      true_branch     := twiddle_rom(indices(x)(log2Ceil(config.n/4)-1, 0)).divj()
      false_branch    := twiddle_rom(indices(x))
      Mux(indices(x)(log2Ceil(config.n/4)), true_branch, false_branch)
    }))
  }

  // bp-point decimation-in-time biplex pipelined FFT with outputs in bit-reversed order
  // TODO: change type? should it all be genIn?
  val stage_outputs = List.fill(log2Up(config.bp)+2)(List.fill(lanesIn)(Wire(genOut())))
  io.in.bits.zip(stage_outputs(0)).foreach { case(in, out) => out := in }

  // create the FFT hardware
  for (i <- 0 until log2Up(config.bp)+1) {
    for (j <- 0 until lanesIn/2) {

      val skip = 1
      val start = j*2

      // hook it up
      // last stage just has one extra permutation, no butterfly
      val mux_out = BarrelShifter(Vec(stage_outputs(i)(start), ShiftRegisterMem(stage_outputs(i)(start+skip), stage_delays(i))), ShiftRegisterMem(sync(i)(log2Up(config.bp)-1 - { if (i == log2Up(config.bp)) 0 else i }), {if (i == 0) 0 else config.pipe.dropRight(log2Up(config.n)-i).reduceRight(_+_)}))
      if (i == log2Up(config.bp)) {
        Seq(stage_outputs(i+1)(start), stage_outputs(i+1)(start+skip)).zip(Seq(ShiftRegisterMem(mux_out(0), stage_delays(i)), mux_out(1))).foreach { x => x._1 := x._2 }
      } else {
        Seq(stage_outputs(i+1)(start), stage_outputs(i+1)(start+skip)).zip(Butterfly(Seq(ShiftRegisterMem(mux_out(0), stage_delays(i)), mux_out(1)), twiddle(i))).foreach { x => x._1 := ShiftRegisterMem(x._2, config.pipe(i)) }
      }

    }
  }

  // wire up top-level outputs
  io.out.bits := stage_outputs(log2Up(config.bp)+1)

}

/**
  * fast fourier transform - cooley-tukey algorithm, decimation-in-time
  * mixed version
  * note, this is always an n-point FFT
  * @tparam T
  */
class FFT[T<:Data:Real]()(implicit val p: Parameters) extends Module with HasGenParameters[DspComplex[T], DspComplex[T]] {
  val config = p(FFTKey(p(DspBlockId)))

  require(lanesIn == lanesOut, "FFT must have an equal number of input and output lanes")
  require(lanesIn >= 2, "Must have at least 2 parallel inputs")
  require(isPow2(lanesIn), "FFT parallelism must be a power of 2")
  require(lanesIn <= config.n, "An n-point FFT cannot have more than n inputs (p must be less than or equal to n)")

  val io = IO(new FFTIO[T])

  // feed in zeros when invalid
  val in = Wire(ValidWithSync(Vec(lanesIn, genIn())))
  when (io.in.valid) {
    in.bits := io.in.bits
  } .otherwise {
    in.bits := Vec.fill(lanesIn)(DspComplex(Real[T].zero, Real[T].zero))
  }
  in.valid := io.in.valid
  in.sync := io.in.sync

  val direct = Module(new DirectFFT[T])
  io.out <> direct.io.out

  if (config.n != lanesIn) {
    val biplex = Module(new BiplexFFT[T])
    direct.io.in := biplex.io.out
    biplex.io.in <> in
  } else {
    direct.io.in <> in
  }
}
