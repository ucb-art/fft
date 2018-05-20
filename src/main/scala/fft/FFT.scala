// See LICENSE for license details.

// Author: Stevo Bailey (stevo.bailey@berkeley.edu)

package fft

import chisel3.util._
import chisel3._
import chisel3.experimental._
import chisel3.core.ExplicitCompileOptions
import chisel3.internal.firrtl.KnownBinaryPoint
import dsptools._
import dsptools.numbers._
import dsptools.numbers.implicits._
import dspjunctions._
import dspblocks._
import scala.math._
import rocketchip.PeripheryUtils
import junctions._
import craft._
import cde._
import testchipip._
import uncore.tilelink._


class DirectFFTIO[T<:Data:Real](genMid: DspComplex[T])(implicit val p: Parameters) extends Bundle with HasGenParameters[DspComplex[T], DspComplex[T]] {
  val in = Input(ValidWithSync(Vec(lanesIn, genMid)))
  val out = Output(ValidWithSync(Vec(lanesOut, genOut())))
}

/**
  * fast fourier transform - cooley-tukey algorithm, decimation-in-time
  * direct form version
  * note, this is always a p-point FFT, though the twiddle factors will be different if p < n
  * @tparam T
  */
class DirectFFT[T<:Data:Real](genMid: DspComplex[T], genTwiddle: DspComplex[T], genOutFull: DspComplex[T])(implicit val p: Parameters) extends Module with HasGenParameters[DspComplex[T], DspComplex[T]] {
  val config: FFTConfig = p(FFTKey(p(DspBlockId)))
  val lanes_new = if (config.quadrature) lanesIn/2 else lanesIn

  val io = IO(new DirectFFTIO[T](genMid))

  // synchronize
  val valid_delay = Reg(next=io.in.valid)
  val sync = CounterWithReset(true.B, config.bp, io.in.sync, ~valid_delay & io.in.valid)._1
  io.out.sync := ShiftRegisterWithReset(io.in.valid && sync === (config.bp-1).U, config.direct_pipe, 0.U) // should valid keep sync from propagating?
  io.out.valid := ShiftRegisterWithReset(io.in.valid, config.direct_pipe, 0.U)

  // wire up twiddles
  val genTwiddleReal = genTwiddle.real
  val genTwiddleImag = genTwiddle.imag
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
    twiddle
  }))
  val indices_rom = Vec(config.dindices.map(x => UInt(x)))
  require(isPow2(lanes_new), "FFT lanes must be power of 2")
  val start = (sync << UInt(log2Ceil(lanes_new))) - sync
  val twiddle = Vec.fill(lanes_new-1)(Wire(genTwiddle))
  // special case when n = 4, because the pattern breaks down
  if (config.n == 4) {
    twiddle := Vec((0 until lanes_new-1).map(x => {
      val true_branch  = Wire(genTwiddle)
      true_branch     := twiddle_rom(0).divj()
      val false_branch = Wire(genTwiddle)
      false_branch    := twiddle_rom(0)
      Mux(
        indices_rom(start+UInt(x))(log2Ceil(config.n/4)),
        true_branch,
        false_branch
      )
    }))
  } else {
    twiddle := Vec((0 until lanes_new-1).map(x => {
      val index = indices_rom(start+UInt(x))
      val true_branch  = Wire(genTwiddle)
      true_branch     := twiddle_rom(index(log2Ceil(config.n/4)-1, 0)).divj()
      val false_branch = Wire(genTwiddle)
      false_branch    := twiddle_rom(index)

      Mux(index(log2Ceil(config.n/4)),
          true_branch,
          false_branch
      )
    }))
  }

  // p-point decimation-in-time direct form FFT with inputs in normal order
  // (outputs bit reversed)
  if (config.quadrature) {
    val stage_outputs_i = List.fill(log2Up(lanes_new)+1)(List.fill(lanes_new)(Wire(genOutFull)))
    val stage_outputs_q = List.fill(log2Up(lanes_new)+1)(List.fill(lanes_new)(Wire(genOutFull)))

    stage_outputs_i(0).zip(io.in.bits.take(lanes_new)).foreach { case(out, in) => out := in }
    stage_outputs_q(0).zip(io.in.bits.drop(lanes_new)).foreach { case(out, in) => out := in }

    // indices to the twiddle Vec
    var indices = List(List(0,1),List(0,2))
    for (i <- 0 until log2Up(lanes_new)-2) {
      indices = indices.map(x => x.map(y => y+1))
      val indices_max = indices.foldLeft(0)((b,a) => max(b,a.reduceLeft((d,c) => max(c,d))))
      indices = indices ++ indices.map(x => x.map(y => y+indices_max))
      indices = indices.map(x => 0 +: x)
    }

    // create the FFT hardware
    for (i <- 0 until log2Up(lanes_new)) {
      for (j <- 0 until lanes_new/2) {

        val skip = pow(2,log2Up(config.n/2)-(i+log2Ceil(config.bp))).toInt
        val start = ((j % skip) + floor(j/skip) * skip*2).toInt
        val shr_delay = config.pipe.drop(log2Ceil(config.bp)).dropRight(log2Up(lanes_new)-i).foldLeft(0)(_+_)
        val shr = ShiftRegisterMem[DspComplex[T]](twiddle(indices(j)(i)), shr_delay, name = this.name + s"_${i}_${j}_twiddle_sram")

        // hook it up
        val outputs_i           = List(stage_outputs_i(i+1)(start), stage_outputs_i(i+1)(start+skip))
        val butterfly_outputs_i = Butterfly[T](Seq(stage_outputs_i(i)(start), stage_outputs_i(i)(start+skip)), shr)
        outputs_i.zip(butterfly_outputs_i).foreach { x =>
          x._1 := ShiftRegisterMem(x._2, config.pipe(i+log2Ceil(config.bp)), name = this.name + s"_${i}_${j}_pipeline_sram")
        }

        // hook it up
        val outputs_q           = List(stage_outputs_q(i+1)(start), stage_outputs_q(i+1)(start+skip))
        val butterfly_outputs_q = Butterfly[T](Seq(stage_outputs_q(i)(start), stage_outputs_q(i)(start+skip)), shr)
        outputs_q.zip(butterfly_outputs_q).foreach { x =>
          x._1 := ShiftRegisterMem(x._2, config.pipe(i+log2Ceil(config.bp)), name = this.name + s"_${i}_${j}_pipeline_sram")
        }

      }
    }

    // wire up top-level outputs
    // note, truncation happens here!
    stage_outputs_i(log2Up(lanes_new)).zip(io.out.bits.take(lanes_new)).foreach { case (in, out) => out := in }
    stage_outputs_q(log2Up(lanes_new)).zip(io.out.bits.drop(lanes_new)).foreach { case (in, out) => out := in }
  } else {
    val stage_outputs = List.fill(log2Up(lanesIn)+1)(List.fill(lanesIn)(Wire(genOutFull)))
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
        val shr               = ShiftRegisterMem[DspComplex[T]](twiddle(indices(j)(i)), shr_delay, name = this.name + s"_${i}_${j}_twiddle_sram")
        val butterfly_outputs = Butterfly[T](Seq(stage_outputs(i)(start), stage_outputs(i)(start+skip)), shr)
        outputs.zip(butterfly_outputs).foreach { x =>
          x._1 := ShiftRegisterMem(x._2, config.pipe(i+log2Ceil(config.bp)), name = this.name + s"_${i}_${j}_pipeline_sram")
        }

      }
    }

    // wire up top-level outputs
    // note, truncation happens here!
    io.out.bits := stage_outputs(log2Up(lanesIn))
  }
}

class BiplexFFTIO[T<:Data:Real](genMid: DspComplex[T])(implicit val p: Parameters) extends Bundle with HasGenParameters[DspComplex[T], DspComplex[T]] {
  val in = Input(ValidWithSync(Vec(lanesIn, genIn())))
  val out = Output(ValidWithSync(Vec(lanesOut, genMid)))
}

/**
  * fast fourier transform - cooley-tukey algorithm, decimation-in-time
  * biplex pipelined version
  * note, this is always a bp-point FFT
  * @tparam T
  */
class BiplexFFT[T<:Data:Real](genMid: DspComplex[T], genTwiddle: DspComplex[T])(implicit val p: Parameters) extends Module with HasGenParameters[DspComplex[T], DspComplex[T]] {
  val config = p(FFTKey(p(DspBlockId)))

  val io = IO(new BiplexFFTIO[T](genMid))
  val lanes_new = if (config.quadrature) lanesIn/2 else lanesIn

  // synchronize
  val stage_delays = (0 until log2Up(config.bp)+1).map(x => { if (x == log2Up(config.bp)) config.bp/2 else (config.bp/pow(2,x+1)).toInt })
  val sync = List.fill(log2Up(config.bp)+1)(Wire(UInt(width=log2Up(config.bp))))
  val valid_delay = Reg(next=io.in.valid)
  //sync(0) := CounterWithReset(true.B, config.bp, io.in.sync, ~valid_delay & io.in.valid)._1
  //sync.drop(1).zip(sync).zip(stage_delays).foreach { case ((next, prev), delay) => next := ShiftRegisterWithReset(prev, delay, 0.U) }
  def sync_reset_val(index: Int): Int = {
    if (index == 0) { 0 }
    else { (-stage_delays.take(index).reduce(_+_)%config.bp)+config.bp }
  }
  sync.zipWithIndex.foreach { case (s, i) => s := CounterWithReset(true.B, config.bp, io.in.sync, ~valid_delay & io.in.valid, sync_reset_val(i).U, sync_reset_val(i).U)._1 }
  io.out.sync := sync(log2Up(config.bp)) === UInt((config.bp/2-1+config.biplex_pipe)%config.bp)
  // [stevo]: delay is 2*stage delays, since worst-case goes through both
  io.out.valid := ShiftRegisterWithReset(io.in.valid, stage_delays.reduce(_+_) + config.biplex_pipe, 0.U)

  // wire up twiddles
  val genTwiddleReal = genTwiddle.real
  val genTwiddleImag = genTwiddle.imag
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
  val indices = (0 until log2Up(config.bp)).map(x => indices_rom(UInt((pow(2,x)-1).toInt) +& { if (x == 0) UInt(0) else ShiftRegisterMem(sync(x+1), config.pipe.dropRight(log2Up(config.n)-x).reduceRight(_+_), name = this.name + s"_twiddle_sram")(log2Up(config.bp)-2,log2Up(config.bp)-1-x) }))
  val twiddle = Vec.fill(log2Up(config.bp))(Wire(genTwiddle))
  // special cases
  if (config.n == 4) {
    twiddle := Vec((0 until log2Up(config.bp)).map(x => {
      val true_branch  = Wire(genTwiddle)
      val false_branch = Wire(genTwiddle)
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
      val true_branch  = Wire(genTwiddle)
      val false_branch = Wire(genTwiddle)
      true_branch     := twiddle_rom(indices(x)(log2Ceil(config.n/4)-1, 0)).divj()
      false_branch    := twiddle_rom(indices(x))
      Mux(indices(x)(log2Ceil(config.n/4)), true_branch, false_branch)
    }))
  }


  // bp-point decimation-in-time biplex pipelined FFT with outputs in bit-reversed order
  // up-scale to genMid immediately for simplicity
  if (config.quadrature) {
    val stage_outputs_i = List.fill(log2Up(config.bp)+2)(List.fill(lanes_new)(Wire(genMid)))
    val stage_outputs_q = List.fill(log2Up(config.bp)+2)(List.fill(lanes_new)(Wire(genMid)))

    stage_outputs_i(0).zip(io.in.bits.take(lanes_new)).foreach { case(out, in) => out := in }
    stage_outputs_q(0).zip(io.in.bits.drop(lanes_new)).foreach { case(out, in) => out := in }

    // create the FFT hardware
    for (i <- 0 until log2Up(config.bp)+1) {
      for (j <- 0 until lanes_new/2) {

        val skip = 1
        val start = j*2

        // hook it up
        // last stage just has one extra permutation, no butterfly
        val mux_out_i = BarrelShifter(Vec(stage_outputs_i(i)(start), ShiftRegisterMem(stage_outputs_i(i)(start+skip), stage_delays(i), name = this.name + s"_${i}_${j}_mux0i_sram")), ShiftRegisterMem(sync(i)(log2Up(config.bp)-1 - { if (i == log2Up(config.bp)) 0 else i }), {if (i == 0) 0 else config.pipe.dropRight(log2Up(config.n)-i).reduceRight(_+_)},  name = this.name + s"_${i}_${j}_mux1i_sram"))
        val mux_out_q = BarrelShifter(Vec(stage_outputs_q(i)(start), ShiftRegisterMem(stage_outputs_q(i)(start+skip), stage_delays(i), name = this.name + s"_${i}_${j}_mux0q_sram")), ShiftRegisterMem(sync(i)(log2Up(config.bp)-1 - { if (i == log2Up(config.bp)) 0 else i }), {if (i == 0) 0 else config.pipe.dropRight(log2Up(config.n)-i).reduceRight(_+_)},  name = this.name + s"_${i}_${j}_mux1q_sram"))
        if (i == log2Up(config.bp)) {
          Seq(stage_outputs_i(i+1)(start), stage_outputs_i(i+1)(start+skip)).zip(Seq(ShiftRegisterMem(mux_out_i(0), stage_delays(i), name = this.name + s"_${i}_${j}_lasti_sram" ), mux_out_i(1))).foreach { x => x._1 := x._2 }
          Seq(stage_outputs_q(i+1)(start), stage_outputs_q(i+1)(start+skip)).zip(Seq(ShiftRegisterMem(mux_out_q(0), stage_delays(i), name = this.name + s"_${i}_${j}_lastq_sram" ), mux_out_q(1))).foreach { x => x._1 := x._2 }
        } else {
          Seq(stage_outputs_i(i+1)(start), stage_outputs_i(i+1)(start+skip)).zip(Butterfly(Seq(ShiftRegisterMem(mux_out_i(0), stage_delays(i), name = this.name + s"_${i}_${j}_pipeline0i_sram"), mux_out_i(1)), twiddle(i))).foreach { x => x._1 := ShiftRegisterMem(x._2, config.pipe(i), name = this.name + s"_${i}_${j}_pipeline1i_sram") }
          Seq(stage_outputs_q(i+1)(start), stage_outputs_q(i+1)(start+skip)).zip(Butterfly(Seq(ShiftRegisterMem(mux_out_q(0), stage_delays(i), name = this.name + s"_${i}_${j}_pipeline0q_sram"), mux_out_q(1)), twiddle(i))).foreach { x => x._1 := ShiftRegisterMem(x._2, config.pipe(i), name = this.name + s"_${i}_${j}_pipeline1q_sram") }
        }

      }
    }

    // wire up top-level outputs
    stage_outputs_i(log2Up(config.bp)+1).zip(io.out.bits.take(lanes_new)).foreach { case (in, out) => out := in }
    stage_outputs_q(log2Up(config.bp)+1).zip(io.out.bits.drop(lanes_new)).foreach { case (in, out) => out := in }
  } else {
    val stage_outputs = List.fill(log2Up(config.bp)+2)(List.fill(lanesIn)(Wire(genMid)))
    io.in.bits.zip(stage_outputs(0)).foreach { case(in, out) => out := in }

    // create the FFT hardware
    for (i <- 0 until log2Up(config.bp)+1) {
      for (j <- 0 until lanesIn/2) {

        val skip = 1
        val start = j*2

        // hook it up
        // last stage just has one extra permutation, no butterfly
        val mux_out = BarrelShifter(Vec(stage_outputs(i)(start), ShiftRegisterMem(stage_outputs(i)(start+skip), stage_delays(i), name = this.name + s"_${i}_${j}_mux0_sram")), ShiftRegisterMem(sync(i)(log2Up(config.bp)-1 - { if (i == log2Up(config.bp)) 0 else i }), {if (i == 0) 0 else config.pipe.dropRight(log2Up(config.n)-i).reduceRight(_+_)},  name = this.name + s"_${i}_${j}_mux1_sram"))
        if (i == log2Up(config.bp)) {
          Seq(stage_outputs(i+1)(start), stage_outputs(i+1)(start+skip)).zip(Seq(ShiftRegisterMem(mux_out(0), stage_delays(i), name = this.name + s"_${i}_${j}_last_sram" ), mux_out(1))).foreach { x => x._1 := x._2 }
        } else {
          Seq(stage_outputs(i+1)(start), stage_outputs(i+1)(start+skip)).zip(Butterfly(Seq(ShiftRegisterMem(mux_out(0), stage_delays(i), name = this.name + s"_${i}_${j}_pipeline0_sram"), mux_out(1)), twiddle(i))).foreach { x => x._1 := ShiftRegisterMem(x._2, config.pipe(i), name = this.name + s"_${i}_${j}_pipeline1_sram") }
        }

      }
    }

    // wire up top-level outputs
    io.out.bits := stage_outputs(log2Up(config.bp)+1)
  }

}

/**
  * IO Bundle for FFT
  * @tparam T
  */
class FFTIO[T<:Data:Real]()(implicit val p: Parameters) extends Bundle with HasGenParameters[DspComplex[T], DspComplex[T]] {

  val in = Input(ValidWithSync(Vec(lanesIn, genIn())))
  val out = Output(ValidWithSync(Vec(lanesOut, genOut())))

  val data_set_end_status = Output(Bool())
  val data_set_end_clear = Input(Bool())
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
  val lanes_new = if (config.quadrature) lanesIn/2 else lanesIn
  require(lanes_new >= 2, "Must have at least 2 parallel inputs")
  require(isPow2(lanes_new), "FFT parallelism must be a power of 2")
  require(lanes_new <= config.n, "An n-point FFT cannot have more than n inputs (p must be less than or equal to n)")

  val io = IO(new FFTIO[T])

  // calculate direct FFT input bitwidth
  // this is just the input total width + growth of 1 bit per biplex stage
  val genMid: DspComplex[T] = {
    if (config.bp == 1) { genIn() }
    else {
      val growth = log2Up(config.bp)/2
      genIn().asInstanceOf[DspComplex[T]].underlyingType() match {
        case "fixed" =>
          genIn().asInstanceOf[DspComplex[T]].real.asInstanceOf[FixedPoint].binaryPoint match {
            case KnownBinaryPoint(binaryPoint) =>
              val totalBits = genIn().asInstanceOf[DspComplex[T]].real.getWidth + growth
              DspComplex(FixedPoint(totalBits.W, binaryPoint.BP), FixedPoint(totalBits.W, binaryPoint.BP)).asInstanceOf[DspComplex[T]]
            case _ => throw new DspException("Error: unknown binary point when calculating FFT bitwdiths")
          }
        case "sint" => {
          val totalBits = genIn().asInstanceOf[DspComplex[T]].real.getWidth + growth
          DspComplex(SInt(totalBits.W), SInt(totalBits.W)).asInstanceOf[DspComplex[T]]
        }
        case _ => throw new DspException("Error: unknown type when calculating FFT bitwidths")
      }
    }
  }

  // calculate twiddle factor bitwidth
  // total input bits
  val genTwiddleBiplex: DspComplex[T] = {
    val growth = log2Up(config.bp)/2
    genIn().asInstanceOf[DspComplex[T]].underlyingType() match {
      case "fixed" =>
        genIn().asInstanceOf[DspComplex[T]].real.asInstanceOf[FixedPoint].binaryPoint match {
          case KnownBinaryPoint(binaryPoint) =>
            val totalBits = genIn().asInstanceOf[DspComplex[T]].real.getWidth + growth
            DspComplex(FixedPoint(totalBits.W, (totalBits-2).BP), FixedPoint(totalBits.W, (totalBits-2).BP)).asInstanceOf[DspComplex[T]]
          case _ => throw new DspException("Error: unknown binary point when calculating FFT bitwdiths")
        }
      case "sint" => {
        val totalBits = genIn().asInstanceOf[DspComplex[T]].real.getWidth + growth
        DspComplex(SInt(totalBits.W), SInt(totalBits.W)).asInstanceOf[DspComplex[T]]
      }
      case _ => throw new DspException("Error: unknown type when calculating FFT bitwidths")
    }
  }

  val genTwiddleDirect: DspComplex[T] = {
    val growth = log2Up(config.n)/2
    genIn().asInstanceOf[DspComplex[T]].underlyingType() match {
      case "fixed" =>
        genIn().asInstanceOf[DspComplex[T]].real.asInstanceOf[FixedPoint].binaryPoint match {
          case KnownBinaryPoint(binaryPoint) =>
            val totalBits = genIn().asInstanceOf[DspComplex[T]].real.getWidth + growth
            DspComplex(FixedPoint(totalBits.W, (totalBits-2).BP), FixedPoint(totalBits.W, (totalBits-2).BP)).asInstanceOf[DspComplex[T]]
          case _ => throw new DspException("Error: unknown binary point when calculating FFT bitwdiths")
        }
      case "sint" => {
        val totalBits = genIn().asInstanceOf[DspComplex[T]].real.getWidth + growth
        DspComplex(SInt(totalBits.W), SInt(totalBits.W)).asInstanceOf[DspComplex[T]]
      }
      case _ => throw new DspException("Error: unknown type when calculating FFT bitwidths")
    }
  }

  // calculate direct FFT output bitwidth
  // this is just the input total width + growth of 1 bit per FFT stage
  val genOutDirect: DspComplex[T] = {
    if (config.bp == 1) { genIn() }
    else {
      val growth = log2Up(config.n)/2
      genIn().asInstanceOf[DspComplex[T]].underlyingType() match {
        case "fixed" =>
          genIn().asInstanceOf[DspComplex[T]].real.asInstanceOf[FixedPoint].binaryPoint match {
            case KnownBinaryPoint(binaryPoint) =>
              val totalBits = genIn().asInstanceOf[DspComplex[T]].real.getWidth + growth
              DspComplex(FixedPoint(totalBits.W, binaryPoint.BP), FixedPoint(totalBits.W, binaryPoint.BP)).asInstanceOf[DspComplex[T]]
            case _ => throw new DspException("Error: unknown binary point when calculating FFT bitwdiths")
          }
        case "sint" => {
          val totalBits = genIn().asInstanceOf[DspComplex[T]].real.getWidth + growth
          DspComplex(SInt(totalBits.W), SInt(totalBits.W)).asInstanceOf[DspComplex[T]]
        }
        case _ => throw new DspException("Error: unknown type when calculating FFT bitwidths")
      }
    }
  }

  // feed in zeros when invalid
  val in = Wire(ValidWithSync(Vec(lanesIn, genIn())))
  when (io.in.valid) {
    in.bits := io.in.bits
  } .otherwise {
    in.bits := Vec.fill(lanesIn)(DspComplex(Real[T].zero, Real[T].zero))
  }
  in.valid := io.in.valid
  in.sync := io.in.sync

  // data set end flag
  val valid_delay = Reg(next=io.out.valid)
  val dses = Reg(init=false.B)
  when (io.data_set_end_clear) {
    dses := false.B
  } .elsewhen (valid_delay & ~io.out.valid) {
    dses := true.B
  }
  io.data_set_end_status := dses

  // instantiate sub-FFTs
  val direct = Module(new DirectFFT[T](genMid, genTwiddleDirect, genOutDirect))
  io.out <> direct.io.out

  if (config.n != lanes_new) {
    val biplex = Module(new BiplexFFT[T](genMid, genTwiddleBiplex))
    direct.io.in := biplex.io.out
    biplex.io.in <> in
  } else {
    direct.io.in <> in
  }
}
