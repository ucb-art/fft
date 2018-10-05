// See LICENSE for license details.

// Author: Stevo Bailey (stevo.bailey@berkeley.edu)

package fft

import chisel3.util._
import chisel3._
import chisel3.experimental._
import chisel3.core.ExplicitCompileOptions
import chisel3.internal.firrtl.KnownBinaryPoint
import craft._
import dsptools._
import dsptools.numbers._
import dsptools.numbers.implicits._
import dspjunctions._
import dspblocks._
import freechips.rocketchip.config.Parameters
import scala.math._


class DirectFFTIO[T<:Data:Real](genMid: DspComplex[T], genOut: DspComplex[T], lanes: Int)(implicit val p: Parameters) extends Bundle {
  val in = Input(ValidWithSync(Vec(lanes, genMid)))
  val out = Output(ValidWithSync(Vec(lanes, genOut)))
}

/**
 * fast fourier transform - cooley-tukey algorithm, decimation-in-time
 * direct form version
 * note, this is always a p-point FFT, though the twiddle factors will be different if p < n
 * @tparam T
 */
class DirectFFT[T<:Data:Real](config: FFTConfig[T], genMid: DspComplex[T], genTwiddle: DspComplex[T], genOutFull: DspComplex[T])(implicit val p: Parameters) extends Module {
  val io = IO(new DirectFFTIO[T](genMid, genOutFull, config.lanes))

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
    real := Real[T].fromDouble(x(0), genTwiddleReal)
    imag := Real[T].fromDouble(x(1), genTwiddleImag)
    val twiddle = Wire(DspComplex(genTwiddleReal, genTwiddleImag))
    twiddle.real := real
    twiddle.imag := imag
    twiddle
  }))
  val indices_rom = Vec(config.dindices.map(x => UInt(x)))
  // TODO: make this not a multiply
  val start = sync*UInt(config.lanes-1)
  val twiddle = Wire(Vec(config.lanes-1, genTwiddle.cloneType))
  // special case when n = 4, because the pattern breaks down
  if (config.n == 4) {
    twiddle := VecInit((0 until config.lanes-1).map(x => {
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
    twiddle.zipWithIndex.foreach { case (t, x) =>
      t := {
       val true_branch = twiddle_rom(indices_rom(start+UInt(x))(log2Ceil(config.n/4)-1, 0)).divj().asTypeOf(genTwiddle)
       val false_branch = twiddle_rom(indices_rom(start+UInt(x))).asTypeOf(genTwiddle)
       val index = indices_rom(start+UInt(x))
       Mux(index(log2Ceil(config.n/4)),
         true_branch,
         false_branch
       )
      }
    }
  }

  // p-point decimation-in-time direct form FFT with inputs in normal order
  // (outputs bit reversed)
  val stage_outputs = List.fill(log2Up(config.lanes)+1)(List.fill(config.lanes)(Wire(genOutFull)))
  io.in.bits.zip(stage_outputs(0)).foreach { case(in, out) => out := in }

  // indices to the twiddle Vec
  var indices = List(List(0,1),List(0,2))
  for (i <- 0 until log2Up(config.lanes)-2) {
    indices = indices.map(x => x.map(y => y+1))
    val indices_max = indices.foldLeft(0)((b,a) => max(b,a.reduceLeft((d,c) => max(c,d))))
    indices = indices ++ indices.map(x => x.map(y => y+indices_max))
    indices = indices.map(x => 0 +: x)
  }

  // create the FFT hardware
  for (i <- 0 until log2Up(config.lanes)) {
    for (j <- 0 until config.lanes/2) {

      val skip = pow(2,log2Up(config.n/2)-(i+log2Ceil(config.bp))).toInt
      val start = ((j % skip) + floor(j/skip) * skip*2).toInt

      // hook it up
      val outputs           = List(stage_outputs(i+1)(start), stage_outputs(i+1)(start+skip))
      val shr_delay         = config.pipe.drop(log2Ceil(config.bp)).dropRight(log2Up(config.lanes)-i).foldLeft(0)(_+_)
      val shr               = ShiftRegisterMem[DspComplex[T]](twiddle(indices(j)(i)), shr_delay, name = this.name + s"_${i}_${j}_twiddle_sram")
      val butterfly_outputs = Butterfly[T](Seq(stage_outputs(i)(start), stage_outputs(i)(start+skip)), shr)
      outputs.zip(butterfly_outputs).foreach { x =>
        x._1 := ShiftRegisterMem(x._2, config.pipe(i+log2Ceil(config.bp)), name = this.name + s"_${i}_${j}_pipeline_sram")
      }

    }
  }

  // wire up top-level outputs
  // note, truncation happens here!
  io.out.bits := stage_outputs(log2Up(config.lanes))
}

class BiplexFFTIO[T<:Data:Real](lanes: Int, genIn: DspComplex[T], genMid: DspComplex[T])(implicit val p: Parameters) extends Bundle {
  val in = Input(ValidWithSync(Vec(lanes, genIn)))
  val out = Output(ValidWithSync(Vec(lanes, genMid)))
}

/**
 * fast fourier transform - cooley-tukey algorithm, decimation-in-time
 * biplex pipelined version
 * note, this is always a bp-point FFT
 * @tparam T
 */
class BiplexFFT[T<:Data:Real](config: FFTConfig[T], genMid: DspComplex[T], genTwiddle: DspComplex[T])(implicit val p: Parameters) extends Module {
  val io = IO(new BiplexFFTIO[T](config.lanes, config.genIn, genMid))

  // synchronize
  val stage_delays = (0 until log2Up(config.bp)+1).map(x => { if (x == log2Up(config.bp)) config.bp/2 else (config.bp/pow(2,x+1)).toInt })
  val sync = List.fill(log2Up(config.bp)+1)(Wire(UInt(width=log2Up(config.bp).W)))
  val valid_delay = Reg(next=io.in.valid)
  sync(0) := CounterWithReset(true.B, config.bp, io.in.sync, ~valid_delay & io.in.valid)._1
  sync.drop(1).zip(sync).zip(stage_delays).foreach { case ((next, prev), delay) => next := ShiftRegisterWithReset(prev, delay, 0.U) }
  io.out.sync := sync(log2Up(config.bp)) === UInt((config.bp/2-1+config.biplex_pipe)%config.bp)
  io.out.valid := ShiftRegisterWithReset(io.in.valid, stage_delays.reduce(_+_) + config.biplex_pipe, 0.U)

  // wire up twiddles
  val genTwiddleReal = genTwiddle.real
  val genTwiddleImag = genTwiddle.imag
  val twiddle_rom = Vec(config.twiddle.map(x => {
    val real = Wire(genTwiddleReal.cloneType)
    val imag = Wire(genTwiddleImag.cloneType)
    real := Real[T].fromDouble(x(0), genTwiddleReal)
    imag := Real[T].fromDouble(x(1), genTwiddleImag)
    val twiddle = Wire(DspComplex(genTwiddleReal, genTwiddleImag))
    twiddle.real := real
    twiddle.imag := imag
    twiddle
  }))
  val indices_rom = Vec(config.bindices.map(x => UInt(x)))
  val indices = (0 until log2Up(config.bp)).map(x => indices_rom(UInt((pow(2,x)-1).toInt) +& { if (x == 0) UInt(0) else ShiftRegisterMem(sync(x+1), config.pipe.dropRight(log2Up(config.n)-x).reduceRight(_+_), name = this.name + s"_twiddle_sram")(log2Up(config.bp)-2,log2Up(config.bp)-1-x) }))
  val twiddle = Wire(Vec(log2Up(config.bp), genTwiddle))
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
  val stage_outputs = List.fill(log2Up(config.bp)+2)(List.fill(config.lanes)(Wire(genMid)))
  io.in.bits.zip(stage_outputs(0)).foreach { case(in, out) => out := in }

  // create the FFT hardware
  for (i <- 0 until log2Up(config.bp)+1) {
    for (j <- 0 until config.lanes/2) {

      val skip = 1
      val start = j*2

      // hook it up
      // last stage just has one extra permutation, no butterfly
      val mux_out = BarrelShifter(Vec(stage_outputs(i)(start), ShiftRegisterMem(stage_outputs(i)(start+skip), stage_delays(i), name = this.name + s"_${i}_${j}_mux1_sram")), ShiftRegisterMem(sync(i)(log2Up(config.bp)-1 - { if (i == log2Up(config.bp)) 0 else i }), {if (i == 0) 0 else config.pipe.dropRight(log2Up(config.n)-i).reduceRight(_+_)},  name = this.name + s"_${i}_${j}_mux1_sram"))
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

/**
 * IO Bundle for FFT
 * @tparam T
 */
class FFTIO[T<:Data:Real](lanes: Int, genIn: DspComplex[T], genOut: DspComplex[T])(implicit val p: Parameters) extends Bundle {

  val in = Input(ValidWithSync(Vec(lanes, genIn)))
  val out = Output(ValidWithSync(Vec(lanes, genOut)))

  val data_set_end_status = Output(Bool())
  val data_set_end_clear = Input(Bool())
}

/**
 * fast fourier transform - cooley-tukey algorithm, decimation-in-time
 * mixed version
 * note, this is always an n-point FFT
 * @tparam T
 */
class FFT[T<:Data:Real](val config: FFTConfig[T])(implicit val p: Parameters) extends Module {

  require(config.lanes >= 2, "Must have at least 2 parallel inputs")
  require(isPow2(config.lanes), "FFT parallelism must be a power of 2")
  require(config.lanes <= config.n, "An n-point FFT cannot have more than n inputs (p must be less than or equal to n)")

  val io = IO(new FFTIO(config.lanes, config.genIn, config.genOut))

  // calculate direct FFT input bitwidth
  // this is just the input total width + growth of 1 bit per biplex stage
  val genMid: DspComplex[T] = {
    if (config.bp == 1) { config.genIn }
    else {
      val growth = log2Up(config.bp)
      config.genIn.underlyingType() match {
        case "fixed" =>
          config.genIn.real.asInstanceOf[FixedPoint].binaryPoint match {
            case KnownBinaryPoint(binaryPoint) =>
              val totalBits = config.genIn.real.getWidth + growth
              DspComplex(FixedPoint(totalBits.W, binaryPoint.BP), FixedPoint(totalBits.W, binaryPoint.BP)).asInstanceOf[DspComplex[T]]
            case _ => throw new DspException("Error: unknown binary point when calculating FFT bitwdiths")
          }
        case "sint" => {
          val totalBits = config.genIn.real.getWidth + growth
          DspComplex(SInt(totalBits.W), SInt(totalBits.W)).asInstanceOf[DspComplex[T]]
        }
        case _ => throw new DspException("Error: unknown type when calculating FFT bitwidths")
      }
    }
  }

  // calculate twiddle factor bitwidth
  // total input bits
  val genTwiddleBiplex: DspComplex[T] = {
    val growth = log2Up(config.bp)
    config.genIn.asInstanceOf[DspComplex[T]].underlyingType() match {
      case "fixed" =>
        config.genIn.asInstanceOf[DspComplex[T]].real.asInstanceOf[FixedPoint].binaryPoint match {
          case KnownBinaryPoint(binaryPoint) =>
            val totalBits = config.genIn.asInstanceOf[DspComplex[T]].real.getWidth + growth
            DspComplex(FixedPoint(totalBits.W, (totalBits-2).BP), FixedPoint(totalBits.W, (totalBits-2).BP)).asInstanceOf[DspComplex[T]]
          case _ => throw new DspException("Error: unknown binary point when calculating FFT bitwdiths")
        }
      case "sint" => {
        val totalBits = config.genIn.asInstanceOf[DspComplex[T]].real.getWidth + growth
        DspComplex(SInt(totalBits.W), SInt(totalBits.W)).asInstanceOf[DspComplex[T]]
      }
      case _ => throw new DspException("Error: unknown type when calculating FFT bitwidths")
    }
  }

  val genTwiddleDirect: DspComplex[T] = {
    val growth = log2Up(config.n)
    config.genIn.asInstanceOf[DspComplex[T]].underlyingType() match {
      case "fixed" =>
        config.genIn.asInstanceOf[DspComplex[T]].real.asInstanceOf[FixedPoint].binaryPoint match {
          case KnownBinaryPoint(binaryPoint) =>
            val totalBits = config.genIn.asInstanceOf[DspComplex[T]].real.getWidth + growth
            DspComplex(FixedPoint(totalBits.W, (totalBits-2).BP), FixedPoint(totalBits.W, (totalBits-2).BP)).asInstanceOf[DspComplex[T]]
          case _ => throw new DspException("Error: unknown binary point when calculating FFT bitwdiths")
        }
      case "sint" => {
        val totalBits = config.genIn.asInstanceOf[DspComplex[T]].real.getWidth + growth
        DspComplex(SInt(totalBits.W), SInt(totalBits.W)).asInstanceOf[DspComplex[T]]
      }
      case _ => throw new DspException("Error: unknown type when calculating FFT bitwidths")
    }
  }

  // calculate direct FFT output bitwidth
  // this is just the input total width + growth of 1 bit per FFT stage
  val genOutDirect: DspComplex[T] = {
    if (config.bp == 1) { config.genIn }
    else {
      val growth = log2Up(config.n)
      config.genIn.asInstanceOf[DspComplex[T]].underlyingType() match {
        case "fixed" =>
          config.genIn.asInstanceOf[DspComplex[T]].real.asInstanceOf[FixedPoint].binaryPoint match {
            case KnownBinaryPoint(binaryPoint) =>
              val totalBits = config.genIn.asInstanceOf[DspComplex[T]].real.getWidth + growth
              DspComplex(FixedPoint(totalBits.W, binaryPoint.BP), FixedPoint(totalBits.W, binaryPoint.BP)).asInstanceOf[DspComplex[T]]
            case _ => throw new DspException("Error: unknown binary point when calculating FFT bitwdiths")
          }
        case "sint" => {
          val totalBits = config.genIn.asInstanceOf[DspComplex[T]].real.getWidth + growth
          DspComplex(SInt(totalBits.W), SInt(totalBits.W)).asInstanceOf[DspComplex[T]]
        }
        case _ => throw new DspException("Error: unknown type when calculating FFT bitwidths")
      }
    }
  }

  // feed in zeros when invalid
  val in = Wire(ValidWithSync(Vec(config.lanes, config.genIn)))
  when (io.in.valid) {
    in.bits := io.in.bits
  } .otherwise {
    in.bits.foreach { case b => 
      b.real := Real[T].zero
      b.imag := Real[T].zero
    }
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
  val direct = Module(new DirectFFT[T](
    config = config,
    genMid = genMid,
    genTwiddle = genTwiddleDirect,
    genOutFull = genOutDirect
  ))
  io.out <> direct.io.out

  if (config.n != config.lanes) {
    val biplex = Module(new BiplexFFT[T](config, genMid, genTwiddleBiplex))
    direct.io.in := biplex.io.out
    biplex.io.in <> in
  } else {
    direct.io.in <> in
  }
}
