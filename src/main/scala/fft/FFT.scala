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
  val valid_delay = RegNext(io.in.valid)
  val sync = CounterWithReset(true.B, config.bp, io.in.sync, ~valid_delay & io.in.valid)._1
  io.out.sync := ShiftRegisterWithReset(io.in.valid && sync === (config.bp-1).U, config.direct_pipe, 0.U) // should valid keep sync from propagating?
  io.out.valid := ShiftRegisterWithReset(io.in.valid, config.direct_pipe, 0.U)

  // wire up twiddles
  val genTwiddleReal = genTwiddle.real
  val genTwiddleImag = genTwiddle.imag
  // cleaner version possible?
  val twiddle_rom = config.dtwiddles.map(i => {VecInit(i.map(j => {
    val real = Wire(genTwiddleReal.cloneType)
    val imag = Wire(genTwiddleImag.cloneType)
    real := Real[T].fromDouble(j(0), genTwiddleReal)
    imag := Real[T].fromDouble(j(1), genTwiddleImag)
    val twiddle = Wire(DspComplex(genTwiddleReal, genTwiddleImag))
    twiddle.real := real
    twiddle.imag := imag
    twiddle
  }))})

  // p-point decimation-in-time direct form FFT with inputs in normal order
  // (outputs bit reversed)
  if (config.quadrature) {
    val stage_outputs_i = List.fill(log2Ceil(config.lanes_new)+1)(List.fill(config.lanes_new)(Wire(genOutFull)))
    val stage_outputs_q = List.fill(log2Ceil(config.lanes_new)+1)(List.fill(config.lanes_new)(Wire(genOutFull)))

    stage_outputs_i(0).zip(io.in.bits.take(config.lanes_new)).foreach { case(out, in) => out := in }
    stage_outputs_q(0).zip(io.in.bits.drop(config.lanes_new)).foreach { case(out, in) => out := in }

    // create the FFT hardware
    for (i <- 0 until log2Ceil(config.lanes_new)) {
      for (j <- 0 until config.lanes_new/2) {
        val skip = pow(2,log2Ceil(config.n/2)-(i+log2Ceil(config.bp))).toInt
        val start = ((j % skip) + floor(j/skip) * skip*2).toInt

        // hook it up
        val outputs_i           = List(stage_outputs_i(i+1)(start), stage_outputs_i(i+1)(start+skip))
        val butterfly_outputs_i = Butterfly[T](Seq(stage_outputs_i(i)(start), stage_outputs_i(i)(start+skip)), twiddle_rom(config.tdindices(j)(i))(sync))
        outputs_i.zip(butterfly_outputs_i).foreach { x =>
          x._1 := ShiftRegisterMem(x._2, config.pipe(i+log2Ceil(config.bp)), name = this.name + s"_${i}_${j}_i_pipeline_sram")
        }

        // hook it up
        val outputs_q           = List(stage_outputs_q(i+1)(start), stage_outputs_q(i+1)(start+skip))
        val butterfly_outputs_q = Butterfly[T](Seq(stage_outputs_q(i)(start), stage_outputs_q(i)(start+skip)), twiddle_rom(config.tdindices(j)(i))(sync))
        outputs_q.zip(butterfly_outputs_q).foreach { x =>
          x._1 := ShiftRegisterMem(x._2, config.pipe(i+log2Ceil(config.bp)), name = this.name + s"_${i}_${j}_q_pipeline_sram")
        }

      }
    }

    // wire up top-level outputs
    // note, truncation happens here!
    stage_outputs_i(log2Ceil(config.lanes_new)).zip(io.out.bits.take(config.lanes_new)).foreach { case (in, out) => out := in }
    stage_outputs_q(log2Ceil(config.lanes_new)).zip(io.out.bits.drop(config.lanes_new)).foreach { case (in, out) => out := in }
  } else {
    val stage_outputs = List.fill(log2Ceil(config.lanes)+1)(List.fill(config.lanes)(Wire(genOutFull)))
    io.in.bits.zip(stage_outputs(0)).foreach { case(in, out) => out := in }

    // create the FFT hardware
    for (i <- 0 until log2Ceil(config.lanes)) {
      for (j <- 0 until config.lanes/2) {

        val skip = pow(2,log2Ceil(config.n/2)-(i+log2Ceil(config.bp))).toInt
        val start = ((j % skip) + floor(j/skip) * skip*2).toInt

        // hook it up
        val outputs           = List(stage_outputs(i+1)(start), stage_outputs(i+1)(start+skip))
        val butterfly_outputs = Butterfly[T](Seq(stage_outputs(i)(start), stage_outputs(i)(start+skip)), twiddle_rom(config.tdindices(j)(i))(sync))
        outputs.zip(butterfly_outputs).foreach { x =>
          x._1 := ShiftRegisterMem(x._2, config.pipe(i+log2Ceil(config.bp)), name = this.name + s"_${i}_${j}_pipeline_sram")
        }

      }
    }
    // wire up top-level outputs
    // note, truncation happens here!
    io.out.bits := stage_outputs(log2Ceil(config.lanes))
  }

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
  val stage_delays = (0 until log2Ceil(config.bp)+1).map(x => { if (x == log2Ceil(config.bp)) config.bp/2 else (config.bp/pow(2,x+1)).toInt })
  val sync = List.fill(log2Ceil(config.bp)+1)(Wire(UInt(width=log2Ceil(config.bp).W)))
  val valid_delay = Reg(next=io.in.valid)
  // [stevo]: reset all internal syncs to correct value immediately upon syncronization signal, io.in.sync or io.in.valid rising edge
  def sync_reset_val(index: Int): Int = {
    if (index == 0) { 0 }
    else { (-stage_delays.take(index).reduce(_+_)%config.bp)+config.bp }
  }
  sync.zipWithIndex.foreach { case (s, i) =>
    s := CounterWithReset(true.B, config.bp, io.in.sync, ~valid_delay & io.in.valid)/*(sync_reset_val(i).U, sync_reset_val(i).U)*/._1
  }
  io.out.sync := sync(log2Ceil(config.bp)) === ((config.bp/2-1+config.biplex_pipe)%config.bp).U
  io.out.valid := ShiftRegisterWithReset(io.in.valid, stage_delays.reduce(_+_) + config.biplex_pipe, 0.U)

  // wire up twiddles
  // fortunately each column in the biplex has the same twiddle vavlue, so they're truely parallel FFTs
  val genTwiddleReal = genTwiddle.real
  val genTwiddleImag = genTwiddle.imag
  // cleaner version possible?
  val twiddle_rom = config.btwiddles.map(i => {Vec(i.map(j => {
    val real = Wire(genTwiddleReal.cloneType)
    val imag = Wire(genTwiddleImag.cloneType)
    real := Real[T].fromDouble(j(0), genTwiddleReal)
    imag := Real[T].fromDouble(j(1), genTwiddleImag)
    val twiddle = Wire(DspComplex(genTwiddleReal, genTwiddleImag))
    twiddle.real := real
    twiddle.imag := imag
    twiddle
  }))})

  // bp-point decimation-in-time biplex pipelined FFT with outputs in bit-reversed order
  // up-scale to genMid immediately for simplicity
  if (config.quadrature) {
    val stage_outputs_i = List.fill(log2Ceil(config.bp)+2)(List.fill(config.lanes_new)(Wire(genMid)))
    val stage_outputs_q = List.fill(log2Ceil(config.bp)+2)(List.fill(config.lanes_new)(Wire(genMid)))

    stage_outputs_i(0).zip(io.in.bits.take(config.lanes_new)).foreach { case(out, in) => out := in }
    stage_outputs_q(0).zip(io.in.bits.drop(config.lanes_new)).foreach { case(out, in) => out := in }

    // create the FFT hardware
    for (i <- 0 until log2Ceil(config.bp)+1) {
      for (j <- 0 until config.lanes_new/2) {

        val skip = 1
        val start = j*2

        // hook it up
        // last stage just has one extra permutation, no butterfly
        val mux_out_i = BarrelShifter(Vec(stage_outputs_i(i)(start), ShiftRegisterMem(stage_outputs_i(i)(start+skip), stage_delays(i), name = this.name + s"_${i}_${j}_mux0i_sram")), ShiftRegisterMem(sync(i)(log2Ceil(config.bp)-1 - { if (i == log2Ceil(config.bp)) 0 else i }), {if (i == 0) 0 else config.pipe.dropRight(log2Ceil(config.n)-i).reduceRight(_+_)},  name = this.name + s"_${i}_${j}_mux1i_sram"))
        val mux_out_q = BarrelShifter(Vec(stage_outputs_q(i)(start), ShiftRegisterMem(stage_outputs_q(i)(start+skip), stage_delays(i), name = this.name + s"_${i}_${j}_mux0q_sram")), ShiftRegisterMem(sync(i)(log2Ceil(config.bp)-1 - { if (i == log2Ceil(config.bp)) 0 else i }), {if (i == 0) 0 else config.pipe.dropRight(log2Ceil(config.n)-i).reduceRight(_+_)},  name = this.name + s"_${i}_${j}_mux1q_sram"))
        if (i == log2Ceil(config.bp)) {
          Seq(stage_outputs_i(i+1)(start), stage_outputs_i(i+1)(start+skip)).zip(Seq(ShiftRegisterMem(mux_out_i(0), stage_delays(i), name = this.name + s"_${i}_${j}_lasti_sram" ), mux_out_i(1))).foreach { x => x._1 := x._2 }
          Seq(stage_outputs_q(i+1)(start), stage_outputs_q(i+1)(start+skip)).zip(Seq(ShiftRegisterMem(mux_out_q(0), stage_delays(i), name = this.name + s"_${i}_${j}_lastq_sram" ), mux_out_q(1))).foreach { x => x._1 := x._2 }
        } else {
          Seq(stage_outputs_i(i+1)(start), stage_outputs_i(i+1)(start+skip)).zip(Butterfly(Seq(ShiftRegisterMem(mux_out_i(0), stage_delays(i), name = this.name + s"_${i}_${j}_pipeline0i_sram"), mux_out_i(1)), twiddle_rom(i)(sync(i+1)))).foreach { x => x._1 := ShiftRegisterMem(x._2, config.pipe(i), name = this.name + s"_${i}_${j}_pipeline1i_sram") }
          Seq(stage_outputs_q(i+1)(start), stage_outputs_q(i+1)(start+skip)).zip(Butterfly(Seq(ShiftRegisterMem(mux_out_q(0), stage_delays(i), name = this.name + s"_${i}_${j}_pipeline0q_sram"), mux_out_q(1)), twiddle_rom(i)(sync(i+1)))).foreach { x => x._1 := ShiftRegisterMem(x._2, config.pipe(i), name = this.name + s"_${i}_${j}_pipeline1q_sram") }
        }

      }
    }

    // wire up top-level outputs
    stage_outputs_i(log2Ceil(config.bp)+1).zip(io.out.bits.take(config.lanes_new)).foreach { case (in, out) => out := in }
    stage_outputs_q(log2Ceil(config.bp)+1).zip(io.out.bits.drop(config.lanes_new)).foreach { case (in, out) => out := in }
  } else {
    val stage_outputs = List.fill(log2Ceil(config.bp)+2)(List.fill(config.lanes)(Wire(genMid)))
    io.in.bits.zip(stage_outputs(0)).foreach { case(in, out) => out := in }

    // create the FFT hardware
    for (i <- 0 until log2Ceil(config.bp)+1) {
      for (j <- 0 until config.lanes/2) {

        val skip = 1
        val start = j*2

        // hook it up
        // last stage just has one extra permutation, no butterfly
        val mux_out = BarrelShifter(VecInit(stage_outputs(i)(start), ShiftRegisterMem(stage_outputs(i)(start+skip), stage_delays(i), name = this.name + s"_${i}_${j}_mux0_sram")), ShiftRegisterMem(sync(i)(log2Ceil(config.bp)-1 - { if (i == log2Ceil(config.bp)) 0 else i }), {if (i == 0) 0 else config.pipe.dropRight(log2Ceil(config.n)-i).reduceRight(_+_)}, name = this.name + s"_${i}_${j}_mux1_sram"))
        if (i == log2Ceil(config.bp)) {
          Seq(stage_outputs(i+1)(start), stage_outputs(i+1)(start+skip)).zip(Seq(ShiftRegisterMem(mux_out(0), stage_delays(i), name = this.name + s"_${i}_${j}_last_sram" ), mux_out(1))).foreach { x => x._1 := x._2 }
        } else {
          Seq(stage_outputs(i+1)(start), stage_outputs(i+1)(start+skip)).zip(Butterfly(Seq(ShiftRegisterMem(mux_out(0), stage_delays(i), name = this.name + s"_${i}_${j}_pipeline0_sram"), mux_out(1)), twiddle_rom(i)(sync(i+1)))).foreach { x => x._1 := ShiftRegisterMem(x._2, config.pipe(i), name = this.name + s"_${i}_${j}_pipeline1_sram") }
        }

      }
    }
    // wire up top-level outputs
    io.out.bits := stage_outputs(log2Ceil(config.bp)+1)
  }

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

  require(config.lanes_new >= 2, "Must have at least 2 parallel inputs")
  require(isPow2(config.lanes_new), "FFT parallelism must be a power of 2")
  require(config.lanes_new <= config.n, "An n-point FFT cannot have more than n inputs (p must be less than or equal to n)")

  val io = IO(new FFTIO(config.lanes, config.genIn, config.genOut))

  // calculate direct FFT input bitwidth
  // truncating every other stage is the same as growing one bit every other stage, so we can
  // grow by log2(bp)/2 and be okay I think
  val genMid: DspComplex[T] = {
    if (config.bp == 1) { config.genIn }
    else {
      val growth = log2Ceil(config.bp)/2
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
    val growth = log2Ceil(config.bp)/2
    config.genIn.underlyingType() match {
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
    val growth = math.ceil(log2Ceil(config.n).toDouble/2.0).toInt
    config.genIn.underlyingType() match {
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
      val growth = math.ceil(log2Ceil(config.n).toDouble/2.0).toInt
      config.genIn.underlyingType() match {
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
  val valid_delay = RegNext(io.out.valid)
  val dses = RegInit(false.B)
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

  if (config.n != config.lanes_new) {
    val biplex = Module(new BiplexFFT[T](config, genMid, genTwiddleBiplex))
    direct.io.in := biplex.io.out
    biplex.io.in <> in
  } else {
    direct.io.in <> in
  }
}
