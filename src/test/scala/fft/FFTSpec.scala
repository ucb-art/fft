// See LICENSE for license details.

package fft

import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import breeze.math.{Complex}
import breeze.signal.{fourierTr}
import breeze.linalg._
import chisel3._
import chisel3.experimental._
import chisel3.util._
import chisel3.iotesters._
import firrtl_interpreter.InterpreterOptions
import dsptools.numbers.{DspReal, SIntOrder, SIntRing}
import dsptools.{DspContext, DspTester, Grow}
import org.scalatest.{FlatSpec, Matchers}
import chisel3.iotesters.{PeekPokeTester, TesterOptionsManager}

// comment when using FixedPoint, uncomment for DspReal
// import dsptools.numbers.implicits._

import dsptools.numbers.{DspComplex, Real}
import scala.util.Random
import scala.math.{pow, abs, round}
import org.scalatest.Tag
import dspjunctions._
import dspblocks._

import craft._
import dsptools._
import freechips.rocketchip.config.Parameters

object LocalTest extends Tag("edu.berkeley.tags.LocalTest")

class FFTTester[T<:Data](val c: FFT[T]) extends DspTester(c) {
  // this is a hack to use FFTTester outside of the normal driver methods
  override def finish = true
  def actualFinish = super.finish
}

object spectrumTester {
  def testSignal[T<:Data](dut: FFTTester[T], signal: Seq[Complex]): Seq[Complex] = {
    
    // reset
    dut.reset(5)

    // get some useful variables
    val config = dut.c.config
    val io = dut.c.io
    val groupedSignal: Seq[Seq[Complex]] = signal.grouped(config.lanes).toSeq

    // checks
    require(signal.size >= config.n, "Cannot test a signal shorter than the FFT size")
    if (signal.size > config.n) { println("Warning: test signal longer than the FFT size, will only use first n points") }

    // synchronize to the next input
    //dut.poke(io.in.sync, 1)
    //dut.step(1)
    dut.poke(io.in.sync, 0)
    dut.poke(io.in.valid, 0)
    dut.step(1)
    dut.poke(io.in.valid, 1)

    // get delay
    val stage_delays = (0 until log2Ceil(config.bp)+1).map(x => { if (x == log2Ceil(config.bp)) config.bp/2 else (config.bp/pow(2,x+1)).toInt })
    val test_length = config.bp + config.pipelineDepth + stage_delays.reduce(_+_)

    // create return val
    val retval = new scala.collection.mutable.Queue[Complex]()
    var synced = false
    var last_valid = false

    // poke input signal
    for (i <- 0 until test_length) {
      // repeat end of signal
      groupedSignal(min(i, groupedSignal.size-1)).zip(io.in.bits).foreach { case(sig, port) => dut.poke(port, sig) }
      val valid = dut.peek(io.out.valid)
      if (!synced && (valid & !last_valid)) { synced = true; println(s"synced on cycle $i") }
      if (synced || config.bp == 1) { io.out.bits.foreach(x => retval += dut.peek(x)) }
      if (!synced && dut.peek(io.out.sync) && config.bp != 1) { synced = true; println(s"synced on cycle $i") }
      last_valid = valid
      dut.step(1)
    }

    // return unscrambled output
    require(!retval.isEmpty, "Output queue is empty")
    require(retval.size == config.n, s"Output queue has the wrong number of values, got ${retval.size}, expected ${config.n}")
    unscramble(retval.toSeq, config.lanes)
  }

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

  // unscramble fft output
  def unscramble(in: Seq[Complex], p: Int): Seq[Complex] = {
    val n = in.size
    val bp = n/p
    val res = Array.fill(n)(Complex(0.0,0.0))
    in.grouped(p).zipWithIndex.foreach { case (set, sindex) => 
      set.zipWithIndex.foreach { case (bin, bindex) => 
        if (bp > 1) {
          val p1 = if (sindex/(bp/2) >= 1) 1 else 0
          val new_index = bit_reverse((sindex % (bp/2)) * 2 + p1, log2Ceil(bp)) + bit_reverse(bindex, log2Ceil(n))
          res(new_index) = bin
        } else {
          val new_index = bit_reverse(bindex, log2Ceil(n))
          res(new_index) = bin
        }
      }
    }
    res
  }

  def setupTester[T <: Data](c: () => FFT[T], verbose: Boolean = false): FFTTester[T] = {
    var tester: FFTTester[T] = null
    val manager = new TesterOptionsManager {
      testerOptions = TesterOptions(backendName = "firrtl", testerSeed = 7L)
      interpreterOptions = InterpreterOptions(setVerbose = false, writeVCD = verbose, maxExecutionDepth = 2000)
    }
    chisel3.iotesters.Driver.execute(c, manager) (c => {
      val t = new FFTTester(c)
      tester = t
      t
    })

    tester
  }

  def teardownTester[T <: Data](tester: FFTTester[T]): Unit = {
    tester.actualFinish
  }

  def getTone(numSamples: Int, f: Double): Seq[Complex] = {
    // uncomment to scale input tone 
    //(0 until numSamples).map(i => pow(2, -(numSamples+1))*Complex(math.cos(2 * math.Pi * f * i), math.sin(2 * math.Pi * f * i)))
    (0 until numSamples).map(i => Complex(math.cos(2 * math.Pi * f * i), math.sin(2 * math.Pi * f * i)))
  }

  def apply[T<:Data](c: () => FFT[T], config: FFTConfig[T], verbose: Boolean = false): Unit = {

    // get some parameters
    val fftSize = config.n

    // bin-by-bin testing
    val m = 16 // at most 16 bins
    (0 until min(fftSize, m)).foreach{ bin =>
      val b = if (fftSize > m) fftSize/m*bin else bin
      val tester = setupTester(c, verbose) 
      val tone = getTone(fftSize, b.toDouble/fftSize)
      val testResult = testSignal(tester, tone)
      val expectedResult = fourierTr(DenseVector(tone.toArray)).toArray
      if (verbose) {
        println("Tone = ")
        println(tone.toArray.deep.mkString("\n"))
        println("Chisel output = ")
        println(testResult.toArray.deep.mkString("\n"))
        println("Expected output = ")
        println(expectedResult.toArray.deep.mkString("\n"))
      }
      compareOutputComplex(testResult, expectedResult, 1e-2)
      teardownTester(tester)
    }

    // random testing
    (0 until 4).foreach{ x =>
      val tester = setupTester(c, verbose) 
      val tone = (0 until fftSize).map(x => Complex(Random.nextDouble(), Random.nextDouble()))
      val testResult = testSignal(tester, tone)
      val expectedResult = fourierTr(DenseVector(tone.toArray)).toArray
      if (verbose) {
        println("Tone = ")
        println(tone.toArray.deep.mkString("\n"))
        println("Chisel output = ")
        println(testResult.toArray.deep.mkString("\n"))
        println("Expected output = ")
        println(expectedResult.toArray.deep.mkString("\n"))
      }
      compareOutputComplex(testResult, expectedResult, 5e-2)
      teardownTester(tester)
    }
  }

  // compares chisel and reference outputs, errors if they differ by more than epsilon
  def compareOutputComplex(chisel: Seq[Complex], ref: Seq[Complex], epsilon: Double = 1e-12): Unit = {
    chisel.zip(ref).zipWithIndex.foreach { case((c, r), index) =>
      if (c.real != r.real) {
        val err = abs(c.real-r.real)/(abs(r.real)+epsilon)
        assert(err < epsilon || abs(r.real) < epsilon, s"Error: mismatch in real value on output $index of ${err*100}%\n\tReference: ${r.real}\n\tChisel:    ${c.real}")
      }
      if (c.imag != r.imag) {
        val err = abs(c.imag-r.imag)/(abs(r.imag)+epsilon)
        assert(err < epsilon || abs(r.imag) < epsilon, s"Error: mismatch in imag value on output $index of ${err*100}%\n\tReference: ${r.imag}\n\tChisel:    ${c.imag}")
      }
    }
  }
}

class FFTSpec extends FlatSpec with Matchers {
  behavior of "FFT"

  it should "Fourier transform" in {

    val tests = Seq(
      // (FFT points, lanes, total width, fractional bits, pipeline depth)
      Seq(8,   8,  35, 19, 0),
      Seq(128, 16, 27, 16, 17),
      Seq(32, 4, 27, 16, 0)
    )

    for (test <- tests) {
      val totalWidth = test(2)
      val fractionalBits = test(3)
      implicit object FixedTypeclass extends dsptools.numbers.FixedPointReal {
        override def fromDouble(x: Double): FixedPoint = {
          FixedPoint.fromDouble(x, width = totalWidth.W, binaryPoint = fractionalBits.BP)
        }
      }
      val config = FFTConfig(
        genIn = DspComplex(FixedPoint(totalWidth.W, fractionalBits.BP), FixedPoint(totalWidth.W, fractionalBits.BP)),
        genOut = DspComplex(FixedPoint(totalWidth.W, fractionalBits.BP), FixedPoint(totalWidth.W, fractionalBits.BP)),
        n = test(0),
        lanes = test(1),
        pipelineDepth = test(4),
        quadrature = false,
      )
      implicit val p: Parameters = null
      println(s"Testing ${test(0)}-point FFT with ${test(1)} lanes, ${test(2)} total bits, ${test(3)} fractional bits, and ${test(4)} pipeline depth")
      spectrumTester(() => new FFT(config), config, false)
    }
  }
}

