// See LICENSE for license details.

package fft

import diplomacy.{LazyModule, LazyModuleImp}
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

// comment when using FixedPoint, uncomment for DspReal
// import dsptools.numbers.implicits._

import dsptools.numbers.{DspComplex, Real}
import scala.util.Random
import scala.math._
import org.scalatest.Tag
import dspjunctions._
import dspblocks._

import cde._
import junctions._
import uncore.tilelink._
import uncore.coherence._

import craft._
import dsptools._

object LocalTest extends Tag("edu.berkeley.tags.LocalTest")

class FFTTester[T <: Data](c: FFTBlock[T])(implicit p: Parameters) extends DspBlockTester(c) {

  // grab some parameters and configuration stuff
  def config = p(FFTKey(p(DspBlockId)))
  def gk = p(GenKey(p(DspBlockId)))
  val stage_delays = (0 until log2Up(config.bp)+1).map(x => { if (x == log2Up(config.bp)) config.bp/2 else (config.bp/pow(2,x+1)).toInt })
  val test_length = config.bp + config.pipelineDepth + stage_delays.reduce(_+_)

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

  // unscramble, expects a single array of size n
  def unscramble(in: Seq[Complex]): Seq[Complex] = {
    val p = gk.lanesIn
    val n = config.n
    val bp = n/p
    assert(n == in.size, s"Error: input $in has the wrong length, expected $n but got ${in.size}")

    val res = Array.fill(n)(Complex(0.0,0.0))
    in.grouped(p).zipWithIndex.foreach { case (set, sindex) => 
      set.zipWithIndex.foreach { case (bin, bindex) => 
        if (bp > 1) {
          val p1 = if (sindex/(bp/2) >= 1) 1 else 0
          val new_index = bit_reverse((sindex % (bp/2)) * 2 + p1, log2Up(bp)) + bit_reverse(bindex, log2Up(n))
          res(new_index) = bin
        } else {
          val new_index = bit_reverse(bindex, log2Up(n))
          res(new_index) = bin
        }
      }
    }
    res
  }

  // random input data
  def input = Seq.fill(test_length * config.n)(Seq.fill(gk.lanesIn)(Complex(Random.nextDouble*2+1, Random.nextDouble*2+1)))
  //val input = Seq.fill(test_length)(Seq.fill(gk.lanesIn)(Complex(-1.4, -2.22)))

  def streamIn = {
    println(s"gk is $gk")
    gk.genIn[DspComplex[FixedPoint]] match {
    case gen: DspComplex[FixedPoint] => {
      println(s"gen is $gen")
      // println(s"underlying type is ${gen.underlyingType()}")
      println(s"input is $input")
      val a = packInputStream(input, gen)
      println(s"packed input is $a")
      a
    }
    case _ => throw new Exception(s"genIn needs to be DspComplex, not $gk.genIn")
  }
  }

  // calculate expected output
  val expected_output = fourierTr(DenseVector(input.take(config.bp).toArray.flatten)).toArray

  // reset 5 cycles
  reset(5)

  // run test
  playStream
  step(test_length * config.n)
  val output = unscramble(unpackOutputStream(gk.genOut, gk.lanesOut))

  // print out data sets for visual confirmation
  println("Input")
  println(input.toArray.flatten.deep.mkString("\n"))
  println("Chisel Output")
  println(output.toArray.deep.mkString("\n"))
  println("Reference Output")
  println(expected_output.toArray.deep.mkString("\n"))

  // compare results, only works for DC impulse spectra right now
  // TODO: unscramble, handle multi-cycle data sets
  compareOutputComplex(output, expected_output, 5e-2)
}

class FFTSpec extends FlatSpec with Matchers {
  val totalWidth = 32
  val fractionalBits = 16
  behavior of "FFT"
  val manager = new TesterOptionsManager {
    testerOptions = TesterOptions(backendName = "firrtl", testerSeed = 7L)
    interpreterOptions = InterpreterOptions(setVerbose = false, writeVCD = true)
  }

  it should "work with DspBlockTester" in {
    implicit object FixedTypeclass extends dsptools.numbers.FixedPointReal {
      override def fromDouble(x: Double): FixedPoint = {
        FixedPoint.fromDouble(x, binaryPoint = fractionalBits)
      }
    }
    implicit val p: Parameters = Parameters.root(
      FFTConfigBuilder(
        "fft",
        FFTConfig(),
        {() => FixedPoint(totalWidth.W, fractionalBits.BP)}).toInstance)
    val dut = () => LazyModule(new LazyFFTBlock[FixedPoint]).module
    chisel3.iotesters.Driver.execute(dut, manager) { c => new FFTTester(c) } should be (true)
  }

}

