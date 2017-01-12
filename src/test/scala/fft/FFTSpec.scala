// See LICENSE for license details.

package fft

import breeze.math.{Complex}
import breeze.signal.{fourierTr}
import breeze.linalg._
import chisel3._
import chisel3.util._
import chisel3.iotesters._
import firrtl_interpreter.InterpreterOptions
import dsptools.numbers.{DspReal, SIntOrder, SIntRing}
import dsptools.{DspContext, DspTester, Grow}
import org.scalatest.{FlatSpec, Matchers}
import dsptools.numbers.implicits._
import dsptools.numbers.{DspComplex, Real}
import scala.util.Random
import scala.math._
import org.scalatest.Tag

import cde._
import junctions._
import uncore.tilelink._
import uncore.coherence._

import dsptools._

object LocalTest extends Tag("edu.berkeley.tags.LocalTest")

class FFTWrapperTester[T <: Data](c: FFTWrapper[T]) extends DspBlockTester(c) {
  def doublesToBigInt(in: Seq[Double]): BigInt = {
    in.reverse.foldLeft(BigInt(0)) {case (bi, dbl) =>
      val new_bi = BigInt(java.lang.Double.doubleToLongBits(dbl))
      (bi << 64) | new_bi
    }
  }
  def rawStreamIn = Seq(
    Seq(1.0) ++ Seq.fill(15){0.0},
    Seq.fill(16){1.0},
    Seq.fill(16){0.0}
  )
  def streamIn = rawStreamIn.map(doublesToBigInt)

  pauseStream
  val addrMap = testchipip.SCRAddressMap("FFTWrapper").get
  println("Addr Map:\n")
  println(addrMap.map(_.toString).toString)
  println(addrMap("fftControl").toString)

  axiWrite(0, 1)

  println(peek(c.io.out.sync).toString)

  step(10)
  axiWrite(8, 0)
  println(peek(c.io.out.sync).toString)

  playStream

  step(10)
  println(peek(c.io.out.sync).toString)

  println("Input:")
  rawStreamIn.foreach{ x => println(x.toString) }

  println("Output:")
  streamOut.foreach { x => (0 until 16).foreach { idx => {
    val y = (x >> (64 * idx)) & 0xFFFFFFFFFFFFFFFFL
    print(java.lang.Double.longBitsToDouble(y.toLong).toString + " ") }}
    println()
  }
}

class FFTWrapperSpec extends FlatSpec with Matchers {
  behavior of "FFTWrapper"
  val manager = new TesterOptionsManager {
    testerOptions = TesterOptions(backendName = "firrtl", testerSeed = 7L)
    interpreterOptions = InterpreterOptions(setVerbose = false, writeVCD = true)
  }

  it should "work with DspBlockTester" in {
    implicit val p: Parameters = Parameters.root(new DspConfig().toInstance)
    val dut = () => new FFTWrapper[DspReal]()
    chisel3.iotesters.Driver.execute(dut, manager) { c => new FFTWrapperTester(c) } should be (true)
  }

}
