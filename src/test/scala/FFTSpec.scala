// See LICENSE for license details.

package fft

import breeze.math.{Complex}
import breeze.signal.{fourierTr}
import breeze.linalg._
import chisel3._
import chisel3.util._
import chisel3.iotesters.PeekPokeTester
import dsptools.numbers.{DspReal, SIntOrder, SIntRing}
import dsptools.{DspContext, DspTester, Grow}
import org.scalatest.{FlatSpec, Matchers}
import dsptools.numbers.implicits._
import dsptools.numbers.{DspComplex, Real}
import scala.util.Random
import scala.math._
import org.scalatest.Tag

object LocalTest extends Tag("edu.berkeley.tags.LocalTest")

class DirectFFTTester[T<:Data:Real](c: DirectFFT[T], min: Int = -20, max: Int = 20) extends DspTester(c, base=10) {

  // bit reverse a value
  def bit_reverse(in: Int, width: Int): Int = {
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

  import co.theasi.plotly._
  val parallelism = c.config.p
  val fft_size = c.config.n
  val input = Array.fill(parallelism)(Complex(0,0))
  def test_tone(freq: Double): Seq[Complex] = { (0 until parallelism).map(j => {
      val x_t = scala.math.sin(2*math.Pi * freq * j.toDouble / fft_size)
      dspPoke(c.io.in.bits(j), Complex(x_t, 0))
      input(j) = Complex(x_t, 0)
    })
    poke(c.io.in.sync, 0)
    step(1)
    var toret = Array.fill(parallelism)(Complex(0,0))
    c.io.out.bits.zipWithIndex.foreach { case(port,index) => toret(bit_reverse(index, log2Up(parallelism))) = dspPeek(port).right.get }
    toret
  }
  val results = test_tone(4.125)

  val x = (0 until results.size)
  val y = fourierTr(DenseVector(input)).toArray
  val p = Plot()
    .withScatter(x, results.map(_.abs), ScatterOptions().name("Chisel"))
    .withScatter(x, y.map(_.abs), ScatterOptions().name("Reference"))
  draw(p, "spectrum", writer.FileOptions(overwrite=true))
  
  //for(i <- 0 until 2) {
  //  //val in = Seq.fill(c.config.p)(Complex(nextInt(), nextInt()))
  //  val in = Seq.fill(c.config.p)(Complex(1, 1))
  //  c.io.in.bits.zip(in).foreach { case(port, in) => dspPoke(port, in) }
  //  //c.io.in.bits.zipWithIndex.foreach { case(port, index) => {if (index == 0) dspPoke(port, 1) else if (index == 6) dspPoke(port, 0.9) else dspPoke(port, 0)} }
  //  poke(c.io.in.sync, i%c.config.bp)
  //  step(1)
  //  c.io.out.bits.foreach { port => println(dspPeek(port).toString) }
  //  c.io.twiddle.foreach { port => println(dspPeek(port).toString) }
  //  peek(c.io.out.sync)
  //}
}

//class BiplexFFTTester[T<:Data:Real](c: BiplexFFT[T], min: Int = -20, max: Int = 20) extends DspTester(c, base=10) {
//
//  // bit reverse a value
//  def bit_reverse(in: Int, width: Int): Int = {
//    var test = in
//    var out = 0
//    for (i <- 0 until width) {
//      if (test / pow(2, width-i-1) >= 1) {
//        out += pow(2,i).toInt
//        test -= pow(2,width-i-1).toInt
//      }
//    }
//    out
//  }
//
//  import co.theasi.plotly._
//  val parallelism = c.config.p
//  val fft_size = c.config.n
//  val bp = c.config.bp
//  val input = Array.fill(fft_size*2)(Complex(0,0))
//  def test_tone(freq: Double): Seq[Complex] = { (0 until fft_size*2).map(j => {
//      //val x_t = scala.math.sin(2*math.Pi * freq * j.toDouble / fft_size)
//      val x_t = j
//      dspPoke(c.io.in.bits(0), Complex(x_t, 0))
//      dspPoke(c.io.in.bits(1), Complex(x_t, 0))
//      input(j) = Complex(x_t, 0)
//      poke(c.io.in.sync, j%bp==0)
//      step(1)
//      c.io.out.bits.foreach { x => println(dspPeek(x).toString) }
//      //c.sync.foreach { x => println(peek(x).toString) }
//    })
//    var toret = Array.fill(fft_size*2)(Complex(0,0))
//    toret
//  }
//  val results = test_tone(4.125)
//
//  //val x = (0 until results.size)
//  //val y = fourierTr(DenseVector(input)).toArray
//  //val p = Plot()
//  //  .withScatter(x, results.map(_.abs), ScatterOptions().name("Chisel"))
//  //  .withScatter(x, y.map(_.abs), ScatterOptions().name("Reference"))
//  //draw(p, "spectrum", writer.FileOptions(overwrite=true))
//}

class FFTSpec extends FlatSpec with Matchers {

  // DirectFFT
  behavior of "DirectFFT"
  it should "Fourier transform the input, fastly" taggedAs(LocalTest) in {
    def getReal(): DspReal = new DspReal
    chisel3.iotesters.Driver(() => new DirectFFT(genIn = DspComplex(getReal, getReal), config = new FFTConfig(n = 16, p = 16))) {
    //chisel3.iotesters.Driver(() => new DirectFFTTestbench(genIn = DspComplex(FixedPoint(width=16, binaryPoint=8), FixedPoint(width=16, binaryPoint=8)), config = new FFTConfig(n = 32, p = 32))) {
      c => new DirectFFTTester(c)
    } should be (true)
  }

  //behavior of "BiplexFFT"
  //it should "Fourier transform the input, fastly" in {
  //  def getReal(): DspReal = new DspReal
  //  chisel3.iotesters.Driver(() => new BiplexFFT(genIn = DspComplex(getReal, getReal), config = new FFTConfig(n = 4, p = 2))) {
  //    c => new BiplexFFTTester(c)
  //  } should be (true)
  //}

}

//object FFTSpec {
//  def getReal(): DspReal = new DspReal
//  def main(args: Array[String]): Unit = {
//    dsptools.Driver.executeFirrtlRepl(
//      () => new BiplexFFT(genIn = DspComplex(getReal, getReal), config = new FFTConfig(n = 4, p = 2))
//    )
//  }
//}
