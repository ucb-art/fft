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
import FFTFunctions._

// need separate testbench that hooks up control and data modules
class DirectFFTTestbench[T<:Data:Real](genIn: => DspComplex[T], genOut: => Option[DspComplex[T]] = None, 
  genTwiddle: => Option[DspComplex[T]] = None, val config: FFTConfig = FFTConfig()) extends Module {

  val io = new Bundle {
    val data_in = Vec(config.p, genIn).flip
    val data_out = Vec(config.p, genOut.getOrElse(genIn))
    val sync_in = UInt(log2Up(config.bp)).flip
    val sync_out = UInt(log2Up(config.bp))
  }

  val direct = Module(new DirectFFT(genIn, genOut, genTwiddle, config))
  val direct_control = Module(new DirectFFTControl(genTwiddle.getOrElse(genIn), config))

  direct.io.data_in := io.data_in
  io.data_out := direct.io.data_out
  direct.io.twiddle := direct_control.io.twiddle
  direct_control.io.sync_in := io.sync_in
  io.sync_out := direct_control.io.sync_out
}

class DirectFFTTester[T<:Data:Real](c: DirectFFTTestbench[T], min: Int = -20, max: Int = 20) extends DspTester(c, base=10) {
  import co.theasi.plotly._
  val parallelism = c.config.p
  val fft_size = c.config.n
  val input = Array.fill(parallelism)(Complex(0,0))
  def test_tone(freq: Double): Seq[Complex] = { (0 until parallelism).map(j => {
      val x_t = scala.math.sin(2*math.Pi * freq * j.toDouble / fft_size)
      dspPoke(c.io.data_in(j), Complex(x_t, 0))
      input(j) = Complex(x_t, 0)
    })
    poke(c.io.sync_in, 0)
    step(1)
    var toret = Array.fill(parallelism)(Complex(0,0))
    c.io.data_out.zipWithIndex.foreach { case(port,index) => toret(bit_reverse(index, log2Up(parallelism))) = dspPeek(port).right.get }
    toret
  }
  val results = test_tone(4.5)

  val x = (0 until results.size)
  val y = fourierTr(DenseVector(input)).toArray
  val p = Plot()
    .withScatter(x, results.map(_.abs), ScatterOptions().name("Chisel"))
    .withScatter(x, y.map(_.abs), ScatterOptions().name("Reference"))
  draw(p, "spectrum", writer.FileOptions(overwrite=true))
  
  //for(i <- 0 until 2) {
  //  //val in = Seq.fill(c.config.p)(Complex(nextInt(), nextInt()))
  //  val in = Seq.fill(c.config.p)(Complex(1, 1))
  //  c.io.data_in.zip(in).foreach { case(port, in) => dspPoke(port, in) }
  //  //c.io.data_in.zipWithIndex.foreach { case(port, index) => {if (index == 0) dspPoke(port, 1) else if (index == 6) dspPoke(port, 0.9) else dspPoke(port, 0)} }
  //  poke(c.io.sync_in, i%c.config.bp)
  //  step(1)
  //  c.io.data_out.foreach { port => println(dspPeek(port).toString) }
  //  c.io.twiddle.foreach { port => println(dspPeek(port).toString) }
  //  peek(c.io.sync_out)
  //}
}

class FFTSpec extends FlatSpec with Matchers {

  // DirectFFT
  behavior of "DirectFFT"
  it should "Fourier transform the input, fast" in {
    def getReal(): DspReal = new DspReal
    chisel3.iotesters.Driver(() => new DirectFFTTestbench(genIn = DspComplex(getReal, getReal), config = new FFTConfig(n = 32, p = 32))) {
    //chisel3.iotesters.Driver(() => new DirectFFTTestbench(genIn = DspComplex(FixedPoint(width=16, binaryPoint=8), FixedPoint(width=16, binaryPoint=8)), config = new FFTConfig(n = 32, p = 32))) {
      c => new DirectFFTTester(c)
    } should be (true)
  }


}

