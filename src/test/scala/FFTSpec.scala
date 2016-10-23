// See LICENSE for license details.

package fft

import breeze.math.Complex
import chisel3._
import chisel3.util._
import chisel3.iotesters.PeekPokeTester
import dsptools.numbers.{DspReal, SIntOrder, SIntRing}
import dsptools.{DspContext, DspTester, Grow}
import org.scalatest.{FlatSpec, Matchers}
import dsptools.numbers.implicits._
import dsptools.numbers.{DspComplex, Real}
import scala.util.Random

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
  require(max > min)
  def nextInt(): Int = Random.nextInt(max - min) - min
  
  for(i <- 0 until 1) {
    //val in = Seq.fill(c.config.p)(Complex(nextInt(), nextInt()))
    val in = Seq.fill(c.config.p)(Complex(0, 0))
    c.io.data_in.zip(in).foreach { case(port, in) => dspPoke(port, in) }
    //c.io.data_in.zipWithIndex.foreach { case(port, index) => {if (index == 0) dspPoke(port, 1) else if (index == 6) dspPoke(port, 0.9) else dspPoke(port, 0)} }
    poke(c.io.sync_in, i%c.config.bp)
    step(1)
    c.io.data_out.foreach { port => println(dspPeek(port).toString) }
    c.direct_control.io.twiddle.foreach { port => println(dspPeek(port).toString) }
    peek(c.io.sync_out)
  }
}

class FFTSpec extends FlatSpec with Matchers {

  // DirectFFT
  behavior of "DirectFFT"
  it should "Fourier transform the input, fast" in {
    def getReal(): DspReal = new DspReal
    chisel3.iotesters.Driver(() => new DirectFFTTestbench(genIn = DspComplex(getReal, getReal), config = new FFTConfig(n = 4, p = 4))) {
      c => new DirectFFTTester(c)
    } should be (true)
  }


}

