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

class FFTTester[T<:Data:Real](c: FFT[T], min: Int = -20, max: Int = 20) extends DspTester(c, base=10) {

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
  val bp = c.config.bp
  val freq = 4
  val input = (0 until fft_size).map(x => Complex(scala.math.sin(2*math.Pi * freq * x.toDouble / fft_size), 0)).toArray
  val output = Array.fill(fft_size)(Complex(0,0))
  var input_counter = 0
  var output_counter = -1
  var break = 0 
  reset(12)
  while (break == 0) {
    c.io.in.bits.zipWithIndex.foreach { case(port, index) => dspPoke(port, input((input_counter*parallelism+index)%fft_size)) }
    poke(c.io.in.sync, input_counter%bp == bp-1)
    poke(c.io.in.valid, 1)
    input_counter = input_counter + 1
    step(1)
    val valid = peek(c.io.out.valid).toInt
    val sync = peek(c.io.out.sync).toInt
    if (valid == 1 && output_counter >= 0) { 
      
      c.io.out.bits.zipWithIndex.foreach { case(port, index) => {
        output(bit_reverse(bit_reverse(output_counter, log2Up(bp))*parallelism+index, log2Up(fft_size))) = dspPeek(port).right.get 
      }}
      output_counter = output_counter + 1 
    }
    if (valid == 1 && sync == 1 && output_counter == -1) { output_counter = 0 } 
    else if (valid == 1 && sync == 1) { break = 1 }
  }

  output.zip(fourierTr(DenseVector(input)).toArray).zipWithIndex.foreach { case ((chisel, ref), index) =>
    if (chisel != ref) {
      val epsilon = 1e-13
      val err = (chisel-ref).abs/(ref.abs+epsilon)
      assert(err < epsilon || ref.abs < epsilon, s"Error: mismatch on bin $index of $err\n\tReference: $ref\n\tChisel:    $chisel")
    }
  }

  //val x = (0 until output.size)
  //val y = fourierTr(DenseVector(input)).toArray
  //val p = Plot()
  //  .withScatter(x, output.map(_.abs), ScatterOptions().name("Chisel"))
  //  .withScatter(x, y.map(_.abs), ScatterOptions().name("Reference"))
  //draw(p, "spectrum", writer.FileOptions(overwrite=true))
}

class FFTSpec extends FlatSpec with Matchers {

  // FFT
  behavior of "FFT"
  it should "Fourier transform the input, fastly" in {
    def getReal(): DspReal = new DspReal
    for (i <- 2 until 5) {
      for (j <- 1 until i) {
        chisel3.iotesters.Driver(() => new FFT(genIn = DspComplex(getReal, getReal), config = new FFTConfig(n = pow(2,i).toInt, p = pow(2,j).toInt))) {
          c => new FFTTester(c)
        } should be (true)
      }
    }
  }

}

object FFTSpec {
  def getReal(): DspReal = new DspReal
  def main(args: Array[String]): Unit = {
    dsptools.Driver.executeFirrtlRepl(
      () => new FFT(genIn = DspComplex(getReal, getReal), config = new FFTConfig(n = 8, p = 2))
    )
  }
}
