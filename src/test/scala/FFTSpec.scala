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

import cde._
import junctions._
import uncore.tilelink._
import uncore.coherence._

object LocalTest extends Tag("edu.berkeley.tags.LocalTest")

class FFTTester[T<:Data:Real](c: FFTUnpacked[T], min: Int = -20, max: Int = 20) extends DspTester(c, base=10) {

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

  val parallelism = c.config.p
  val fft_size = c.config.n
  val bp = c.config.bp
  val pipe = c.config.pipelineDepth
  var latency = pipe + bp/2 + (0 until log2Up(bp)).map(x => (bp/pow(2,x+1)).toInt).foldRight(0)(_+_)
  val freq = 7.4
  val real_freq = fft_size-freq
  val input = (0 until fft_size).map(x => Complex(scala.math.sin(2*math.Pi * real_freq * x.toDouble / fft_size), scala.math.cos(2*math.Pi * real_freq * x.toDouble / fft_size))).toArray
  val output = Array.fill(fft_size)(Complex(0,0))
  var input_counter = 0
  var output_counter = -1
  var step_counter = 0
  var break = 0 
  reset(12)
  while (break == 0) {
    c.io.in.bits.zipWithIndex.foreach { case(port, index) => dspPoke(port, input((input_counter*parallelism+index)%fft_size)) }
    poke(c.io.in.sync, input_counter%bp == bp-1)
    val in_valid = Random.nextInt(2)
    poke(c.io.in.valid, in_valid)
    if (in_valid == 1) {
      input_counter = input_counter + 1
      step_counter = step_counter + 1
    }
    step(1)
    val valid = peek(c.io.out.valid).toInt
    val sync = peek(c.io.out.sync).toInt
    if (valid == 1 && output_counter >= 0) {
      c.io.out.bits.zipWithIndex.foreach { case(port, index) => {
        val subindex = output_counter*2*parallelism+index
        val bin = (bit_reverse(subindex%fft_size+subindex/fft_size*parallelism, log2Up(fft_size))) 
        val value = dspPeek(port).right.get
        val mag = value.abs
        output(bin) = dspPeek(port).right.get 
      }}
      output_counter = output_counter + 1
    }
    if (valid == 1 && sync == 1 && output_counter == -1 && step_counter >= latency) { output_counter = 0 } 
    else if (valid == 1 && sync == 1 && step_counter >= latency) { break = 1 }
  }

  var errs = Array.fill(output.size)(0.0)
  output.zip(fourierTr(DenseVector(input)).toArray).zipWithIndex.foreach { case ((chisel, ref), index) =>
    if (chisel != ref) {
      val epsilon = 1e-12
      val err = (chisel-ref).abs/(ref.abs+epsilon)
      val refabs = ref.abs
      val chiselabs = chisel.abs
      errs(index) = err
      assert(err < epsilon || ref.abs < epsilon, s"Error: mismatch on bin $index of $err\n\tReference: $ref\n\tChisel:    $chisel")
    }
  }

  //import java.io._
  //val pw = new PrintWriter(new File(s"n${fft_size}p${parallelism}k${pipe}.errors"))
  //pw.write(errs.deep.mkString("\n"))
  //pw.close

  //import co.theasi.plotly._
  //val x = (0 until output.size)
  //val y = fourierTr(DenseVector(input)).toArray
  //val p = Plot()
  //  .withScatter(x, output.map(_.real), ScatterOptions().name("Chisel"))
  //  .withScatter(x, y.map(_.real), ScatterOptions().name("Reference"))
  //draw(p, "real spectrum", writer.FileOptions(overwrite=true))
  //val q = Plot()
  //  .withScatter(x, output.map(_.imag), ScatterOptions().name("Chisel"))
  //  .withScatter(x, y.map(_.imag), ScatterOptions().name("Reference"))
  //draw(q, "imag spectrum", writer.FileOptions(overwrite=true))
}

class FFTSpec extends FlatSpec with Matchers {
  implicit val p = Parameters.empty.alter(Map(
	NastiKey -> NastiParameters(64, 64, 64),
    PAddrBits -> 64,
    CacheBlockOffsetBits -> 0,
    AmoAluOperandBits -> 0,
    TLId -> "FFT",
    TLKey("FFT") ->
        TileLinkParameters(
          coherencePolicy = new MICoherence(
            new NullRepresentation(1)),
          nManagers = 1,
          nCachingClients = 0,
          nCachelessClients = 1,
          maxClientXacts = 4,
          maxClientsPerPort = 1,
          maxManagerXacts = 1,
          dataBeats = 8,
          dataBits = 64)

  ))

  // FFT
  behavior of "FFT"
  it should "Fourier transform the input, fastly" in {
    //def getReal(): FixedPoint = FixedPoint(width = 16, binaryPoint = 7)
    def getReal(): DspReal = DspReal(0.0)
    for (i <- 2 until 7 by 2) {
      for (j <- 1 until i+1) {
        for (k <- 0 until i+1 by 2) {
          chisel3.iotesters.Driver(() => new FFTUnpacked(genIn = DspComplex(getReal, getReal), config = new FFTConfig(n = pow(2,i).toInt, p = pow(2,j).toInt, pipelineDepth=k))) {
            c => new FFTTester(c)
          } should be (true)
        }
      }
    }
  }
}

object FFTVerilog extends App {
  implicit val p = Parameters.empty.alter(Map(
	NastiKey -> NastiParameters(64, 64, 64)
  ))

  override def main(args: Array[String]): Unit = {
    import firrtl._
    def getReal(): DspReal = DspReal(0.0)
    //def getReal(): FixedPoint = FixedPoint(width = 16, binaryPoint = 7)
    val input = chisel3.Driver.emit(() => new FFT(genIn = DspComplex(getReal, getReal), config = new FFTConfig(n = 8, p = 8)))
    val om = new ExecutionOptionsManager("FFT") with HasFirrtlOptions
    om.setTargetDirName("generated-src")
    om.setTopName("FFT")
    om.firrtlOptions = om.firrtlOptions.copy(firrtlSource = Some(input))
    println(firrtl.Driver.execute(om))
  }
}

object FFTSpec {
  implicit val p = Parameters.empty.alter(Map(
	NastiKey -> NastiParameters(64, 64, 64)
  ))

  def getReal(): DspReal = new DspReal
  def main(args: Array[String]): Unit = {
    dsptools.Driver.executeFirrtlRepl(
      () => new FFT(genIn = DspComplex(getReal, getReal), config = new FFTConfig(n = 8, p = 4))
    )
  }
}
