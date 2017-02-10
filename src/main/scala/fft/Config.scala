package fft

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
import dsptools.numbers.implicits._
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
import dsptools.numbers.{Field=>_,_}
import dsptools.numbers.implicits._

import scala.collection.mutable.Map

object FFTConfigBuilder {
  def apply[T <: Data : Real](
    id: String, fftConfig: FFTConfig, genIn: () => T, genOut: Option[() => T] = None): Config = new Config(
    (pname, site, here) => pname match {
      case FFTKey(id) => fftConfig
      case IPXACTParameters(id) => {
        val parameterMap = Map[String, String]()

        // Conjure up some IPXACT synthsized parameters.
        val gk = site(GenKey(id))
        val fftsize = fftConfig.n
        // double these because genIn is underlying type, but input is complex
        val totalWidthIn = gk.lanesIn * genIn().getWidth * 2 
        val totalWidthOut = gk.lanesOut * genOut.getOrElse(genIn)().getWidth * 2
        parameterMap ++= List(
          ("nBands", (fftsize/gk.lanesIn).toString),
          ("InputLanes", gk.lanesIn.toString),
          ("InputTotalBits", totalWidthIn.toString),
          ("OutputLanes", gk.lanesOut.toString),
          ("OutputTotalBits", totalWidthOut.toString),
          ("OutputPartialBitReversed", "1")
        )

        // add fractional bits if it's fixed point
        // TODO: check if it's fixed point or not
        genIn() match {
          case fp: FixedPoint =>
            val fractionalBits = fp.binaryPoint
            parameterMap ++= List(
              ("InputFractionalBits", fractionalBits.get.toString)
            )
          case _ =>
        }
        genOut.getOrElse(genIn)() match {
          case fp: FixedPoint =>
            val fractionalBits = fp.binaryPoint
            parameterMap ++= List(
              ("OutputFractionalBits", fractionalBits.get.toString)
            )
          case _ =>
        }

        // tech stuff, TODO
        parameterMap ++= List(("ClockRate", "100"), ("Technology", "TSMC16nm"))

        parameterMap
      }
      case _ => throw new CDEMatchError
    }) ++
  ConfigBuilder.dspBlockParams(id, fftConfig.lanes, () => DspComplex(genIn(), genIn()), genOutFunc = Some(() => DspComplex(genOut.getOrElse(genIn)(), genOut.getOrElse(genIn)())))
  def standalone[T <: Data : Real](
    id: String, fftConfig: FFTConfig, genIn: () => T, genOut: Option[() => T] = None): Config =
    apply(id, fftConfig, genIn, genOut) ++
    ConfigBuilder.buildDSP(id, {implicit p: Parameters => new LazyFFTBlock[T]})
}

class DefaultStandaloneRealFFTConfig extends Config(FFTConfigBuilder.standalone("fft", FFTConfig(n=128), () => DspReal()))
class DefaultStandaloneFixedPointFFTConfig extends Config(FFTConfigBuilder.standalone("fft", FFTConfig(n=128), () => FixedPoint(16.W, 8.BP), Some(() => FixedPoint(20.W, 8.BP))))

case class FFTKey(id: String) extends Field[FFTConfig]

// [stevo]: select twiddle factor size
// default is None, which gets mapped to Output
// if output is fixedpoint, then this is same total bits
// but only 2 whole bits, the rest fractional
trait HasFFTGenParameters[T <: Data] extends HasGenParameters[T, T] {
  def genTwiddle: Option[T] = {
    genOut() match {
      case fp: FixedPoint =>
        val totalBits = fp.getWidth
        val t = FixedPoint(totalBits.W, (totalBits-2).BP)
        Some(DspComplex(t, t).asInstanceOf[T])
      case _ => None
    }
  }
}

/**
  * Case class for holding FFT configuration information
  * Also calculates lots of useful intermediate values for FFTs in general, such as
  * pipeline register locations and twiddle factors
  * @param n Total size of the FFT
  * @param pipelineDepth Number of pipeline registers inserted (locations automatically chosen)
  * @param lanes Number of parallel input and output lanes
  * @param real Not currently used
  */
case class FFTConfig(n: Int = 8, // n-point FFT
                     pipelineDepth: Int = 0,
                     lanes: Int = 8,
                     real: Boolean = false // real inputs?
                    ) {
  require(n >= 4, "For an n-point FFT, n must be 4 or more")
  require(isPow2(n), "For an n-point FFT, n must be a power of 2")
  require(pipelineDepth >= 0, "Cannot have negative pipelining, you silly goose.")

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

  // bp stands for biplex points, so the biplex FFT is a bp-point FFT
  val bp = n/lanes

  // pipelining
  val num = (log2Up(n)+1).toDouble
  val ratio = num/(pipelineDepth%log2Up(n)+1)
  val stages_to_pipeline = (0 until pipelineDepth%log2Up(n)).map(x => if (ratio*(x+1) < num/2 && ratio*(x+1)-0.5 == floor(ratio*(x+1))) floor(ratio*(x+1)).toInt else round(ratio*(x+1)).toInt)
  val pipe = (0 until log2Up(n)).map(x => floor(pipelineDepth/log2Up(n)).toInt + {if (stages_to_pipeline contains (x+1)) 1 else 0})
  val direct_pipe = pipe.drop(log2Up(bp)).foldLeft(0)(_+_)
  val biplex_pipe = pipe.dropRight(log2Up(lanes)).foldLeft(0)(_+_)
  println("Pipeline registers inserted on stages: " + pipe.toArray.deep.mkString(","))
  println(s"Total biplex pipeline depth: $biplex_pipe")
  println(s"Total direct pipeline depth: $direct_pipe")

  // twiddling
  val twiddle = (0 until n/4).map(x => Array(cos(2*Pi/n*x),-sin(2*Pi/n*x)))

  // indicies to the twiddle factors
  var indices = Array.fill(log2Up(n))(0)
  var prev = Array.fill(log2Up(n))(0)
  for (i <- 1 until n/2) {
    val next = (0 until log2Up(n)).map(x => floor(i/pow(2,x)).toInt).reverse
    prev.zip(next).foreach{case(lanes,n) => {if (n != lanes) indices = indices :+ n}}
    prev = next.toArray
  }
  indices = indices.map(x => bit_reverse(x, log2Up(n)-1))

  // take subsets of indices for split FFTs, then bit reverse to permute as needed
  var q = n
  var temp = Array(indices)
  var bindices = Array[Int]()
  while (q > lanes) {
    temp.foreach{x => bindices = bindices ++ x.take(1)}
    temp = temp.map(x => x.drop(1).splitAt((x.size-1)/2)).flatMap(x => Array(x._1, x._2))
    q = q/2
  }
  val dindices = (0 until temp.size).map(x => temp((x*2)%temp.size+x*2/temp.size)).flatten
}

