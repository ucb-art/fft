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

import fft.Generator.params

import dsptools._

import scala.collection.mutable.Map

trait HasIPXACTParameters {
  def getIPXACTParameters: Map[String, String]
}

case object FFTSize extends Field[Int]

// create a new DSP Configuration
class DspConfig extends Config(
  (pname, site, here) => pname match {
    case BuildDSP => { (q: Parameters) => {
      Module(new FFTWrapper[DspReal]()(DspRealRealImpl, q))
    }}
    case FFTSize => 8
    case FFTKey => { (q: Parameters) => { 
      implicit val p = q
      FFTConfig[DspReal](n = site(FFTSize))
    }}
    //NastiId => "FFT"
	  case NastiKey => NastiParameters(64, 32, 1)
    case PAddrBits => 32
    case CacheBlockOffsetBits => 6
    case AmoAluOperandBits => 64
    case TLId => "FFT"
    case TLKey("FFT") =>
        TileLinkParameters(
          coherencePolicy = new MICoherence(
            new NullRepresentation(1)),
          nManagers = 1,
          nCachingClients = 0,
          nCachelessClients = 1,
          maxClientXacts = 4,
          maxClientsPerPort = 1,
          maxManagerXacts = 1,
          dataBeats = 1,
          dataBits = 64)
    case DspBlockKey => DspBlockParameters(1024, 1024)
    case GenKey => new GenParameters {
      def getReal(): DspReal = DspReal(0.0).cloneType
      def genIn [T <: Data] = DspComplex(getReal(), getReal()).asInstanceOf[T]
      override def genOut[T <: Data] = DspComplex(getReal(), getReal()).asInstanceOf[T]
      val lanesIn = 8
      override val lanesOut = 8
    }
    case _ => throw new CDEMatchError
  }) with HasIPXACTParameters {
  def getIPXACTParameters: Map[String, String] = {
    // Get unadulterated, top level parameters.
    val parameterList = List[Field[_]](TLId, PAddrBits)
    val parameterMap = parameterList.foldLeft(Map[String, String]()) { (m, s) => m(s.toString) = params(s).toString; m }

    // Get some nested parameters.
    val (nastiDataBits, nastiAddrBits, nastiIdBits) = params(NastiKey) match {
      case NastiParameters(d: Int, a: Int, i: Int) => (d, a, i)
    }
    parameterMap ++= List(("nastiDataBits", nastiDataBits.toString))

    // Get some nested parameters, one level deeper.
    val (nManagers, dataBits) = params(TLKey(params(TLId))) match {
      case TileLinkParameters(    coherencePolicy: CoherencePolicy,
      nManagers: Int,
      nCachingClients: Int,
      nCachelessClients: Int,
      maxClientXacts: Int,
      maxClientsPerPort: Int,
      maxManagerXacts: Int,
      dataBits: Int,
      dataBeats: Int,
      overrideDataBitsPerBeat: Option[Int]
    ) => (nManagers.toString, dataBits)
    }
    parameterMap ++= List(("nManagers", nManagers.toString), ("dataBits", dataBits.toString))

    // Conjure up some IPXACT synthsized parameters.
    val fftSize = params(FFTSize)
    parameterMap ++= List(("IsComplex", "0"), ("IsSigned", "1"), ("FFTSize", fftSize.toString))

    parameterMap
  }
}

case object FFTKey extends Field[(Parameters) => FFTConfig[DspReal]]

trait HasFFTGenParameters[T <: Data] extends HasGenParameters[T, T] {
   def genTwiddle: Option[T] = None
}

case class FFTConfig[T<:Data:Real](n: Int = 8, // n-point FFT
                                   pipelineDepth: Int = 0,
                                   real: Boolean = false // real inputs?
                                  )(implicit val p: Parameters) extends HasFFTGenParameters[DspComplex[T]] {
  assert(lanesIn == lanesOut, "FFT must have an equal number of input and output lanes")
  assert(n >= 4, "For an n-point FFT, n must be 4 or more")
  assert(lanesIn >= 2, "Must have at least 2 parallel inputs")
  assert(isPow2(n), "For an n-point FFT, n must be a power of 2")
  assert(pipelineDepth >= 0, "Cannot have negative pipelining, you silly goose.")
  assert(lanesIn <= n, "An n-point FFT cannot have more than n inputs (p must be less than or equal to n)")
  assert(isPow2(lanesIn), "FFT parallelism must be a power of 2")

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
  val bp = n/lanesIn

  // pipelining
  val num = (log2Up(n)+1).toDouble
  val ratio = num/(pipelineDepth%log2Up(n)+1)
  val stages_to_pipeline = (0 until pipelineDepth%log2Up(n)).map(x => if (ratio*(x+1) < num/2 && ratio*(x+1)-0.5 == floor(ratio*(x+1))) floor(ratio*(x+1)).toInt else round(ratio*(x+1)).toInt)
  val pipe = (0 until log2Up(n)).map(x => floor(pipelineDepth/log2Up(n)).toInt + {if (stages_to_pipeline contains (x+1)) 1 else 0})
  val direct_pipe = pipe.drop(log2Up(bp)).foldLeft(0)(_+_)
  val biplex_pipe = pipe.dropRight(log2Up(lanesIn)).foldLeft(0)(_+_)
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
    prev.zip(next).foreach{case(lanesIn,n) => {if (n != lanesIn) indices = indices :+ n}}
    prev = next.toArray
  }
  indices = indices.map(x => bit_reverse(x, log2Up(n)-1))

  // take subsets of indices for split FFTs, then bit reverse to permute as needed
  var q = n
  var temp = Array(indices)
  var bindices = Array[Int]()
  while (q > lanesIn) {
    temp.foreach{x => bindices = bindices ++ x.take(1)}
    temp = temp.map(x => x.drop(1).splitAt((x.size-1)/2)).flatMap(x => Array(x._1, x._2))
    q = q/2
  }
  val dindices = (0 until temp.size).map(x => temp((x*2)%temp.size+x*2/temp.size)).flatten

  // create rom
  val twiddle_rom = Wire(Vec(twiddle.size, genTwiddle.getOrElse(genIn())))
  twiddle_rom.zip(twiddle).foreach { case(rom, value) => rom := DspComplex.wire(implicitly[Real[T]].fromDouble(value(0)), implicitly[Real[T]].fromDouble(value(1))) }
}

