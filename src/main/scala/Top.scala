package fft

import chisel3._
import cde.Parameters
import diplomacy.{LazyModule, LazyModuleImp}
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

class DspTop(p: Parameters) extends LazyModule {
  override lazy val module = Module(new DspTopModule(p, this, new DspTopBundle(p)))
}

class DspTopBundle(p: Parameters) extends Bundle {}

class DspTopModule[+L <: DspTop, +B <: DspTopBundle](val p: Parameters, l: L, b: => B)
  extends LazyModuleImp(l) with DspModule {
    val io = IO(b)
  }

case object BuildDSP extends Field[(Parameters) => Module]

trait DspModule {
  // import LocalParams._

  implicit val p: Parameters
  val module = p(BuildDSP)(p)
}

object LocalParams {
  def getReal(): DspReal = DspReal(0.0).cloneType
  implicit val p = Parameters.empty.alter(Map(
    FFTKey -> FFTConfig(n = 8, p = 8),
    //NastiId -> "FFT",
	NastiKey -> NastiParameters(64, 32, 1),
    PAddrBits -> 32,
    CacheBlockOffsetBits -> 6,
    AmoAluOperandBits -> 64,
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
          dataBeats = 1,
          dataBits = 64),
    DspBlockKey -> DspBlockParameters(-4, -4),
    GenKey -> new GenParameters {
      def genIn [T <: Data] = DspComplex(getReal(), getReal()).asInstanceOf[T]
      override def genOut[T <: Data] = DspComplex(getReal(), getReal()).asInstanceOf[T]
      val lanesIn = 8
      override val lanesOut = 8
    }
  ))
}
