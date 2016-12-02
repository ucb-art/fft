package fft

//import cde._
//import testchipip.WithSerialAdapter
//import uncore.tilelink.ClientUncachedTileLinkIO
//import rocketchip.PeripheryUtils
//import chisel3._
//import dsptools.numbers.{DspReal, DspComplex, Real}
//import dsptools.numbers.implicits._

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

// create a new DSP Configuration
class DspConfig extends Config(
  (pname, site, here) => pname match {
    case BuildDSP => { (q: Parameters) => {
      implicit val p = q
      //val dsp = Module(new FFT2[DspReal]()(p=p))
      Module(new FFT(genIn=DspComplex(DspReal(0.0), DspReal(0.0)), config=new FFTConfig(n=8, p=8)))
      // ()
    }}
    case _ => throw new CDEMatchError
  })
