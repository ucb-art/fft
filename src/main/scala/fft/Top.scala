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
import dspjunctions._
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

class DspTopBundle(p: Parameters) extends BasicDspBlockIO()(p) {}

class DspTopModule[+L <: DspTop, +B <: DspTopBundle](val p: Parameters, l: L, b: => B)
  extends LazyModuleImp(l) with DspModule {
    val io = IO(b)
    io <> module.io
  }

case object BuildDSP extends Field[(Parameters) => LazyDspBlock]

trait DspModule {
  val p: Parameters
  val module = LazyModule(p(BuildDSP)(p)).module
}

class DspBareTop(val p: Parameters) extends Module with DspModule {
  val io = IO(module.io.cloneType)
  io <> module.io
}
