package fft

import chisel3._
import cde.Parameters
import diplomacy.{LazyModule, LazyModuleImp}

class DspTop(p: Parameters) extends LazyModule {
  override lazy val module = new DspTopModule(p, this, new DspTopBundle(p))
}

class DspTopBundle(p: Parameters) extends Bundle {}

class DspTopModule[+L <: DspTop, +B <: DspTopBundle](p: Parameters, l: L, b: => B)
  extends LazyModuleImp(l)
