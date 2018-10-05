package fft

import chisel3._
import chisel3.util._
import dsptools.numbers.{DspComplex, Real}
import dsptools.numbers.implicits._
import scala.math._

// single radix-2 butterfly
object Butterfly {
  def apply[T<:Data:Real](in: Seq[DspComplex[T]], twiddle: DspComplex[T]): Seq[DspComplex[T]] = 
  {
    require(in.length == 2, "Butterfly requires two data inputs")   
    val product = in(1)*twiddle
    Seq(in(0)+product, in(0)-product)
  }
}

// simple barrel shifter, probably doesn't produce efficient hardware though
object BarrelShifter {
  def apply[T<:Data](in: Vec[T], shift: UInt): Vec[T] = 
  {
    VecInit((0 until in.size).map(i => {
      val idx = Wire(UInt(width=log2Ceil(in.size).W))
      idx := shift + i.U
      in(idx)
    }))
  }
}

