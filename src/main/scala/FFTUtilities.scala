package fft

import chisel3._
import chisel3.util._
import dsptools.numbers.{DspComplex, Real}
import dsptools.numbers.implicits._
import scala.math._

import cde._

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
    Vec((0 until in.size).map(i => {
      val idx = Wire(UInt(width=log2Up(in.size)))
      idx := shift + UInt(i)
      in(idx)
    }))
  }
}

// shift register implemented as an SRAM memory with internal counter
object ShiftRegisterMem {

  // use_sp_mem = use single port SRAMs?
  // use_two_srams = When using single-ported SRAMs, you have the option of using
  //   two SRAMs with type "in" and depth "n", or one SRAM with
  //   inputs twice the width of "in" and depth "n/2". I assume you
  //   want only 1 SRAM by default.

  // TODO: can we use SeqMem instead of Mem?
  def apply[T <: Data](in: T, n: Int, en: Bool = Bool(true), use_sp_mem: Boolean = false, use_two_srams: Boolean = false, name: String = null): T =
  {
    if (n%2 == 1 && use_sp_mem && !use_two_srams) {
      println("Warning: Creating a ShiftRegisterMem with an odd shift amount will use two SRAMs instead of one.")
    }
    if (n == 0) {
      in
    } else if (use_sp_mem) {
      val out = in.cloneType
      if (use_two_srams || n%2 == 1) {
        val sram0 = Mem(n, in.cloneType)
        val sram1 = Mem(n, in.cloneType)
        if (name != null) {
          println(s"No name support yet")
          //sram0.setName(name + "_0")
          //sram1.setName(name + "_1")
        }
        val (index_counter, switch_sram) = Counter(en, n)
        val sram_num = Reg(init=Bool(false))
        sram_num := Mux(switch_sram, ~sram_num, sram_num)
        val reg_raddr0 = Reg(UInt())
        val reg_raddr1 = Reg(UInt())
        val reg_waddr = Reg(next=index_counter, init=UInt(n-1, log2Up(n)))
        when (en) {
          when (sram_num) {
            sram1(reg_waddr) := in
            reg_raddr0 := index_counter
          } .otherwise {
            sram0(reg_waddr) := in
            reg_raddr1 := index_counter
          }
        }
        out := Mux(sram_num, sram0(reg_raddr0), sram1(reg_raddr1))
        out
      }
      else {
        val sram = Mem(n/2, Vec(in, in))
        if (name != null) {
          println(s"Name support not implemented")
          //sram.setName(name)
        }
        val index_counter = Counter(en, n)._1
        val reg_waddr = Reg(next=(index_counter >> UInt(1)), init=UInt(n/2-1, log2Up(n)-1))
        val reg_raddr = Reg(UInt())
        val des = Reg(in.cloneType)
        val ser = Reg(in.cloneType)
        when (en) {
          when (index_counter(0)) {
            sram(reg_waddr) := Vec(des, in)
          } .otherwise {
            des := in
            reg_raddr := Mux(index_counter === UInt(n-2), UInt(0), (index_counter >> UInt(1)) + UInt(1))
          }
        }
        when (index_counter(0)) {
          out := ser
        } .otherwise {
          val sram_out = sram(reg_raddr)
          ser := sram_out(1)
          out := sram_out(0)
        }
        out
      }
    } else {
      val sram = Mem(n, in.cloneType)
      val index_counter = Counter(en, n)._1
      when (en) {
        sram(index_counter) := in
      }
      sram(index_counter)
    }
  }
}
