package fft

import chisel3._
import chisel3.util._
import dsptools.numbers.{DspComplex, Real}
import dsptools.numbers.implicits._
import scala.math._

object FFTFunctions {

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

}

case class FFTConfig(n: Int = 8, // n-point FFT
                     p: Int = 8, // parallelism, or number of parallel inputs
                     pipelineDepth: Int = 0,
                     real: Boolean = false // real inputs?
                    ) {
  assert(n >= 4, "For an n-point FFT, n must be 4 or more")
  assert(isPow2(n), "For an n-point FFT, n must be a power of 2")
  assert(pipelineDepth >= 0, "Cannot have negative pipelining, you silly goose.")
  assert(p <= n, "An n-point FFT cannot have more than n inputs (p must be less than or equal to n)")
  assert(isPow2(p), "FFT parallelism must be a power of 2")

  // bp stands for biplex points, so the biplex FFT is a bp-point FFT
  val bp = n/p

  val biplex_pipe = floor(log2Up(bp).toDouble/log2Up(n)*pipelineDepth).toInt
  val direct_pipe = pipelineDepth-biplex_pipe

  // calculate where to place pipeline registers
  // synthesis retiming will take care of the rest 
  // if more than one pipeline register per stage is requested, registers are doubled up

  // biplex
  val b_num = (log2Up(bp)+1).toDouble
  val b_den = pipelineDepth%log2Up(bp)+1
  val b_stages_to_pipeline = (0 until biplex_pipe%log2Up(bp)).map(x => if (b_num/b_den*(x+1) < b_num/2 && b_num/b_den*(x+1)-0.5 == floor(b_num/b_den*(x+1))) floor(b_num/b_den*(x+1)).toInt else round(b_num/b_den*(x+1)).toInt)
  val b_pipelines_per_stage = floor(biplex_pipe/log2Up(bp)).toInt
  val b_pipe_amts = (0 until log2Up(bp)).map(x => b_pipelines_per_stage + {if (b_stages_to_pipeline contains (x+1)) 1 else 0})

  // direct
  val d_num = (log2Up(p)+1).toDouble
  val d_den = pipelineDepth%log2Up(p)+1
  val d_stages_to_pipeline = (0 until direct_pipe%log2Up(p)).map(x => if (d_num/d_den*(x+1) < d_num/2 && d_num/d_den*(x+1)-0.5 == floor(d_num/d_den*(x+1))) floor(d_num/d_den*(x+1)).toInt else round(d_num/d_den*(x+1)).toInt)
  val d_pipelines_per_stage = floor(direct_pipe/log2Up(p)).toInt
  val d_pipe_amts = (0 until log2Up(p)).map(x => d_pipelines_per_stage + {if (d_stages_to_pipeline contains (x+1)) 1 else 0})
  val d_pipe_delays = d_pipe_amts.scanLeft(0)((a,b) => a+b).dropRight(1).+:(-1)
}

// single radix-2 butterfly
object Butterfly {
  def apply[T<:Data:Real](in: Seq[DspComplex[T]], twiddle: DspComplex[T]): Seq[DspComplex[T]] = 
  {
    require(in.length == 2, "Butterfly requires two data inputs")   
    val product = in(1)*twiddle
    List(in(0)+product, in(0)-product)
  }
}

// simple barrel shifter, probably doesn't produce efficient hardware though
object BarrelShifter {
  def apply[T<:Data](in: Vec[T], shift: UInt): Vec[T] = 
  {
    Vec((0 until in.size).map(i => {
      val idx = UInt(width=log2Up(in.size))
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
        val sram0 = SeqMem(n, in.cloneType)
        val sram1 = SeqMem(n, in.cloneType)
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
        val sram = SeqMem(n/2, Vec(in, in))
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
      val sram = SeqMem(n, in.cloneType)
      val index_counter = Counter(en, n)._1
      when (en) {
        sram(index_counter) := in
      }
      sram(index_counter)
    }
  }
}
