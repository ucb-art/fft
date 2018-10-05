package fft

import breeze.math.{Complex}
import breeze.signal.{fourierTr}
import breeze.linalg._
import chisel3._
import chisel3.experimental._
import chisel3.util._
import chisel3.internal.firrtl.KnownBinaryPoint
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

import craft._
import dsptools._
import dsptools.numbers.{Field=>_,_}
import dsptools.numbers.implicits._

import freechips.rocketchip.config._

import scala.collection.mutable.{ArrayBuffer, Map}

/**
  * Case class for holding FFT configuration information
  * Also calculates lots of useful intermediate values for FFTs in general, such as
  * pipeline register locations and twiddle factors
  * @param n Total size of the FFT
  * @param pipelineDepth Number of pipeline registers inserted (locations automatically chosen)
  * @param lanes Number of parallel input and output lanes
  */
case class FFTConfig[T <: Data](
  genIn: DspComplex[T],
  genOut: DspComplex[T],
  n: Int = 16, // n-point FFT
  pipelineDepth: Int = 0,
  lanes: Int = 8,
  quadrature: Boolean = true,
) {
  require(n >= 4, "For an n-point FFT, n must be 4 or more")
  require(isPow2(n), "For an n-point FFT, n must be a power of 2")
  require(pipelineDepth >= 0, "Cannot have negative pipelining, you silly goose.")
  //if (pipelineDepth < 0) { pipelineDepth = log2Up(n) }

  val real = !quadrature
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

  def rotateRight[A](seq: Seq[A], i: Int): Seq[A] = {
    val size = seq.size
    seq.drop(size - (i % size)) ++ seq.take(size - (i % size))
  }
  def rotateLeft[A](seq: Seq[A], i: Int): Seq[A] = {
    val size = seq.size
    seq.drop(i % size) ++ seq.take(i % size)
  }

  def findCol(l: Seq[Seq[Int]], i: Int): Int = {
    for (row <- l) {
      row.zipWithIndex.foreach{ case (value, column) => {
        if (value == i) { return column }
      }}
    }
    return -1
  }

  // bp stands for biplex points, so the biplex FFT is a bp-point FFT
  val lanes_new = if (quadrature) lanes/2 else lanes
  val bp = n/lanes_new

  // pipelining
  val num = (log2Ceil(n)+1).toDouble
  val ratio = num/(pipelineDepth%log2Ceil(n)+1)
  val stages_to_pipeline = (0 until pipelineDepth%log2Ceil(n)).map(x => if (ratio*(x+1) < num/2 && ratio*(x+1)-0.5 == floor(ratio*(x+1))) floor(ratio*(x+1)).toInt else round(ratio*(x+1)).toInt)
  val pipe = (0 until log2Ceil(n)).map(x => floor(pipelineDepth/log2Ceil(n)).toInt + {if (stages_to_pipeline contains (x+1)) 1 else 0})
  val direct_pipe = pipe.drop(log2Ceil(bp)).foldLeft(0)(_+_)
  val biplex_pipe = pipe.dropRight(log2Ceil(lanes_new)).foldLeft(0)(_+_)
  println("Pipeline registers inserted on stages: " + pipe.toArray.deep.mkString(","))
  println(s"Total biplex pipeline depth: $biplex_pipe")
  println(s"Total direct pipeline depth: $direct_pipe")

  // twiddling
  val twiddle = (0 until n/2).map(x => Array(cos(2*Pi/n*x),-sin(2*Pi/n*x)))

  // indicies to the twiddle factors
  var indices = Array.fill(log2Ceil(n))(0)
  var prev = Array.fill(log2Ceil(n))(0)
  for (i <- 1 until n/2) {
    val next = (0 until log2Ceil(n)).map(x => floor(i/pow(2,x)).toInt).reverse
    prev.zip(next).foreach{case(px,nx) => {if (nx != px) indices = indices :+ nx}}
    prev = next.toArray
  }
  indices = indices.map(x => bit_reverse(x, log2Ceil(n)-1))

  // take subsets of indices for split FFTs, then bit reverse to permute as needed
  var q = n
  var temp = Array(indices)
  var bindices = Array[Int]()
  while (q > lanes_new) {
    temp.foreach{x => bindices = bindices ++ x.take(1)}
    temp = temp.map(x => x.drop(1).splitAt((x.size-1)/2)).flatMap(x => Array(x._1, x._2))
    q = q/2
  }
  val dindices = (0 until temp.size).map(x => temp((x*2)%temp.size+x*2/temp.size)).flatten

  // how the biplex indices (bindices) map to hardware butterflies
  var tbindices = List.fill(log2Ceil(bp))(ArrayBuffer.fill(bp/2)(0)) 
  bindices.zipWithIndex.foreach{ case (bindex, index) => {
    val col = log2Floor(index+1)
    val repl = math.pow(2, log2Ceil(bp)-col-1).toInt
    val start = (index-math.pow(2,col).toInt+1)*repl
    for (i <- 0 until repl) {
      tbindices(col)(start+i) = bindex
    }
  }}

  // pre-compute set of twiddle factors per-butterfly, including rotation for pipelining
  val btwiddles = tbindices.zipWithIndex.map{ case(i, index) => {
    val rot_amt = pipe.dropRight(log2Up(n)-index).foldLeft(0)(_+_)
    val rot_list = rotateRight(i, rot_amt)
    rot_list.map(j => twiddle(j))
  }}

  // how the direct indices (dindices) map to hardware butterflies
  var tdindices = List(List(0,1),List(0,2))
  for (i <- 0 until log2Up(lanes_new)-2) {
    tdindices = tdindices.map(x => x.map(y => y+1))
    val tdindices_max = tdindices.foldLeft(0)((b,a) => scala.math.max(b,a.reduceLeft((d,c) => scala.math.max(c,d))))
    tdindices = tdindices ++ tdindices.map(x => x.map(y => y+tdindices_max))
    tdindices = tdindices.map(x => 0 +: x)
  }

  // pre-compute set of twiddle factors per-butterfly, including rotation for pipelining
  val dtwiddles = dindices.grouped(lanes_new-1).toList.transpose.zipWithIndex.map{ case(i,index) => {
    // rotate array to account for pipelining
    val col = findCol(tdindices, index)
    val rot_amt = pipe.drop(log2Ceil(bp)).dropRight(log2Up(lanes_new)-col).foldLeft(0)(_+_)
    val rot_list = rotateRight(i, rot_amt)
    // now map to twiddles
    rot_list.map(j => twiddle(j))
  }}

}

