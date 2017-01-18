// See LICENSE for license details.

// Author: Stevo Bailey (stevo.bailey@berkeley.edu)

package fft

import chisel3.util._
import chisel3._
import dsptools.numbers._
import dsptools.numbers.implicits._
import firrtl._

object FFTVerilog extends DspGeneratorApp {
  val longName = "fft"
  override def main(args: Array[String]): Unit = {
    implicit val p = getParameters(new DspConfig)
    //def getReal(): DspReal = DspReal(0.0)
    def getReal(): FixedPoint = FixedPoint(width = 16, binaryPoint = 7)
    val input = chisel3.Driver.emit(() => new FFT[FixedPoint]())
    val om = new ExecutionOptionsManager("FFT") with HasFirrtlOptions
    om.setTargetDirName("generated-src")
    om.setTopName("FFT")
    om.firrtlOptions = om.firrtlOptions.copy(firrtlSource = Some(input))
    println(firrtl.Driver.execute(om))
  }
}

