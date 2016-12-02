package fft

import cde._
import testchipip.WithSerialAdapter
import uncore.tilelink.ClientUncachedTileLinkIO
import rocketchip.PeripheryUtils
import chisel3._
import dsptools.numbers.DspReal

case object BuildDSP extends Field[(Parameters) => Unit]

// create a new DSP Configuration
class DspConfig extends Config(
  (pname, site, here) => pname match {
    case BuildDSP => (p: Parameters) => {
      //import LocalParams._
      val dsp = Module(new FFT2[DspReal]()(p))
      ()
    }
    case _ => throw new CDEMatchError
  })
