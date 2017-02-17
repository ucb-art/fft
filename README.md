Pipelined FFT [![Build Status](https://travis-ci.org/ucb-art/fft.svg?branch=master)](https://travis-ci.org/ucb-art/fft)
=======================

# Overview

This project contains a streaming, pipelined Fast Fourier Transform (FFT).
Twiddle factors are hard-coded for you.
The transform is split into pipelined Biplex FFTs and a direct form FFT to multiplex the logic for large FFT sizes.

# Usage

## GitHub Pages

See [here](https://ucb-art.github.io/fft/latest/api/) for the GitHub pages scaladoc.

## Setup

Clone the repository and update the depenedencies:

```
git clone git@github.com:ucb-art/pfb.git
git submodule update --init
cd dsp-framework
./update.bash
cd ..
```

See the [https://github.com/ucb-art/dsp-framework/blob/master/README.md](dsp-framework README) for more details on this infrastructure.
Build the dependencies by typing `make libs`.

## Building

The build flow generates FIRRTL, then generates Verilog, then runs the TSMC memory compiler to generate memories.
Memories are black boxes in the Verilog by default.
IP-Xact is created with the FIRRTL.
The build targets for each of these are firrtl, verilog, and mems, respectively.
Depedencies are handled automatically, so to build the Verilog and memories, just type `make mems`.
Results are placed in a `generated-src` directory.

## Testing

To test the block, type `make test`.
This runs the block tester in the `src/test/scala` directory.

## Configuring

In `src/main/scala` there is a `Config.scala` file.
A few default configurations are defined for you, called DefaultStandaloneXFFTConfig, where X is either Real or FixedPoint.
These generate a small FFT with default parameters.
To run them, type `make verilog CONFIG=DefaultStandaloneXFFTConfig`, replacing X with Real or FixedPoint.
The default make target is the default FixedPoint configuration.

The suggested way to create a custom configuration is to modify CustomStandaloneFFTConfig, which defines values for all possible parameters.
Then run `make verilog CONFIG=CustomStandaloneFFTConfig` to generate the Verilog.
Choosing lanes = FFT size (n) (both default to 8) only creates a direct form FFT.

# Specifications

## Interfaces

The PFB uses the [https://github.com/ucb-art/rocket-dsp-utils/blob/master/doc/stream.md](DSP streaming interface) (a subset of AXI4-Stream) on both the data input and data output.
There are nominally no status or control registers, so no SCR file exists.

## Signaling

### Bits

It is expected that the bits inputs contain time-series data time-multiplexed on the inputs, such that on the first cycle are values x[0], x[1], …, x[p-1], then the next cycle contains x[p], x[p+1], … and this continues until the input is x[n-p], x[n-p+1], …, x[n-1]. 
The outputs are scrambled spectral bins. 
Since there is some unscrambling between the biplex and direct form FFT, the output indices are not purely bit reversed. 
The unscrambling routine is hard to describe in words, but the FFT tester has a definition to handle this, copied here.
The FFT size is inferred from the length of `in`, and `p` gives the number of lanes in the design.

```
def unscramble(in: Seq[Complex], p: Int): Seq[Complex] = {
  val n = in.size
  val bp = n/p
  val res = Array.fill(n)(Complex(0.0,0.0))
  in.grouped(p).zipWithIndex.foreach { case (set, sindex) =>
    set.zipWithIndex.foreach { case (bin, bindex) =>
      if (bp > 1) {
        val p1 = if (sindex/(bp/2) >= 1) 1 else 0
        val new_index = bit_reverse((sindex % (bp/2)) * 2 + p1, log2Up(bp)) + bit_reverse(bindex, log2Up(n))
        res(new_index) = bin
      } else {
        val new_index = bit_reverse(bindex, log2Up(n))
        res(new_index) = bin
      }
    }
  }
  res
}
```

### Valid

The FFT delays the input valid by a value equal to the total data delay (biplex FFT delay + pipeline depth).
When valid is low, the FFT creates zeros at its input.
Internal counters continue to count, flushing out extant data.
The shift register delaying the valid signal is set to all 0s during reset.

### Sync

The shift register delaying the sync signal is set to all 0s during reset.
The first input sync signal will flush through, synchronizing all the FFT butterflies with the first dataset. 
The input sync is expected to be periodic in the size of the FFT (n) divided by the number of input lanes (p). 
Sync should be high on the last cycle of the spectrum. 
The new spectrum starts on the next valid cycle. 
When n=p, sync should always be high when valid is high.

## Implementation

The FFT supports any power of two size of 4 or greater (n >= 4). 
The input rate may be divided down, resulting in a number of parallel input lanes different from the FFT size. 
But the input lanes (p) must be a power of 2, greater than or equal to 2, but less than or equal to the FFT size. 

When the number of parallel inputs equals the FFT size, a simple, direct form, streaming FFT is used, as shown below. 
The dotted lines mark "stage" boundaries, or places where pipeline registers may be inserted. 
The input will never be pipelined, but the output might be pipelined based on your desired pipeline depth. 
Pipeline registers are automatically inserted in reasonable locations.

![In-place FFT](/doc/inplacefft.png?raw=true)

When the input is serialized, the FFT may have fewer input lanes than the size of the FFT. 
In this case, the inputs are assumed to arrive in time order, time-multiplexed on the same wires. 
To accommodate this time multiplexing, the FFT architecture changes. 
Pipelined biplex FFTs are inserted before the direct form FFT. 
These FFTs efficiently reuse hardware and memories to calculate the FFT at a slower rate but higher latency. 
The figure below shows their architecture, as taken from the JPL technical report. 
Since the channels are adjacent, N = n/2, where n is the size of the biplex FFT. 
The solid boxes are shift registers of delay shown, and the boxes with dotted lines in them are 2-input barrel shifters, periodically crossing or passing the inputs at a rate shown. 
The Xs in the diagram are butterflies. 
An extra shift register on input 1 of size N/2 aligns the adjacent channels, and extra shift registers of n/2 at the output unscramble the data before they arrive at the direct form FFT. 
Pipeline registers may be inserted after each butterfly, but never at the input or output.

![Biplex FFT](/doc/biplexfft.png?raw=true)

A final direct form FFT sits at the output of the biplex FFTs, finishing the Fourier transform. 
Thus the overall architecture looks like below. 
Pipeline registers favor the direct form FFT slightly, though the critical path through this circuit is still through log2(n) butterflies, so one pipeline register per stage (a pipeline depth of log2(n)) is recommended.

![Split FFT](/doc/splitfft.png?raw=true)

