Pipelined FFT [![Build Status](https://travis-ci.org/ucb-art/fft.svg?branch=master)](https://travis-ci.org/ucb-art/fft)
=======================

# Overview

This project contains a streaming, pipelined Fast Fourier Transform (FFT).

# Usage

## GitHub Pages

See ![here](https://ucb-art.github.io/fft/latest/api/) for the GitHub pages scaladoc.

## Building

Build the dependencies by typing `make libs`.
To build the Verilog and IP-Xact output, type `make verilog`.
Results are placed in a `generated-src` directory.

## Testing

To test the block, type `make test`.
This runs the block tester in the `src/test/scala` directory.
It currently just tests the DC bin until unscrambling and futher testing capabilities are added.

## Configuring

In `src/main/scala` there is a `Config.scala` file.
In the `DspConfig` class are a bunch of parameters, like `FFTSize` and `FractionalBits`.
Set these to your desired values, then rebuild and retest the design.

TODO: changing between FixedPoint and DspReal


# Specifications

## Interfaces

The FFT uses the DSP streaming interface (a subset of AXI4-Stream) on both the data input and data output.
There are nominally no status or control registers, but the SCR File requires at least one, so a status register mirrors the sync output.

## Signaling

### Bits

It is expected that the bits inputs contain time-series data time-multiplexed on the inputs, such that on the first cycle are values x[0], x[1], …, x[p-1], then the next cycle contains x[p], x[p+1], … and this continues until the input is x[n-p], x[n-p+1], …, x[n-1]. 
The outputs are scrambled spectral bins. 
Since there is some unscrambling between the biplex and direct form FFT, the output indices are not purely bit reversed. 
The 0th output on each cycle increments by one every cycle, while the other outputs are bit reversed values of the 0th value plus the output number. 
For example, if n = 16 and p = 4, the outputs are time-multiplexed across n/p = 4 cycles. 
Thus the outputs are X[0], X[8], X[4], X[12] on cycle 0, X[1], X[9], X[5], X[13] on cycle 1, X[2], X[10], X[6], X[14] on cycle 2, and X[3], X[11], X[7], X[15] on cycle 3.

### Valid

The FFT does not keep track of which bits are valid after coming out of reset, so valid simply passes through the FFT on the same cycle. 
When valid goes low, all registers in the design are paused.

### Sync

Like valid, the FFT does not keep track of the initial state of synchronization coming out of reset, so the sync signal is passed through the FFT after being pipelined appropriately. 
Thus the first few sync signals at the output coming out of reset maybe be incorrect. 
But the first input sync signal will set flush through, synchronizing all the FFT butterflies with the first dataset. 
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

