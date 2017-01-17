Pipelined FFT [![Build Status](https://travis-ci.org/ucb-art/fft.svg?branch=master)](https://travis-ci.org/ucb-art/fft)
=======================

# Overview

This project contains a streaming, pipelined Fast Fourier Transform (FFT).

# Usage

## Inside this repo

Build the dependencies by typing `make libs`.
To test the block, type `make test`.
This runs the block tester in the `src/test/scala` directory.
To build the Verilog and IP-Xact output, type `make verilog`.
Results are placed in a `generated-src` directory.

## Outside this repo

In your Config class, add an FFTKey which points to the FFTConfig you want.
Set the size of the FFT (n) and the desired pipline depth in the config.
Hook up the input and output to other DSP streaming interfaces, and connect the AXI4 control interface to an AXI4 master or crossbar.

# Specifications

## Interfaces

The FFT uses the DSP streaming interface (a subset of AXI4-Stream) on both the data input and data output.
There are nominally no status or control registers, but the SCR File requires at least one, so a status register mirrors the sync output.

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

