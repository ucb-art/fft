Pipelined FFT [![Build Status](https://travis-ci.org/ucb-art/fft.svg?branch=master)](https://travis-ci.org/ucb-art/fft)
=======================

# Overview

This project contains a streaming, pipelined Fast Fourier Transform (FFT).

# Usage

TODO

# Specifications

## Interfaces

The FFT uses the DSP streaming interface (a subset of AXI4-Stream) on both the data input and data output.
There are nominally no status or control registers, but the SCR File requires at least one, so a status register mirrors the sync output.

The FFT supports any power of two size of 4 or greater (n >= 4). 
The input rate may be divided down, resulting in a number of parallel input lanes different from the FFT size. 
But the input lanes (p) must be a power of 2, greater than or equal to 2, but less than or equal to the FFT size. 

When the number of parallel inputs equals the FFT size, a simple, direct form, streaming FFT is used, as shown below. 
The dotted lines mark "stage" boundaries, or places where pipeline registers may be inserted. 
The input will never be pipelined, but the output might be pipelined based on your desired pipeline depth. 
Pipeline registers are automatically inserted in reasonable locations.

![In-place FFT](/doc/inplacefft.png?raw=true)
