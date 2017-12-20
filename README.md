# HOpenCL

[![Build Status](https://travis-ci.org/lancelet/hopencl.svg?branch=opencl-1.2)](https://travis-ci.org/lancelet/hopencl)

Haskell OpenCL Wrapper API

## Overview

HOpenCL is a Haskell wrapper for Open Compute Lanugage (OpenCL)
developed by Khronos (www.khronos.org/opencl).

Currently OpenCL 1.1 is supported but you can expect 1.2 support to be
added in the near future.

The wrapper is divided into three sub-components:

- `Language.OpenCL.Host.Core`, which is a direct mapping of the OpenCL 
  API into Haskell.
- `Language.OpenCL.Host`, which is a high-level API built on top of the 
  core components.
- `Language.OpenCL.Host.GLInterop`, which adds OpenCL and OpenGL 
  interopobility to the core components.

## Installation

To build the package you require [GHC](http://www.haskell.org/ghc/) 7.4 or 
later.

To compile the package an OpenCL SDK must be installed. There are a number of 
different choices here:

- Apple Mountain Lion, install XCode 4.4.
- Windows and Linux plaforms you can install one of the following:
  - [AMD's APP SDK][AMDAPPSDK],
  - [Intel's OpenCL SDK][INTELSDK], or
  - [NVIDIA's Cuda SDK][NVIDIASDK].

## Optional Requisties

OpenCL interoperability with OpenGL requires the Haskell library
[OpenGL](http://hackage.haskell.org/package/OpenGL).

Additionally, the OpenCL interoperability with OpenGL examples require
the Haskell package [GLUT](http://hackage.haskell.org/package/GLUT).

## Building

To build, try:

```
stack build
```

## Continuous Integration

Travis is currently used for CI builds. Currently, builds are only performed on
MacOS agents due to the difficulty in setting up a CPU-only OpenCL ICD
(Installable Client Driver). See issue 
[#1](https://github.com/lancelet/hopencl/issues/1).

## Authors

- Jonathan Merritt - Commonwealth Bank
- J. Garrett Morris - Portland State Univeristy
- Benedict R. Gaster - Advanced Micro Devices

Contributions and improvements are always welcome.

[AMDAPPSDK]: https://developer.amd.com/amd-accelerated-parallel-processing-app-sdk/ "AMD's APP SDK"
[INTELSDK]: http://software.intel.com/en-us/articles/intel-opencl-sdk/ "Intel's OpenCL SDK"
[NVIDIASDK]: http://developer.nvidia.com/cuda/cuda-downloads "NVIDIA's Cuda SDK"
