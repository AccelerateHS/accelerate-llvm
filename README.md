<div align="center">
<img width="450" src="https://github.com/AccelerateHS/accelerate/raw/master/images/accelerate-logo-text-v.png?raw=true" alt="henlo, my name is Theia"/>

# LLVM backends for the Accelerate array language

[![CI](https://github.com/tmcdonell/accelerate-llvm/actions/workflows/ci.yml/badge.svg)](https://github.com/tmcdonell/accelerate-llvm/actions/workflows/ci.yml)
[![Gitter](https://img.shields.io/gitter/room/nwjs/nw.js.svg)](https://gitter.im/AccelerateHS/Lobby)
[![Hackage](https://img.shields.io/hackage/v/accelerate-llvm.svg)](https://hackage.haskell.org/package/accelerate-llvm)
[![Docker Automated build](https://img.shields.io/docker/automated/tmcdonell/accelerate-llvm.svg)](https://hub.docker.com/r/tmcdonell/accelerate-llvm/)
[![Docker status](https://images.microbadger.com/badges/image/tmcdonell/accelerate-llvm.svg)](https://microbadger.com/images/tmcdonell/accelerate-llvm)

</div>

This package compiles Accelerate code to LLVM IR, and executes that code on
multicore CPUs as well as NVIDIA GPUs. This avoids the need to go through `nvcc`
or `clang`. For details on Accelerate, refer to the [main repository][GitHub].

We love all kinds of contributions, so feel free to open issues for missing
features as well as report (or fix!) bugs on the [issue tracker][Issues].

  [GitHub]:  https://github.com/AccelerateHS/accelerate
  [Issues]:  https://github.com/AccelerateHS/accelerate/issues


 * [Dependencies](#dependencies)
 * [Docker](#docker)
 * [Installing LLVM](#installing-llvm)
   * [Homebrew](#homebrew)
   * [Debian/Ubuntu](#debianubuntu)
   * [Building from source](#building-from-source)


Dependencies
------------

Haskell dependencies are available from Hackage, but there are several external
library dependencies that you will need to install as well:

 * [`LLVM`](http://llvm.org)
 * [`libFFI`](http://sourceware.org/libffi/) (if using the `accelerate-llvm-native` backend for multicore CPUs)
 * [`CUDA`](https://developer.nvidia.com/cuda-downloads) (if using the `accelerate-llvm-ptx` backend for NVIDIA GPUs)
    [`Note that not all versions of CUDA support all NVIDIA GPUs`](https://en.wikipedia.org/wiki/CUDA#GPUs_supported)

Docker
------

A [docker](https://www.docker.com) container is provided with this package
preinstalled (via stack) at `/opt/accelerate-llvm`. Note that if you wish to use
the `accelerate-llvm-ptx` GPU backend, you will need to install the [NVIDIA
docker](https://github.com/NVIDIA/nvidia-docker) plugin; see that page for more
information.

```sh
$ docker run -it tmcdonell/accelerate-llvm
```


Installing LLVM
---------------

When installing LLVM, make sure that it includes the `libLLVM` shared library.
If you want to use the GPU targeting `accelerate-llvm-ptx` backend, make sure
you install (or build) LLVM with the 'nvptx' target.

## Homebrew

Example using [Homebrew](http://brew.sh) on macOS:

```sh
$ brew install llvm@15
```

## Debian/Ubuntu

For Debian/Ubuntu based Linux distributions, the LLVM.org website provides
binary distribution packages. Check [apt.llvm.org](http://apt.llvm.org) for
instructions for adding the correct package database for your OS version, and
then:

```sh
$ apt-get install llvm-15-dev
```

## Building from source

If your OS does not have an appropriate LLVM distribution available, you can also build from source. Detailed build instructions are available on the [LLVM.org website](http://releases.llvm.org/6.0.0/docs/CMake.html). Note that you will require at least [CMake 3.4.3](http://www.cmake.org/cmake/resources/software.html) and a recent C++ compiler; at least Clang 3.1, GCC 4.8, or Visual Studio 2015 (update 3).

  1. Download and unpack the [LLVM source code](https://github.com/llvm/llvm-project/releases/download/llvmorg-15.0.7/llvm-15.0.7.src.tar.xz). We'll refer to
     the path that the source tree was unpacked to as `LLVM_SRC`. Only the main
     LLVM source tree is required, but you can optionally add other components
     such as the Clang compiler or Polly loop optimiser. See the [LLVM releases](https://github.com/llvm/llvm-project/releases/tag/llvmorg-15.0.7)
     page for the complete list.

  2. Create a temporary build directory and `cd` into it, for example:
     ```sh
     $ mkdir /tmp/build
     $ cd /tmp/build
     ```

  3. Execute the following to configure the build. Here `INSTALL_PREFIX` is
     where LLVM is to be installed, for example `/usr/local` or
     `$HOME/opt/llvm`:
     ```sh
     $ cmake $LLVM_SRC -DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_ASSERTIONS=ON -DLLVM_BUILD_LLVM_DYLIB=ON -DLLVM_LINK_LLVM_DYLIB=ON
     ```
     See [options and variables](http://llvm.org/docs/CMake.html#options-and-variables)
     for a list of additional build parameters you can specify.

  4. Build and install:
     ```sh
     $ cmake --build .
     $ cmake --build . --target install
     ```

  5. For macOS only, some additional steps are useful to work around issues related
     to [System Integrity Protection](https://en.wikipedia.org/wiki/System_Integrity_Protection):
     ```sh
     cd $INSTALL_PREFIX/lib
     ln -s libLLVM.dylib libLLVM-15.dylib
     install_name_tool -id $PWD/libLTO.dylib libLTO.dylib
     install_name_tool -id $PWD/libLLVM.dylib libLLVM.dylib
     install_name_tool -change '@rpath/libLLVM.dylib' $PWD/libLLVM.dylib libLTO.dylib
     ```
