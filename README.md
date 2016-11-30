An LLVM backend for the Accelerate Array Language
=================================================

[![Build Status](https://travis-ci.org/AccelerateHS/accelerate-llvm.svg)](https://travis-ci.org/AccelerateHS/accelerate-llvm)

This package compiles Accelerate code to LLVM IR, and executes that code on
multicore CPUs as well as NVIDIA GPUs. This avoids the need to go through `nvcc`
or `clang`. For details on Accelerate, refer to the [main repository][GitHub].
Please also file bug reports and feature requests with the [issue
tracker][Issues] of the main repository.

  [GitHub]:  https://github.com/AccelerateHS/accelerate
  [Issues]:  https://github.com/AccelerateHS/accelerate/issues

Dependencies
------------

Haskell dependencies are available from Hackage. There are several external
dependencies that you will need to install as well:

 * [libFFI](http://sourceware.org/libffi/)
 * [LLVM](http://llvm.org)
 * [CUDA](https://developer.nvidia.com/cuda-downloads) (if using the `accelerate-llvm-ptx` backend)


Installation
------------

You will need to install a couple of foreign libraries: libffi as well as LLVM
__with shared library support__. If you want to use the GPU targeting
`accelerate-llvm-ptx` backend, make sure you install (or build) LLVM with the
'nvptx' target.

Example using [Homebrew](http://brew.sh) on Mac OS X:

```sh
$ brew update
$ brew install libffi
$ brew install homebrew/versions/llvm35 --all-targets
```

Then, installation using
[`stack`](http://docs.haskellstack.org/en/stable/README.html) just requires you
to point it to the appropriate configuration file, for example:

```sh
$ ln -s stack-7.10.yaml stack.yaml
$ stack setup
$ stack install
```

If installing via `cabal`, note that you will need to tell the `llvm-general`
package to use the shared library version of LLVM ([#84][llvm-general-issue84],
[#85][llvm-general-issue85]) before attempting to install `accelerate-llvm`.

```sh
$ cabal install llvm-general -fshared-llvm
$ cabal install accelerate-llvm
```

Finally, it is possible to use libNVVM to optimise the generated GPU code,
rather than LLVM's inbuilt NVPTX backend (so, you will not need to install LLVM
with the NVPTX target). The libNVVM library is distributed as part of the NVIDIA
CUDA toolkit.

Using `stack`, either edit the `stack.yaml` file, install via:

```sh
$ stack install accelerate-llvm-ptx --flag accelerate-llvm-ptx:nvvm
```

If installing via `cabal`:

```sh
$ cabal install accelerate-llvm-ptx -fnvvm
```

 [llvm-general-issue84]:        https://github.com/bscarlet/llvm-general/issues/84
 [llvm-general-issue85]:        https://github.com/bscarlet/llvm-general/issues/85

