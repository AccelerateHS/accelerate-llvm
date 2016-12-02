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

For Debian/Ubuntu based Linux distributions, the LLVM.org website provides
binary distribution packages. Check [apt.llvm.org](http://apt.llvm.org) for
instructions for adding the correct package database for your OS version, and
then:

```sh
$ apt-get install libedit-dev llvm-3.5-dev
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

Note that the version of
[`llvm-general`](https://hackage.haskell.org/package/llvm-general) used must
match the installed version of LLVM. The currently released version of
`llvm-general` is for LLVM-3.5, but releases for
[3.8](https://github.com/bscarlet/llvm-general/tree/llvm-3.8) and
[3.9](https://github.com/bscarlet/llvm-general/tree/llvm-3.9) should be
available soon.


### libNVVM

The `accelerate-llvm-ptx` backend can optionally be compiled to generate GPU
code using the libNVVM library, rather than LLVM's inbuilt NVPTX code generator.
libNVVM is a closed-source library distributed as part of the NVIDIA CUDA
toolkit, and is what the `nvcc` compiler itself uses internally when compiling
CUDA C code.

Using libNVVM may improve GPU performance as it includes several optimisations
not present in NVPTX. One difficult with using it however is that since libNVVM
is also based on LLVM, and typically lags LLVM by several releases, you must
install `accelerate-llvm` with a "compatible" version of LLVM, which will
depend on the version of the CUDA toolkit you have installed.

|              | LLVM-3.3 | LLVM-3.4 | LLVM-3.5 | LLVM-3.8 | LLVM-3.9 |
|:------------:|:--------:|:--------:|:--------:|:--------:|:--------:|
| **CUDA-7.0** |     ⭕    |     ❌    |          |          |          |
| **CUDA-7.5** |          |     ⭕    |     ⭕    |     ❌    |          |
| **CUDA-8.0** |          |          |     ⭕    |     ⭕    |     ❌    |

Note that `accelerate-llvm` itself currently requires at least LLVM-3.5. There
are currently no releases of `llvm-general` planned for LLVM-3.6 or LLVM-3.7.

Using `stack`, either edit the `stack.yaml` and add the following section:

```yaml
flags:
  accelerate-llvm-ptx:
    nvvm: true
```

Or install using the following option on the command line:

```sh
$ stack install accelerate-llvm-ptx --flag accelerate-llvm-ptx:nvvm
```

If installing via `cabal`:

```sh
$ cabal install accelerate-llvm-ptx -fnvvm
```

 [llvm-general-issue84]:        https://github.com/bscarlet/llvm-general/issues/84
 [llvm-general-issue85]:        https://github.com/bscarlet/llvm-general/issues/85

