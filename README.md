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

 * [`LLVM`](http://llvm.org)
 * [`libFFI`](http://sourceware.org/libffi/) (if using the `accelerate-llvm-native` backend for multicore CPUs)
 * [`CUDA`](https://developer.nvidia.com/cuda-downloads) (if using the `accelerate-llvm-ptx` backend for NVIDIA GPUs)


Installation
------------

You will need to install a couple of foreign libraries: libffi as well as LLVM
__with__ the `libLLVM` shared library . If you want to use the GPU targeting
`accelerate-llvm-ptx` backend, make sure you install (or build) LLVM with the
'nvptx' target.

Example using [Homebrew](http://brew.sh) on Mac OS X:

```sh
$ brew install llvm-hs/homebrew-llvm/llvm-4.0
```

For Debian/Ubuntu based Linux distributions, the LLVM.org website provides
binary distribution packages. Check [apt.llvm.org](http://apt.llvm.org) for
instructions for adding the correct package database for your OS version, and
then:

```sh
$ apt-get install llvm-4.0-dev
```

Then, installation using
[`stack`](http://docs.haskellstack.org/en/stable/README.html) just requires you
to point it to the appropriate configuration file, for example:

```sh
$ ln -s stack-8.0.yaml stack.yaml
$ stack setup
$ stack install
```


Note that the version of
[`llvm-hs`](https://hackage.haskell.org/package/llvm-hs) used must match the
installed version of LLVM, which is currently 4.0.


### libNVVM

The `accelerate-llvm-ptx` backend can optionally be compiled to generate GPU
code using the libNVVM library, rather than LLVM's inbuilt NVPTX code generator.
libNVVM is a closed-source library distributed as part of the NVIDIA CUDA
toolkit, and is what the `nvcc` compiler itself uses internally when compiling
CUDA C code.

Using libNVVM _may_ improve GPU performance as it includes several optimisations
not present in NVPTX. One difficult with using it however is that since libNVVM
is also based on LLVM, and typically lags LLVM by several releases, you must
install `accelerate-llvm` with a "compatible" version of LLVM, which will
depend on the version of the CUDA toolkit you have installed.

|              | LLVM-3.3 | LLVM-3.4 | LLVM-3.5 | LLVM-3.8 | LLVM-3.9 | LLVM-4.0 |
|:------------:|:--------:|:--------:|:--------:|:--------:|:--------:|:--------:|
| **CUDA-7.0** |     ⭕    |     ❌    |          |          |          |          |
| **CUDA-7.5** |          |     ⭕    |     ⭕    |     ❌    |          |          |
| **CUDA-8.0** |          |          |     ⭕    |     ⭕    |     ❌    |     ❌    |

Where ⭕ = Works, and ❌ = Does not work.

Note that the above restrictions on CUDA and LLVM version exist _only_ if you
want to use the NVVM component. Otherwise, you should be free to use any
combination of CUDA and LLVM.

Also note that `accelerate-llvm` itself currently requires at least LLVM-3.5.

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

