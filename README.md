An LLVM backend for the Accelerate Array Language
=================================================

This package compiles Accelerate code to LLVM IR, and executes that code on
multicore CPUs as well as NVIDIA GPUs. This avoids the need to go through 'nvcc'
or 'clang'. For details on Accelerate, refer to the [main repository][GitHub].
Please also file bug reports and feature requests with the [issue
tracker][Issues] of the main repository.

  [GitHub]:  https://github.com/AccelerateHS/accelerate
  [Issues]:  https://github.com/AccelerateHS/accelerate/issues

Dependencies
------------

Haskell dependencies are available from Hackage. There are several external
dependencies that you will need to install as well:

 * [LLVM](http://llvm.org)
 * [libFFI](http://sourceware.org/libffi/)


Installation
------------

You will need to install a couple of foreign libraries: libffi as well as LLVM.
When installing LLVM, make sure to install the 'nvptx' target as well, not just
the native target.

Example using [Homebrew](http://brew.sh) on Mac OS X:

```
$ brew update
$ brew install libffi
$ brew install llvm34 --all-targets
```

If you are using GHC-7.8, in order to be able to use LLVM from within GHCi, you
will need to tell the `llvm-general` package to use the shared library version
of LLVM ([84][llvm-general-issue84], [85][llvm-general-issue85]):

```
$ cabal install llvm-general -fshared-llvm
```

Finally, it is possible to use libNVVM to optimise the generated GPU code,
rather than LLVM's inbuilt NVPTX backend (so, you will not need to install LLVM
with the nvptx target). However, this will require an older version of LLVM,
which may impact CPU performance. If you wish to use libNVVM, supply the flag
`-flibnvvm` to cabal when installing `accelerate-llvm`.


TODOs
-----

These are some TODOs and notes that pop into my head from time to time...

**Native backend**

  * Complete coverage of skeleton operations
- [ ] map
- [ ] generate/zipWith
- [ ] backpermute/transform
- [ ] slice/replicate
- [ ] reshape
- [ ] fold (multidimensional)
- [ ] foldAll (parallel optimisation)
- [ ] scanl, scanr
- [ ] scanl1, scanr1
- [ ] scanl', scanr'
- [ ] foldSeg
- [ ] permute
- [ ] stencil
- [ ] stencil2
- [ ] foreign functions


**PTX backend**

  * Complete coverage of skeleton operations
- [ ] map
- [ ] generate/zipWith
- [ ] backpermute/transform
- [ ] slice/replicate
- [ ] reshape
- [ ] fold (multidimensional)
- [ ] foldAll (parallel optimisation)
- [ ] scanl, scanr
- [ ] scanl1, scanr1
- [ ] scanl', scanr'
- [ ] foldSeg
- [ ] permute
- [ ] stencil
- [ ] stencil2
- [ ] foreign functions


 [llvm-general-issue84]:        https://github.com/bscarlet/llvm-general/issues/84
 [llvm-general-issue85]:        https://github.com/bscarlet/llvm-general/issues/85
 [llvm-general-issue101]:       https://github.com/bscarlet/llvm-general/pull/101

