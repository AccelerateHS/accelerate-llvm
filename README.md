An LLVM backend for the Accelerate Array Language
=================================================

This package compiles Accelerate code to LLVM IR, and executes that code on
multicore CPUs as well as NVIDIA GPUs. For details on Accelerate, refer to the
[main repository][GitHub]. Please also file bug reports and feature requests
with the [issue tracker][Issues] of the main repository.

  [GitHub]:  https://github.com/AccelerateHS/accelerate
  [Issues]:  https://github.com/AccelerateHS/accelerate/issues


TODO
====

Code generation
---------------

  * Implement a wrapper over llvm-general-pure so that (at least) Operands are
    typed. The llvm-tf package may provide useful inspiration. With this, we
    could implement a completely typed compilation and execution pipeline
    (yay!). Moreover, LLVM IR is typed, but we only get type errors at
    Accelerate compilation time, which corresponds to Haskell runtime ):

Native backend
--------------

  * Complete coverage of skeleton operations

NVPTX backend
-------------

  * Complete coverage of skeleton operations

