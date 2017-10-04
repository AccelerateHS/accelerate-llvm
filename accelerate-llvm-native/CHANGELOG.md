# Change Log

Notable changes to the project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/) and the
project adheres to the [Haskell Package Versioning
Policy (PVP)](https://pvp.haskell.org)

## [1.1.0.1] - 2017-10-04
### Fixed
 * fix for `runQ*` generating multiple declarations with the same name

## [1.1.0.0] - 2017-09-21
### Added
 * support for GHC-8.2
 * caching of compilation results ([accelerate-llvm#17])
 * new runtime linker; this fixes the annoying "forkOS_entry: interrupted" error. Note that currently this only supports x86_64 macOS and linux
 * support for ahead-of-time compilation (`runQ` and `runQAsync`)

### Changed
 * generalise `run1*` to polyvariadic `runN*`
 * programs run using all cores by default; the environment variable
   `ACCELERATE_LLVM_NATIVE_THREADS` is used to set the number of worker threads
   rather than `+RTS -N`


## [1.0.0.0] - 2017-03-31
  * initial release


[1.1.0.1]:              https://github.com/AccelerateHS/accelerate-llvm/compare/1.1.0.0...1.1.0.1-native
[1.1.0.0]:              https://github.com/AccelerateHS/accelerate-llvm/compare/1.0.0.0...1.1.0.0
[1.0.0.0]:              https://github.com/AccelerateHS/accelerate-llvm/compare/be7f91295f77434b2103c70aa1cabb6a4f2b09a8...1.0.0.0

[accelerate-llvm#17]:   https://github.com/AccelerateHS/accelerate-llvm/issues/17

