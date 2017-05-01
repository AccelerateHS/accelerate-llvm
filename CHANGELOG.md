# Change Log

Notable changes to the project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/) and the
project adheres to the [Haskell Package Versioning
Policy (PVP)](https://pvp.haskell.org)

## [unreleased]
### Added
 * generalise `run1*` to polyvariadic `runN*`
 * caching of compilation results ([accelerate-llvm#17](https://github.com/AccelerateHS/accelerate-llvm/issues/17))
 * internal restructuring of compile/link phases
 * [native] new runtime linker; this fixes the annoying "forkOS_entry: interrupted" error. Note that currently this only supports x86_64 macOS and linux.

### Changed
 * [ptx] build phase compiles to SASS explicitly. This requires the 'ptxas' executable (part of the CUDA toolkit) to be on your PATH.

### Removed
 * ...

### Fixed
 * ...
 
### Deprecated
 * ...


## 1.0.0.0 - 2017-03-31
  * initial release


[unreleased]: https://github.com/AccelerateHS/accelerate-llvm/compare/1.0.0.0...HEAD

