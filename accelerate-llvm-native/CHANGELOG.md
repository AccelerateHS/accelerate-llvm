# Change Log

Notable changes to the project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/) and the
project adheres to the [Haskell Package Versioning
Policy (PVP)](https://pvp.haskell.org)

## [unreleased]
### Added
 * support for GHC-8.2
 * caching of compilation results ([accelerate-llvm#17])
 * new runtime linker; this fixes the annoying "forkOS_entry: interrupted" error. Note that currently this only supports x86_64 macOS and linux.

### Changed
 * generalise `run1*` to polyvariadic `runN*`

### Removed
 * ...

### Fixed
 * ...
 
### Deprecated
 * ...


## [1.0.0.0] - 2017-03-31
  * initial release


[unreleased]:           https://github.com/AccelerateHS/accelerate-llvm/compare/1.0.0.0...HEAD
[1.0.0.0]:              https://github.com/AccelerateHS/accelerate-llvm/compare/be7f91295f77434b2103c70aa1cabb6a4f2b09a8...1.0.0.0

[accelerate-llvm#17]:   https://github.com/AccelerateHS/accelerate-llvm/issues/17

