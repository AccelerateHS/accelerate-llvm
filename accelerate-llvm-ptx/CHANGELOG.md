# Change Log

Notable changes to the project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/) and the
project adheres to the [Haskell Package Versioning
Policy (PVP)](https://pvp.haskell.org)

## [unreleased]
### Added
 * support for GHC-8.2
 * caching of compilation results ([accelerate-llvm#17])

### Changed
 * generalise `run1*` to polyvariadic `runN*`

### Removed
 * ...

### Fixed
 * Fixed synchronisation bug in multidimensional reduction
 
### Deprecated
 * ...


## [1.0.0.1] - 2017-05-25
### Fixed
  * [#386] (partial fix)

## [1.0.0.0] - 2017-03-31
  * initial release


[unreleased]:           https://github.com/AccelerateHS/accelerate-llvm/compare/1.0.0.0...HEAD
[1.0.0.1]:              https://github.com/AccelerateHS/accelerate-llvm/compare/1.0.0.0...1.0.0.1
[1.0.0.0]:              https://github.com/AccelerateHS/accelerate-llvm/compare/be7f91295f77434b2103c70aa1cabb6a4f2b09a8...1.0.0.0

[#386]:                 https://github.com/AccelerateHS/accelerate/issues/386

[accelerate-llvm#17]:   https://github.com/AccelerateHS/accelerate-llvm/issues/17

