# Change Log

Notable changes to the project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/) and the
project adheres to the [Haskell Package Versioning
Policy (PVP)](https://pvp.haskell.org)

## [1.2.0.0] - 2018-04-03
### Added
 * support for half-precision floats
 * support for struct-of-array-of-struct representations
 * support for LLVM-6.0
 * support for GHC-8.4

### Fixed
 * Fix for 32-bit `IsNan` and `IsInfinite` ([#407])

### Contributors

Special thanks to those who contributed patches as part of this release:

 * Trevor L. McDonell ([@tmcdonell][tmcdonell])
 * Ryan Scott ([@ryanglscott][ryanglscott])
 * Moritz Kiefer ([@cocreature][cocreature])


## [1.1.0.0] - 2017-09-21
### Added
 * support for GHC-8.2
 * support for LLVM-5.0

### Changed
 * internal restructuring of compile/link phases


## [1.0.0.0] - 2017-03-31
 * initial release


[1.2.0.0]:    https://github.com/AccelerateHS/accelerate-llvm/compare/1.1.0.0...1.2.0.0
[1.1.0.0]:    https://github.com/AccelerateHS/accelerate-llvm/compare/1.0.0.0...1.1.0.0
[1.0.0.0]:    https://github.com/AccelerateHS/accelerate-llvm/compare/be7f91295f77434b2103c70aa1cabb6a4f2b09a8...1.0.0.0

[#407]:       https://github.com/AccelerateHS/accelerate/issues/407

