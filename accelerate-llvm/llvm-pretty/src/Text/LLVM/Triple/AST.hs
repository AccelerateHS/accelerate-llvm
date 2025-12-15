{- |
Module      : Text.LLVM.Triple.AST
Description : AST of LLVM target triples.
License     : BSD3
Maintainer  : Langston Barrett
Stability   : experimental
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Text.LLVM.Triple.AST
  ( Arch(..)
  , SubArch(..)
  , Vendor(..)
  , OS(..)
  , Environment(..)
  , ObjectFormat(..)
  , TargetTriple(..)
  ) where

import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- | The constructors of this type exactly mirror the LLVM @enum ArchType@,
-- including inconsistent labeling of endianness. Capitalization is taken from
-- the LLVM comments, which are reproduced below.
--
-- Last updated: LLVM 15.0.1:
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/include/llvm/ADT/Triple.h#L46
data Arch
  = UnknownArch
    -- | ARM (little endian): arm armv.* xscale
  | ARM
    -- | ARM (big endian): armeb
  | ARMEB
    -- | AArch64 (little endian): aarch64
  | AArch64
    -- | AArch64 (big endian): aarch64_be
  | AArch64_BE
    -- | AArch64 (little endian) ILP32: aarch64_32
  | AArch64_32
    -- | ARC: Synopsys ARC
  | ARC
    -- | AVR: Atmel AVR microcontroller
  | AVR
    -- | eBPF or extended BPF or 64-bit BPF (little endian)
  | BPFEL
    -- | eBPF or extended BPF or 64-bit BPF (big endian)
  | BPFEB
    -- | CSKY: csky
  | CSKY
    -- | DXIL 32-bit DirectX bytecode
  | DXIL
    -- | Hexagon: hexagon
  | Hexagon
    -- | LoongArch (32-bit): loongarch32
  | LoongArch32
    -- | LoongArch (64-bit): loongarch64
  | LoongArch64
    -- | M68k: Motorola 680x0 family
  | M68k
    -- | MIPS: mips mipsallegrex mipsr6
  | MIPS
    -- | MIPSEL: mipsel mipsallegrexe mipsr6el
  | MIPSEL
    -- | MIPS64: mips64 mips64r6 mipsn32 mipsn32r6
  | MIPS64
    -- | MIPS64EL: mips64el mips64r6el mipsn32el mipsn32r6el
  | MIPS64EL
    -- | MSP430: msp430
  | MSP430
    -- | PPC: powerpc
  | PPC
    -- | PPCLE: powerpc (little endian)
  | PPCLE
    -- | PPC64: powerpc64 ppu
  | PPC64
    -- | PPC64LE: powerpc64le
  | PPC64LE
    -- | R600: AMD GPUs HD2XXX - HD6XXX
  | R600
    -- | AMDGCN: AMD GCN GPUs
  | AMDGCN
    -- | RISC-V (32-bit): riscv32
  | RISCV32
    -- | RISC-V (64-bit): riscv64
  | RISCV64
    -- | Sparc: sparc
  | Sparc
    -- | Sparcv9: Sparcv9
  | Sparcv9
    -- | Sparc: (endianness = little). NB: 'Sparcle' is a CPU variant
  | SparcEL
    -- | SystemZ: s390x
  | SystemZ
    -- | TCE (http://tce.cs.tut.fi/): tce
  | TCE
    -- | TCE little endian (http://tce.cs.tut.fi/): tcele
  | TCELE
    -- | Thumb (little endian): thumb thumbv.*
  | Thumb
    -- | Thumb (big endian): thumbeb
  | ThumbEB
    -- | X86: i[3-9]86
  | X86
    -- | X86-64: amd64 x86_64
  | X86_64
    -- | XCore: xcore
  | XCore
    -- | NVPTX: 32-bit
  | NVPTX
    -- | NVPTX: 64-bit
  | NVPTX64
    -- | le32: generic little-endian 32-bit CPU (PNaCl)
  | Le32
    -- | le64: generic little-endian 64-bit CPU (PNaCl)
  | Le64
    -- | AMDIL
  | AMDIL
    -- | AMDIL with 64-bit pointers
  | AMDIL64
    -- | AMD HSAIL
  | HSAIL
    -- | AMD HSAIL with 64-bit pointers
  | HSAIL64
    -- | SPIR: standard portable IR for OpenCL 32-bit version
  | SPIR
    -- | SPIR: standard portable IR for OpenCL 64-bit version
  | SPIR64
    -- | SPIR-V with 32-bit pointers
  | SPIRV32
    -- | SPIR-V with 64-bit pointers
  | SPIRV64
    -- | Kalimba: generic kalimba
  | Kalimba
    -- | SHAVE: Movidius vector VLIW processors
  | SHAVE
    -- | Lanai: Lanai 32-bit
  | Lanai
    -- | WebAssembly with 32-bit pointers
  | Wasm32
    -- | WebAssembly with 64-bit pointers
  | Wasm64
    -- | 32-bit RenderScript
  | RenderScript32 -- 32-bit RenderScript
    -- | 64-bit RenderScript
  | RenderScript64
    -- | NEC SX-Aurora Vector Engine
  | VE
  deriving (Bounded, Data, Eq, Enum, Generic, Ord, Read, Show, Typeable)

-- | A 'First'-like semigroup instance that simply drops the RHS, unless the LHS
-- is 'UnknownArch'.
instance Semigroup Arch where
  a <> UnknownArch{} = a
  UnknownArch{} <> b = b
  a <> _ = a

-- | @'mempty' == 'UnknownArch'@
instance Monoid Arch where
  mempty = UnknownArch

-- | The constructors of this type exactly mirror the LLVM @enum SubArchType@.
--
-- Last updated: LLVM 15.0.1:
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/include/llvm/ADT/Triple.h#L110
data SubArch
  = NoSubArch
  | ARMSubArch_v9_3a
  | ARMSubArch_v9_2a
  | ARMSubArch_v9_1a
  | ARMSubArch_v9
  | ARMSubArch_v8_8a
  | ARMSubArch_v8_7a
  | ARMSubArch_v8_6a
  | ARMSubArch_v8_5a
  | ARMSubArch_v8_4a
  | ARMSubArch_v8_3a
  | ARMSubArch_v8_2a
  | ARMSubArch_v8_1a
  | ARMSubArch_v8
  | ARMSubArch_v8r
  | ARMSubArch_v8m_baseline
  | ARMSubArch_v8m_mainline
  | ARMSubArch_v8_1m_mainline
  | ARMSubArch_v7
  | ARMSubArch_v7em
  | ARMSubArch_v7m
  | ARMSubArch_v7s
  | ARMSubArch_v7k
  | ARMSubArch_v7ve
  | ARMSubArch_v6
  | ARMSubArch_v6m
  | ARMSubArch_v6k
  | ARMSubArch_v6t2
  | ARMSubArch_v5
  | ARMSubArch_v5te
  | ARMSubArch_v4t

  | AArch64SubArch_arm64e

  | KalimbaSubArch_v3
  | KalimbaSubArch_v4
  | KalimbaSubArch_v5

  | MipsSubArch_r6

  | PPCSubArch_spe

  | SPIRVSubArch_v10
  | SPIRVSubArch_v11
  | SPIRVSubArch_v12
  | SPIRVSubArch_v13
  | SPIRVSubArch_v14
  | SPIRVSubArch_v15
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

-- | A 'First'-like semigroup instance that simply drops the RHS, unless the LHS
-- is 'NoSubArch'.
instance Semigroup SubArch where
  a <> NoSubArch{} = a
  NoSubArch{} <> b = b
  a <> _ = a

-- | @'mempty' == 'NoSubArch'@
instance Monoid SubArch where
  mempty = NoSubArch

-- | The constructors of this type exactly mirror the LLVM @enum VendorType@,
-- including ordering and grouping.
--
-- Last updated: LLVM 15.0.1:
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/include/llvm/ADT/Triple.h#L162
data Vendor
  = UnknownVendor
  | Apple
  | PC
  | SCEI
  | Freescale
  | IBM
  | ImaginationTechnologies
  | MipsTechnologies
  | NVIDIA
  | CSR
  | Myriad
  | AMD
  | Mesa
  | SUSE
  | OpenEmbedded
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

-- | A 'First'-like semigroup instance that simply drops the RHS, unless the LHS
-- is 'UnknownVendor'.
instance Semigroup Vendor where
  a <> UnknownVendor{} = a
  UnknownVendor{} <> b = b
  a <> _ = a

-- | @'mempty' == 'UnknownVendor'@
instance Monoid Vendor where
  mempty = UnknownVendor

-- | The constructors of this type exactly mirror the LLVM @enum OSType@,
-- including ordering and grouping.
--
-- End-of-line comments from LLVM are reproduced as constructor comments.
--
-- Last updated: LLVM 15.0.1:
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/include/llvm/ADT/Triple.h#L181
data OS
  = UnknownOS
  | Ananas
  | CloudABI
  | Darwin
  | DragonFly
  | FreeBSD
  | Fuchsia
  | IOS
  | KFreeBSD
  | Linux
    -- | PS3
  | Lv2
  | MacOSX
  | NetBSD
  | OpenBSD
  | Solaris
  | Win32
  | ZOS
  | Haiku
  | Minix
  | RTEMS
    -- | Native Client
  | NaCl
  | AIX
    -- | NVIDIA CUDA
  | CUDA
    -- | NVIDIA OpenCL
  | NVCL
    -- | AMD HSA Runtime
  | AMDHSA
  | PS4
  | PS5
  | ELFIAMCU
    -- | Apple tvOS
  | TvOS
    -- | Apple watchOS
  | WatchOS
    -- | Apple DriverKit
  | DriverKit
  | Mesa3D
  | Contiki
    -- | AMD PAL Runtime
  | AMDPAL
    -- | HermitCore Unikernel/Multikernel
  | HermitCore
    -- | GNU/Hurd
  | Hurd
    -- | Experimental WebAssembly OS
  | WASI
  | Emscripten
    -- | DirectX ShaderModel
  | ShaderModel
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

-- | A 'First'-like semigroup instance that simply drops the RHS, unless the LHS
-- is 'UnknownOS'.
instance Semigroup OS where
  a <> UnknownOS{} = a
  UnknownOS{} <> b = b
  a <> _ = a

-- | @'mempty' == 'UnknownOS'@
instance Monoid OS where
  mempty = UnknownOS

-- | The constructors of this type exactly mirror the LLVM @enum
-- EnvironmentType@, including ordering and grouping.
--
-- End-of-line comments from LLVM are reproduced as constructor comments.
--
-- Last updated: LLVM 15.0.1:
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/include/llvm/ADT/Triple.h#L224
data Environment
  = UnknownEnvironment

  | GNU
  | GNUABIN32
  | GNUABI64
  | GNUEABI
  | GNUEABIHF
  | GNUX32
  | GNUILP32
  | CODE16
  | EABI
  | EABIHF
  | Android
  | Musl
  | MuslEABI
  | MuslEABIHF
  | MuslX32

  | MSVC
  | Itanium
  | Cygnus
  | CoreCLR
    -- | Simulator variants of other systems e.g. Apple's iOS
  | Simulator
    -- | Mac Catalyst variant of Apple's iOS deployment target.
  | MacABI

  | Pixel
  | Vertex
  | Geometry
  | Hull
  | Domain
  | Compute
  | Library
  | RayGeneration
  | Intersection
  | AnyHit
  | ClosestHit
  | Miss
  | Callable
  | Mesh
  | Amplification
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

-- | A 'First'-like semigroup instance that simply drops the RHS, unless the LHS
-- is 'UnknownEnvironment'.
instance Semigroup Environment where
  a <> UnknownEnvironment{} = a
  UnknownEnvironment{} <> b = b
  a <> _ = a

-- | @'mempty' == 'UnknownEnvironment'@
instance Monoid Environment where
  mempty = UnknownEnvironment

-- | The constructors of this type exactly mirror the LLVM @enum
-- ObjectFormatType@, including ordering.
--
-- Last updated: LLVM 15.0.1:
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/include/llvm/ADT/Triple.h#L269
data ObjectFormat
  = UnknownObjectFormat
  | COFF
  | DXContainer
  | ELF
  | GOFF
  | MachO
  | SPIRV
  | Wasm
  | XCOFF
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show, Typeable)

-- | A 'First'-like semigroup instance that simply drops the RHS, unless the LHS
-- is 'UnknownObjectFormat'.
instance Semigroup ObjectFormat where
  a <> UnknownObjectFormat{} = a
  UnknownObjectFormat{} <> b = b
  a <> _ = a

-- | @'mempty' == 'UnknownObjectFormat'@
instance Monoid ObjectFormat where
  mempty = UnknownObjectFormat

-- | More like a sextuple than a triple, if you think about it.
--
-- The LLVM version of this holds onto the un-normalized string representation.
-- We discard it.
data TargetTriple
  = TargetTriple
    { ttArch :: Arch
    , ttSubArch :: SubArch
    , ttVendor :: Vendor
    , ttOS :: OS
    , ttEnv :: Environment
    , ttObjFmt :: ObjectFormat
    }
  deriving (Bounded, Data, Eq, Generic, Ord, Read, Show, Typeable)

-- | Combines fields pointwise.
instance Semigroup TargetTriple where
  tt <> tt' =
    TargetTriple
    { ttArch = ttArch tt <> ttArch tt'
    , ttSubArch = ttSubArch tt <> ttSubArch tt'
    , ttVendor = ttVendor tt <> ttVendor tt'
    , ttOS = ttOS tt <> ttOS tt'
    , ttEnv = ttEnv tt <> ttEnv tt'
    , ttObjFmt = ttObjFmt tt <> ttObjFmt tt'
    }

-- | Pointwise 'mempty'.
instance Monoid TargetTriple where
  mempty =
    TargetTriple
    { ttArch = mempty
    , ttSubArch = mempty
    , ttVendor = mempty
    , ttOS = mempty
    , ttEnv = mempty
    , ttObjFmt = mempty
    }
