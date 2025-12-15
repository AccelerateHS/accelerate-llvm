{- |
Module      : Text.LLVM.Triple.Print
Description : Printing of LLVM target triples.
License     : BSD3
Maintainer  : Langston Barrett
Stability   : experimental
-}

{-# LANGUAGE LambdaCase #-}

module Text.LLVM.Triple.Print
  ( archName
  , vendorName
  , osName
  , envName
  , objFmtName
  , printTriple
  ) where

import qualified Data.List as List

import Text.LLVM.Triple.AST

-- | @llvm::Triple::getArchTypeName@.
--
-- Retained in the order in which they appear in the LLVM source, rather than an
-- order consistent with the constructors of 'Arch'.
archName :: Arch -> String
archName =
  \case
    UnknownArch -> "unknown"
    AArch64 -> "aarch64"
    AArch64_32 -> "aarch64_32"
    AArch64_BE -> "aarch64_be"
    AMDGCN -> "amdgcn"
    AMDIL64 -> "amdil64"
    AMDIL -> "amdil"
    ARC -> "arc"
    ARM -> "arm"
    ARMEB -> "armeb"
    AVR -> "avr"
    BPFEB -> "bpfeb"
    BPFEL -> "bpfel"
    CSKY -> "csky"
    DXIL -> "dxil"
    Hexagon -> "hexagon"
    HSAIL64 -> "hsail64"
    HSAIL -> "hsail"
    Kalimba -> "kalimba"
    Lanai -> "lanai"
    Le32 -> "le32"
    Le64 -> "le64"
    LoongArch32 -> "loongarch32"
    LoongArch64 -> "loongarch64"
    M68k -> "m68k"
    MIPS64 -> "mips64"
    MIPS64EL -> "mips64el"
    MIPS -> "mips"
    MIPSEL -> "mipsel"
    MSP430 -> "msp430"
    NVPTX64 -> "nvptx64"
    NVPTX -> "nvptx"
    PPC64 -> "powerpc64"
    PPC64LE -> "powerpc64le"
    PPC -> "powerpc"
    PPCLE -> "powerpcle"
    R600 -> "r600"
    RenderScript32 -> "renderscript32"
    RenderScript64 -> "renderscript64"
    RISCV32 -> "riscv32"
    RISCV64 -> "riscv64"
    SHAVE -> "shave"
    Sparc -> "sparc"
    SparcEL -> "sparcel"
    Sparcv9 -> "sparcv9"
    SPIR64 -> "spir64"
    SPIR -> "spir"
    SPIRV32 -> "spirv32"
    SPIRV64 -> "spirv64"
    SystemZ -> "s390x"
    TCE -> "tce"
    TCELE -> "tcele"
    Thumb -> "thumb"
    ThumbEB -> "thumbeb"
    VE -> "ve"
    Wasm32 -> "wasm32"
    Wasm64 -> "wasm64"
    X86 -> "i386"
    X86_64 -> "x86_64"
    XCore -> "xcore"

-- | @llvm::Triple::getVendorTypeName@.
--
-- Retained in the order in which they appear in the LLVM source.
vendorName :: Vendor -> String
vendorName =
  \case
    UnknownVendor -> "unknown"
    AMD -> "amd"
    Apple -> "apple"
    CSR -> "csr"
    Freescale -> "fsl"
    IBM -> "ibm"
    ImaginationTechnologies -> "img"
    Mesa -> "mesa"
    MipsTechnologies -> "mti"
    Myriad -> "myriad"
    NVIDIA -> "nvidia"
    OpenEmbedded -> "oe"
    PC -> "pc"
    SCEI -> "scei"
    SUSE -> "suse"

-- | @llvm::Triple::getOSTypeName@.
--
-- Retained in the order in which they appear in the LLVM source.
osName :: OS -> String
osName =
  \case
    UnknownOS -> "unknown"
    AIX -> "aix"
    AMDHSA -> "amdhsa"
    AMDPAL -> "amdpal"
    Ananas -> "ananas"
    CUDA -> "cuda"
    CloudABI -> "cloudabi"
    Contiki -> "contiki"
    Darwin -> "darwin"
    DragonFly -> "dragonfly"
    DriverKit -> "driverkit"
    ELFIAMCU -> "elfiamcu"
    Emscripten -> "emscripten"
    FreeBSD -> "freebsd"
    Fuchsia -> "fuchsia"
    Haiku -> "haiku"
    HermitCore -> "hermit"
    Hurd -> "hurd"
    IOS -> "ios"
    KFreeBSD -> "kfreebsd"
    Linux -> "linux"
    Lv2 -> "lv2"
    MacOSX -> "macosx"
    Mesa3D -> "mesa3d"
    Minix -> "minix"
    NVCL -> "nvcl"
    NaCl -> "nacl"
    NetBSD -> "netbsd"
    OpenBSD -> "openbsd"
    PS4 -> "ps4"
    PS5 -> "ps5"
    RTEMS -> "rtems"
    Solaris -> "solaris"
    TvOS -> "tvos"
    WASI -> "wasi"
    WatchOS -> "watchos"
    Win32 -> "windows"
    ZOS -> "zos"
    ShaderModel -> "shadermodel"

-- | @llvm::Triple::getEnvironmentName@.
--
-- Retained in the order in which they appear in the LLVM source.
envName :: Environment -> String
envName =
  \case
  UnknownEnvironment -> "unknown"
  Android -> "android"
  CODE16 -> "code16"
  CoreCLR -> "coreclr"
  Cygnus -> "cygnus"
  EABI -> "eabi"
  EABIHF -> "eabihf"
  GNU -> "gnu"
  GNUABI64 -> "gnuabi64"
  GNUABIN32 -> "gnuabin32"
  GNUEABI -> "gnueabi"
  GNUEABIHF -> "gnueabihf"
  GNUX32 -> "gnux32"
  GNUILP32 -> "gnu_ilp32"
  Itanium -> "itanium"
  MSVC -> "msvc"
  MacABI -> "macabi"
  Musl -> "musl"
  MuslEABI -> "musleabi"
  MuslEABIHF -> "musleabihf"
  MuslX32 -> "muslx32"
  Simulator -> "simulator"
  Pixel -> "pixel"
  Vertex -> "vertex"
  Geometry -> "geometry"
  Hull -> "hull"
  Domain -> "domain"
  Compute -> "compute"
  Library -> "library"
  RayGeneration -> "raygeneration"
  Intersection -> "intersection"
  AnyHit -> "anyhit"
  ClosestHit -> "closesthit"
  Miss -> "miss"
  Callable -> "callable"
  Mesh -> "mesh"
  Amplification -> "amplification"

-- | @llvm::Triple::getObjectFormatTypeName@
--
-- Retained in the order in which they appear in the LLVM source.
objFmtName :: ObjectFormat -> String
objFmtName =
  \case
    UnknownObjectFormat -> ""  -- NB: Not "unknown"
    COFF -> "coff"
    ELF -> "elf"
    GOFF -> "goff"
    MachO -> "macho"
    Wasm -> "wasm"
    XCOFF -> "xcoff"
    DXContainer -> "dxcontainer"
    SPIRV -> "spirv"

printTriple :: TargetTriple -> String
printTriple tt =
  List.intercalate
    "-"
    [ archName (ttArch tt)
    , vendorName (ttVendor tt)
    , osName (ttOS tt)
    , envName (ttEnv tt)
    , objFmtName (ttObjFmt tt)
    ]
