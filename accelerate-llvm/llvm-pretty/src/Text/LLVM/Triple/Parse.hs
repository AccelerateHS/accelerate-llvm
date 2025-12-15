{- |
Module      : Text.LLVM.Triple.Parse
Description : Parsing of LLVM target triples.
License     : BSD3
Maintainer  : Langston Barrett
Stability   : experimental

The declarations appear in this module in the same order as in the LLVM source.
-}

{- Note [Implementation]

The very simplest parsing functions are implemented with the 'LookupTable'
structure. For anything more complex, we endeavor to closely mirror the
structure of LLVM's implementation. This will make the code more maintainable
when updating to newer versions of LLVM.

-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Text.LLVM.Triple.Parse
  ( parseArch
  , parseVendor
  , parseOS
  , parseEnv
  , parseObjFmt
  , parseSubArch
  , parseTriple
  ) where

import qualified Data.List as List

import qualified MonadLib as M
import qualified MonadLib.Monads as M

import Text.LLVM.Triple.AST
import qualified Text.LLVM.Triple.Print as Print
import Text.LLVM.Triple.Parse.LookupTable
import qualified Text.LLVM.Triple.Parse.ARM as ARM

-- | @llvm::parseArch@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L442
parseArch :: String -> Arch
parseArch s =
  let mArch =
        -- See Note [Implementation] for the reasoning behind the strange structure.
        --
        -- It would be easy to forget to add patterns here when adding a new
        -- constructor to Arch, but we have exhaustive print-then-parse
        -- roundtrip tests to mitigate this risk.
        if | cases ["i386", "i486", "i586", "i686"] -> X86
           | cases ["i786", "i886", "i986"] -> X86
           | cases ["amd64", "x86_64", "x86_64h"] -> X86_64
           | cases ["powerpc", "powerpcspe", "ppc", "ppc32"] -> PPC
           | cases ["powerpcle", "ppcle", "ppc32le"] -> PPCLE
           | cases ["powerpc64", "ppu", "ppc64"] -> PPC64
           | cases ["powerpc64le", "ppc64le"] -> PPC64LE
           | cases ["xscale"] -> ARM
           | cases ["xscaleeb"] -> ARMEB
           | cases ["aarch64"] -> AArch64
           | cases ["aarch64_be"] -> AArch64_BE
           | cases ["aarch64_32"] -> AArch64_32
           | cases ["arc"] -> ARC
           | cases ["arm64"] -> AArch64
           | cases ["arm64_32"] -> AArch64_32
           | cases ["arm64e"] -> AArch64
           | cases ["arm"] -> ARM
           | cases ["armeb"] -> ARMEB
           | cases ["thumb"] -> Thumb
           | cases ["thumbeb"] -> ThumbEB
           | cases ["avr"] -> AVR
           | cases ["m68k"] -> M68k
           | cases ["msp430"] -> MSP430
           | cases ["mips", "mipseb", "mipsallegrex", "mipsisa32r6"
                   , "mipsr6"] -> MIPS
           | cases ["mipsel", "mipsallegrexel", "mipsisa32r6el", "mipsr6el"] -> MIPSEL
           | cases ["mips64", "mips64eb", "mipsn32", "mipsisa64r6"
                   , "mips64r6", "mipsn32r6"] -> MIPS64
           | cases ["mips64el", "mipsn32el", "mipsisa64r6el", "mips64r6el"
                   , "mipsn32r6el"] -> MIPS64EL
           | cases ["r600"] -> R600
           | cases ["amdgcn"] -> AMDGCN
           | cases ["riscv32"] -> RISCV32
           | cases ["riscv64"] -> RISCV64
           | cases ["hexagon"] -> Hexagon
           | cases ["s390x", "systemz"] -> SystemZ
           | cases ["sparc"] -> Sparc
           | cases ["sparcel"] -> SparcEL
           | cases ["sparcv9", "sparc64"] -> Sparcv9
           | cases ["tce"] -> TCE
           | cases ["tcele"] -> TCELE
           | cases ["xcore"] -> XCore
           | cases ["nvptx"] -> NVPTX
           | cases ["nvptx64"] -> NVPTX64
           | cases ["le32"] -> Le32
           | cases ["le64"] -> Le64
           | cases ["amdil"] -> AMDIL
           | cases ["amdil64"] -> AMDIL64
           | cases ["hsail"] -> HSAIL
           | cases ["hsail64"] -> HSAIL64
           | cases ["spir"] -> SPIR
           | cases ["spir64"] -> SPIR64
           | cases ["spirv32", "spirv32v1.0", "spirv32v1.1", "spirv32v1.2"
                   , "spirv32v1.3", "spirv32v1.4", "spirv32v1.5"] -> SPIRV32
           | cases ["spirv64", "spirv64v1.0", "spirv64v1.1", "spirv64v1.2"
                   , "spirv64v1.3", "spirv64v1.4", "spirv64v1.5"] -> SPIRV64
           | archPfx Kalimba -> Kalimba
           | cases ["lanai"] -> Lanai
           | cases ["renderscript32"] -> RenderScript32
           | cases ["renderscript64"] -> RenderScript64
           | cases ["shave"] -> SHAVE
           | cases ["ve"] -> VE
           | cases ["wasm32"] -> Wasm32
           | cases ["wasm64"] -> Wasm64
           | cases ["csky"] -> CSKY
           | cases ["loongarch32"] -> LoongArch32
           | cases ["loongarch64"] -> LoongArch64
           | cases ["dxil"] -> DXIL
           | otherwise -> UnknownArch
  in case mArch of
        UnknownArch ->
          if | archPfx ARM || archPfx Thumb || archPfx AArch64 ->
                 ARM.parseARMArch (ARM.ArchName s)
             | "bpf" `List.isPrefixOf` s -> parseBPFArch s
             | otherwise -> UnknownArch
        arch -> arch
  where
    cases = any (== s)
    archPfx arch = Print.archName arch `List.isPrefixOf` s

    -- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L292
    parseBPFArch arch =
      if arch == "bpf"
      -- The way that LLVM parses the arch for BPF depends on the endianness of
      -- the host in this case, which feels deeply wrong. We don't do that, not
      -- least since we're not in IO. We default to little-endian instead.
      then BPFEL
      else if | arch == "bpf_be" || arch == "bpfeb" -> BPFEB
              | arch == "bpf_le" || arch == "bpfel" -> BPFEL
              | otherwise -> UnknownArch

-- | @llvm::parseVendor@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L529
parseVendor :: String -> Vendor
parseVendor = lookupWithDefault table UnknownVendor
  where table = enumTable Print.vendorName

-- | @llvm::parseOS@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L549
parseOS :: String -> OS
parseOS = lookupByPrefixWithDefault table UnknownOS
  where table = enumTable Print.osName

-- | @llvm::parseEnvironment@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L593
parseEnv :: String -> Environment
parseEnv = lookupByPrefixWithDefault table UnknownEnvironment
  where table = enumTable Print.envName

-- | @llvm::parseFormat@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L634
parseObjFmt :: String -> ObjectFormat
parseObjFmt = lookupBySuffixWithDefault table UnknownObjectFormat
  where table = enumTable Print.objFmtName

-- | @llvm::parseSubArch@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L648
parseSubArch :: String -> SubArch
parseSubArch subArchName =
  if | startsWith "mips" && (endsWith "r6el" || endsWith "r6") -> MipsSubArch_r6

     | subArchName == "powerpcspe" -> PPCSubArch_spe

     | subArchName == "arm64e" -> AArch64SubArch_arm64e

     | startsWith "arm64e" -> AArch64SubArch_arm64e

     | startsWith "spirv" ->
         if | endsWith "v1.0" -> SPIRVSubArch_v10
            | endsWith "v1.1" -> SPIRVSubArch_v11
            | endsWith "v1.2" -> SPIRVSubArch_v12
            | endsWith "v1.3" -> SPIRVSubArch_v13
            | endsWith "v1.4" -> SPIRVSubArch_v14
            | endsWith "v1.5" -> SPIRVSubArch_v15
            | otherwise -> NoSubArch
     | otherwise ->
         case ARM.parseArch <$> armSubArch of
           Nothing ->
             if | endsWith "kalimba3" -> KalimbaSubArch_v3
                | endsWith "kalimba4" -> KalimbaSubArch_v4
                | endsWith "kalimba5" -> KalimbaSubArch_v5
                | otherwise -> NoSubArch
           Just armArch ->
             if | armArch == ARM.ARMV4 -> NoSubArch
                | armArch == ARM.ARMV4T -> ARMSubArch_v4t
                | armArch == ARM.ARMV5T -> ARMSubArch_v5
                | armArch == ARM.ARMV5TE ||
                  armArch == ARM.IWMMXT ||
                  armArch == ARM.IWMMXT2 ||
                  armArch == ARM.XSCALE ||
                  armArch == ARM.ARMV5TEJ -> ARMSubArch_v5te
                | armArch == ARM.ARMV6 ->  ARMSubArch_v6
                | armArch == ARM.ARMV6K ||
                  armArch == ARM.ARMV6KZ -> ARMSubArch_v6k
                | armArch == ARM.ARMV6T2 ->  ARMSubArch_v6t2
                | armArch == ARM.ARMV6M ->  ARMSubArch_v6m
                | armArch == ARM.ARMV7A ||
                  armArch == ARM.ARMV7R -> ARMSubArch_v7
                | armArch == ARM.ARMV7VE -> ARMSubArch_v7ve
                | armArch == ARM.ARMV7K -> ARMSubArch_v7k
                | armArch == ARM.ARMV7M -> ARMSubArch_v7m
                | armArch == ARM.ARMV7S -> ARMSubArch_v7s
                | armArch == ARM.ARMV7EM -> ARMSubArch_v7em
                | armArch == ARM.ARMV8A -> ARMSubArch_v8
                | armArch == ARM.ARMV8_1A -> ARMSubArch_v8_1a
                | armArch == ARM.ARMV8_2A -> ARMSubArch_v8_2a
                | armArch == ARM.ARMV8_3A -> ARMSubArch_v8_3a
                | armArch == ARM.ARMV8_4A -> ARMSubArch_v8_4a
                | armArch == ARM.ARMV8_5A -> ARMSubArch_v8_5a
                | armArch == ARM.ARMV8_6A -> ARMSubArch_v8_6a
                | armArch == ARM.ARMV8_7A -> ARMSubArch_v8_7a
                | armArch == ARM.ARMV8_8A -> ARMSubArch_v8_8a
                | armArch == ARM.ARMV9A -> ARMSubArch_v9
                | armArch == ARM.ARMV9_1A -> ARMSubArch_v9_1a
                | armArch == ARM.ARMV9_2A -> ARMSubArch_v9_2a
                | armArch == ARM.ARMV9_3A -> ARMSubArch_v9_3a
                | armArch == ARM.ARMV8R -> ARMSubArch_v8r
                | armArch == ARM.ARMV8MBaseline -> ARMSubArch_v8m_baseline
                | armArch == ARM.ARMV8MMainline -> ARMSubArch_v8m_mainline
                | armArch == ARM.ARMV8_1MMainline -> ARMSubArch_v8_1m_mainline
                | otherwise -> NoSubArch
  where
    startsWith = (`List.isPrefixOf` subArchName)
    endsWith = (`List.isSuffixOf` subArchName)
    armSubArch = ARM.getCanonicalArchName (ARM.ArchName subArchName)

-- | @llvm::Triple::getDefaultFormat@
--
-- TODO(#97): Implement me!
defaultObjFmt :: TargetTriple -> ObjectFormat
defaultObjFmt _tt = UnknownObjectFormat

-- | @llvm::Triple::Triple@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L869
parseTriple :: String -> TargetTriple
parseTriple str =
  execState (split '-' str) $ do
    let pop def f =
          M.sets $
            \case
              (hd:rest) -> (f hd, rest)
              [] -> (def, [])
    (arch, subArch) <-
      pop (UnknownArch, NoSubArch) (\s -> (parseArch s, parseSubArch s))
    vendor <- pop UnknownVendor parseVendor
    os <- pop UnknownOS parseOS
    (env, objFmt) <-
      pop (UnknownEnvironment, UnknownObjectFormat) (\s -> (parseEnv s, parseObjFmt s))
    let tt =
          TargetTriple
          { ttArch = arch
          , ttSubArch = subArch
          , ttVendor = vendor
          , ttOS = os
          , ttEnv = env
          , ttObjFmt = objFmt
          }
    return (tt { ttObjFmt =
                   if ttObjFmt tt == UnknownObjectFormat
                   then defaultObjFmt tt
                   else ttObjFmt tt
               })
  where

    -- > split '-' "foo-bar" == ["foo", "bar"]
    split :: Char -> String -> [String]
    split splitter =
      foldr (\c strs -> if c == splitter then ([]:strs) else push c strs) [[]]
      where
        push c [] = [[c]]
        push c (s:strs) = (c:s):strs

    -- Not in MonadLib...
    execState s = fst . M.runState s
