{- |
Module      : Text.LLVM.Triple.Parse.ARM
Description : ARM utilities used in target triple parsing.
License     : BSD3
Maintainer  : Langston Barrett
Stability   : experimental

This module is not exposed as part of the library API.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StrictData #-}

module Text.LLVM.Triple.Parse.ARM
  ( ArchName(..)
    -- * Endianness
  , EndianKind(..)
  , parseEndianKind
    -- * ISA
  , ISAKind(..)
  , parseISAKind
    -- * Arch
  , getCanonicalArchName
  , parseARMArch
  , ARMArch(..)
  , armArchName
  , parseArch
  ) where

import qualified Data.Char as Char
import           Control.Monad (liftM2, when)
import qualified MonadLib as M
import qualified MonadLib.Monads as M
import qualified Data.List as List

import           Text.LLVM.Triple.AST
import qualified Text.LLVM.Triple.Parse.LookupTable as Lookup

-- | The "arch" portion of a target triple
newtype ArchName = ArchName { getArchName :: String }

--------------------------------------------------------------------------------
-- Endianness

-- | @llvm::EndianKind@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/include/llvm/Support/ARMTargetParser.h#L166
data EndianKind
  = Little
  | Big
  deriving (Bounded, Enum, Eq, Ord)

-- | @llvm::ARM::parseArchEndian@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/ARMTargetParser.cpp#L248
parseEndianKind :: ArchName -> Maybe EndianKind
parseEndianKind (ArchName arch) =
  if | hasPfx "armeb" || hasPfx "thumbeb" || hasPfx "aarch64_be" -> Just Big
     | hasPfx "arm" || hasPfx "thumb" ->
         if hasSfx "eb"
         then Just Big
         else Just Little
     | hasPfx "aarch64" || hasPfx "aarch64_32" -> Just Little
     | otherwise -> Nothing
  where
    hasPfx = (`List.isPrefixOf` arch)
    hasSfx = (`List.isSuffixOf` arch)

--------------------------------------------------------------------------------
-- ISA

-- | @llvm::ISAKind@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/include/llvm/Support/ARMTargetParser.h#L162
data ISAKind
  = ISAArm
  | ISAThumb
  | ISAAArch64
  deriving (Bounded, Enum, Eq, Ord)

-- | @llvm::ARM::parseArchISA@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/ARMTargetParser.cpp#L267
parseISAKind :: ArchName -> Maybe ISAKind
parseISAKind (ArchName arch) = Lookup.lookupByPrefix arch table
  where
    table =
      Lookup.makeTable
        [ ("aarch64", ISAAArch64)
        , ("arm64", ISAAArch64)
        , ("thumb", ISAThumb)
        , ("arm", ISAArm)
        ]

--------------------------------------------------------------------------------
-- Arch

-- | @llvm::ARM::getArchSynonym@
getArchSynonym :: ArchName -> ArchName
getArchSynonym (ArchName arch) =
  ArchName $
    if | cases ["v5"] -> "v5t"
       | cases ["v5e"] -> "v5te"
       | cases ["v6j"] -> "v6"
       | cases ["v6hl"] -> "v6k"
       | cases ["v6m", "v6sm", "v6s-m"] -> "v6-m"
       | cases ["v6z", "v6zk"] -> "v6kz"
       | cases ["v7", "v7a", "v7hl", "v7l"] -> "v7-a"
       | cases ["v7r"] -> "v7-r"
       | cases ["v7m"] -> "v7-m"
       | cases ["v7em"] -> "v7e-m"
       | cases ["v8", "v8a", "v8l", "aarch64", "arm64"] -> "v8-a"
       | cases ["v8.1a"] -> "v8.1-a"
       | cases ["v8.2a"] -> "v8.2-a"
       | cases ["v8.3a"] -> "v8.3-a"
       | cases ["v8.4a"] -> "v8.4-a"
       | cases ["v8.5a"] -> "v8.5-a"
       | cases ["v8.6a"] -> "v8.6-a"
       | cases ["v8.7a"] -> "v8.7-a"
       | cases ["v8.8a"] -> "v8.8-a"
       | cases ["v8r"] -> "v8-r"
       | cases ["v9", "v9a"] -> "v9-a"
       | cases ["v9.1a"] -> "v9.1-a"
       | cases ["v9.2a"] -> "v9.2-a"
       | cases ["v9.3a"] -> "v9.3-a"
       | cases ["v8m.base"] -> "v8-m.base"
       | cases ["v8m.main"] -> "v8-m.main"
       | cases ["v8.1m.main"] -> "v8.1-m.main"
       | otherwise -> arch
  where
    cases = any (== arch)

data CanonicalArchNameState
  = CanonicalArchNameState
    { offset :: Int
    , archStr :: String -- ^ @A@ in the LLVM
    }

-- | @llvm::ARM::getCanonicalArchName@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/ARMTargetParser.cpp#L295
getCanonicalArchName :: ArchName -> Maybe ArchName
getCanonicalArchName (ArchName arch) =
  -- See Note [Implementation] for the reasoning behind the strange structure.
  --
  -- Could probably be translated even more directly using ContT, but that feels
  -- like a bit much.
  execState (CanonicalArchNameState 0 arch) $ do
    ifM (liftM2 (&&) (startsWith "aarch64") (contains "eb")) (return Nothing) $ do
      whenM (startsWith "arm64_32") $
        setOffset 8
      whenM (startsWith "arm64e") $
        setOffset 6
      whenM (startsWith "arm64") $
        setOffset 5
      whenM (startsWith "aarch64_32") $
        setOffset 10
      whenM (startsWith "arm") $
        setOffset 3
      whenM (startsWith "thumb") $
        setOffset 5
      whenM (startsWith "aarch64") $ do
        setOffset 7
        whenM ((== "_be") <$> archOffSubstr 3) $
          addOffset 3

      off <- offset <$> M.get
      sub <- archOffSubstr 2
      when (off /= npos && sub == "eb") $
        addOffset 2
      whenM (endsWith "eb") $ do
        changeArch (\arch' -> substr 0 (length arch' - 2) arch')
      off' <- offset <$> M.get
      when (off' /= npos) $
        changeArch (drop off')

      arch' <- archStr <$> M.get
      if | length arch' == 0 -> return Nothing
         | off' /= npos && length arch' > 2 &&
             (take 1 arch' /= "v" || not (all Char.isDigit (substr 1 1 arch'))) ->
               return Nothing
         | off' /= npos && "eb" `List.isInfixOf` arch' -> return Nothing
         | otherwise -> return (Just (ArchName arch'))
  where
    npos = 0

    ifM b thn els = b >>= \b' -> if b' then thn else els
    whenM b k = ifM b k (return ())
    startsWith pfx = (pfx `List.isPrefixOf`) . archStr <$> M.get
    endsWith sfx = (sfx `List.isSuffixOf`) . archStr <$> M.get
    contains ifx = (ifx `List.isInfixOf`) . archStr <$> M.get

    substr start sz = take sz . drop start
    archSubstr begin sz = substr begin sz . archStr <$> M.get
    archOffSubstr sz = do
      off <- offset <$> M.get
      archSubstr off sz

    changeOffset f = do
      s <- M.get
      M.set (s { offset = f (offset s) })
    addOffset n = changeOffset (n+)
    setOffset n = changeOffset (const n)

    changeArch f = M.set . (\s -> s { archStr = f (archStr s) }) =<< M.get

    -- Not in MonadLib...
    execState s = fst . M.runState s

-- | @llvm::parseARMArch@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/lib/Support/Triple.cpp#L377
parseARMArch :: ArchName -> Arch
parseARMArch archName =
  let
    isa = parseISAKind archName
    endian = parseEndianKind archName
    arch =
      case endian of
        Just Little ->
          case isa of
            Just ISAArm -> ARM
            Just ISAThumb -> Thumb
            Just ISAAArch64 -> AArch64
            Nothing -> UnknownArch
        Just Big ->
          case isa of
            Just ISAArm -> ARMEB
            Just ISAThumb -> ThumbEB
            Just ISAAArch64 -> AArch64_BE
            Nothing -> UnknownArch
        Nothing -> UnknownArch
    mArchName = getCanonicalArchName archName
  in case mArchName of
      Nothing -> UnknownArch
      Just (ArchName archNm) ->
        if | isa == Just ISAThumb &&
             ("v2" `List.isPrefixOf` archNm || "v3" `List.isPrefixOf` archNm) -> UnknownArch
            -- TODO(#98): LLVM has one more check here involving the "arch
            -- profile" that's not yet implemented here... Probably not a big
            -- deal because this case is only executed when the target arch
            -- doesn't match the "canonical" versions like "arm", "thumb", and
            -- "aarch64".
           | otherwise -> arch

-- | See LLVM's @ARMTargetParser.def@
--
-- https://github.com/llvm/llvm-project/blob/llvmorg-15.0.1/llvm/include/llvm/Support/ARMTargetParser.def#L48
data ARMArch
  = ARMArchInvalid
  | ARMV2
  | ARMV2A
  | ARMV3
  | ARMV3M
  | ARMV4
  | ARMV4T
  | ARMV5T
  | ARMV5TE
  | ARMV5TEJ
  | ARMV6
  | ARMV6K
  | ARMV6T2
  | ARMV6KZ
  | ARMV6M
  | ARMV7A
  | ARMV7VE
  | ARMV7R
  | ARMV7M
  | ARMV7EM
  | ARMV8A
  | ARMV8_1A
  | ARMV8_2A
  | ARMV8_3A
  | ARMV8_4A
  | ARMV8_5A
  | ARMV8_6A
  | ARMV8_7A
  | ARMV8_8A
  | ARMV9A
  | ARMV9_1A
  | ARMV9_2A
  | ARMV9_3A
  | ARMV8R
  | ARMV8MBaseline
  | ARMV8MMainline
  | ARMV8_1MMainline
  | IWMMXT
  | IWMMXT2
  | XSCALE
  | ARMV7S
  | ARMV7K
  deriving (Bounded, Enum, Eq, Ord)

armArchName :: ARMArch -> String
armArchName =
  \case
    ARMArchInvalid -> "invalid"
    ARMV2 -> "armv2"
    ARMV2A -> "armv2a"
    ARMV3 -> "armv3"
    ARMV3M -> "armv3m"
    ARMV4 -> "armv4"
    ARMV4T -> "armv4t"
    ARMV5T -> "armv5t"
    ARMV5TE -> "armv5te"
    ARMV5TEJ -> "armv5tej"
    ARMV6 -> "armv6"
    ARMV6K -> "armv6k"
    ARMV6T2 -> "armv6t2"
    ARMV6KZ -> "armv6kz"
    ARMV6M -> "armv6-m"
    ARMV7A -> "armv7-a"
    ARMV7VE -> "armv7ve"
    ARMV7R -> "armv7-r"
    ARMV7M -> "armv7-m"
    ARMV7EM -> "armv7e-m"
    ARMV8A -> "armv8-a"
    ARMV8_1A -> "armv8.1-a"
    ARMV8_2A -> "armv8.2-a"
    ARMV8_3A -> "armv8.3-a"
    ARMV8_4A -> "armv8.4-a"
    ARMV8_5A -> "armv8.5-a"
    ARMV8_6A -> "armv8.6-a"
    ARMV8_7A -> "armv8.7-a"
    ARMV8_8A -> "armv8.8-a"
    ARMV9A -> "armv9-a"
    ARMV9_1A -> "armv9.1-a"
    ARMV9_2A -> "armv9.2-a"
    ARMV9_3A -> "armv9.3-a"
    ARMV8R -> "armv8-r"
    ARMV8MBaseline -> "armv8-m.base"
    ARMV8MMainline -> "armv8-m.main"
    ARMV8_1MMainline -> "armv8.1-m.main"
    IWMMXT -> "iwmmxt"
    IWMMXT2 -> "iwmmxt2"
    XSCALE -> "xscale"
    ARMV7S -> "armv7s"
    ARMV7K -> "armv7k"

-- | @llvm::ARM::parseArch@
parseArch :: ArchName -> ARMArch
parseArch arch =
  let ArchName syn = getArchSynonym arch
      table = Lookup.enumTable armArchName
  in Lookup.lookupBySuffixWithDefault table ARMArchInvalid syn
