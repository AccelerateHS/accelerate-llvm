-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Target
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Target (

  module Data.Array.Accelerate.LLVM.Target,
  module Data.Array.Accelerate.LLVM.Native.Target

) where

-- llvm-hs
-- import LLVM.Target                                                  hiding ( Target )
-- import LLVM.AST.DataLayout                                          ( DataLayout )
-- import qualified LLVM.Relocation                                    as RelocationModel
-- import qualified LLVM.CodeModel                                     as CodeModel
-- import qualified LLVM.CodeGenOpt                                    as CodeOptimisation

-- accelerate
import Data.Array.Accelerate.LLVM.Native.Link.Cache                 ( LinkCache )
import Data.Array.Accelerate.LLVM.Native.Execute.Scheduler          ( Workers )
import Data.Array.Accelerate.LLVM.Target                            ( Target(..) )

-- standard library
import Data.Bifunctor                                               ( first )
import Data.ByteString                                              ( ByteString )
import qualified Data.ByteString.Char8                              as BS8
import Data.ByteString.Short                                        ( ShortByteString )
import qualified Data.ByteString.Short.Char8                        as SBS8
import Data.Char                                                    ( isSpace )
import Data.List                                                    ( findIndex )
import Data.List.NonEmpty                                           ( NonEmpty )
import qualified Data.List.NonEmpty                                 as NE
import System.IO.Unsafe
import System.Process


-- | Native machine code JIT execution target
--
data Native = Native
  { linkCache     :: !LinkCache
  , workers       :: !Workers
  }

instance Target Native where
  targetTriple     = Just nativeTargetTriple
  targetDataLayout = Nothing  -- TODO: does this work?


-- | String that describes the native target
--
{-# NOINLINE nativeTargetTriple #-}
nativeTargetTriple :: ShortByteString
nativeTargetTriple = unsafePerformIO $
  -- A target triple suitable for loading code into the current process
  SBS8.pack . trim <$> readProcess "llvm-config" ["--host-target"] ""
  where
    trim = reverse . dropWhile (== '\n') . reverse

-- | A description of the various data layout properties that may be used during
-- optimisation.
--
-- {-# NOINLINE nativeDataLayout #-}
-- nativeDataLayout :: DataLayout
-- nativeDataLayout
--   = unsafePerformIO
--   $ withNativeTargetMachine getTargetMachineDataLayout

-- | String that describes the host CPU
--
-- TODO: this function needs to be generalised a LOT
-- TODO: Ivo says: perhaps we can simply hash the model numbers that the
-- `cpuinfo` instruction returns? No need to do a complicated lookup if we're
-- going to hash the result anyway.
nativeCPUName :: ByteString
nativeCPUName = BS8.pack $
  case parseCPUInfo procCPUInfoContents of
    [] -> "generic"
    (core : _) -> case (lookup "vendor_id" core, lookup "cpu family" core, lookup "model" core) of
      (Just "GenuineIntel", Just "6", Just "165") -> "skylake"  -- Tom's home machine
      (Just "AuthenticAMD", Just "23", Just "8") -> "znver1"  -- jizo
      tup -> error $ "Woefully inadequate /proc/cpuinfo parser! " ++ show tup ++ "  -- " ++ show core
             -- go here: https://llvm.org/doxygen/Host_8cpp_source.html
             -- and find the CPU string corresponding to the 'cpu family' and 'model'
             -- values in your /proc/cpuinfo. (Don't forget to search for both hex
             -- and decimal.) You can cross-check against wikichip, e.g.
             -- https://en.wikichip.org/wiki/amd/cpuid#Family_23_.2817h.29
  where
  {-# NOINLINE procCPUInfoContents #-}
  procCPUInfoContents :: String
  procCPUInfoContents = unsafePerformIO $ readFile "/proc/cpuinfo"

  parseCPUInfo :: String -> [[(String, String)]]
  parseCPUInfo = filter (not . null) . uncurry (:) . go . lines
    where
      go [] = ([], [])
      go ("" : lns) = let (core1, cores) = go lns in ([], core1 : cores)
      go (line : lns) =
        case findIndex (== ':') line of
          Nothing -> first ((line, "") :) (go lns)
          Just i -> case splitAt i line of
                      (lhs, ':' : rhs) -> first ((dropWhileEnd isSpace lhs, dropWhile isSpace rhs) :) (go lns)
                      _ -> error "No ':' in line that has ':'?"

      dropWhileEnd p = reverse . dropWhile p . reverse


-- | Bracket the creation and destruction of a target machine for the native
-- backend running on this host.
--
-- withNativeTargetMachine
--     :: (TargetMachine -> IO a)
--     -> IO a
-- withNativeTargetMachine k = do
--   initializeNativeTarget
--   nativeCPUFeatures <- getHostCPUFeatures
--   (nativeTarget, _) <- lookupTarget Nothing nativeTargetTriple
--   withTargetOptions $ \targetOptions ->
--     withTargetMachine
--         nativeTarget
--         nativeTargetTriple
--         nativeCPUName
--         nativeCPUFeatures
--         targetOptions
--         RelocationModel.PIC
--         CodeModel.Default
--         CodeOptimisation.Default
--         k

-- | Returns list of version components: '12.3' becomes [12, 3].
hostLLVMVersion :: NonEmpty Int
hostLLVMVersion = fmap read (splitOn '.' versionString)
  where
  {-# NOINLINE versionString #-}
  versionString :: String
  versionString = unsafePerformIO $ readProcess "llvm-config" ["--version"] ""

  splitOn :: Eq a => a -> [a] -> NonEmpty [a]
  splitOn _ [] = [] NE.:| []
  splitOn sep (c:cs)
    | c == sep = [] NE.<| splitOn sep cs
    | otherwise = let hd NE.:| tl = splitOn sep cs
                  in (c : hd) NE.:| tl
