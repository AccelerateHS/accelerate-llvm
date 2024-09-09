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

-- accelerate
import Data.Array.Accelerate.LLVM.Native.Link.Cache                 ( LinkCache )
import Data.Array.Accelerate.LLVM.Native.Execute.Scheduler          ( Workers )
import Data.Array.Accelerate.LLVM.Target                            ( Target(..) )

-- standard library
import Data.ByteString                                              ( ByteString )
import qualified Data.ByteString.Char8                              as BS8
import Data.ByteString.Short                                        ( ShortByteString )
import qualified Data.ByteString.Short.Char8                        as SBS8
import Data.Char                                                    ( isSpace )
import Data.List                                                    ( tails )
import Data.List.NonEmpty                                           ( NonEmpty )
import qualified Data.List.NonEmpty                                 as NE
import Data.Maybe                                                   ( fromMaybe, catMaybes )
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
  targetDataLayout = Nothing  -- LLVM will fill it in just fine for CPU targets


-- | String that describes the native target
--
nativeTargetTriple :: ShortByteString
nativeTargetTriple =
  SBS8.pack $
    fromMaybe (error $ "Could not extract native target triple from `clang -###` output: <" ++ clangMachineVersionOutput ++ ">")
              (getLinePrefixedBy "Target: " clangMachineVersionOutput)

-- | String that describes the host CPU
--
nativeCPUName :: ByteString
nativeCPUName =
  case catMaybes (map tryFromLine (lines clangMachineVersionOutput)) of
    cpu : _ -> BS8.pack cpu
    _ -> error $ "Could not extract target CPU from `clang -###` output: <" ++ clangMachineVersionOutput ++ ">"
  where
  tryFromLine :: String -> Maybe String
  tryFromLine line =
    case map snd
         . filter ((== "\"-target-cpu\" ") . fst)
         . map (splitAt 14)
         $ tails line of
      ('"' : rest) : _
        | (cpu, '"' : _) <- break (== '"') rest
        -> Just cpu
      _ -> Nothing


-- | Returns list of version components: '12.3' becomes [12, 3].
hostLLVMVersion :: NonEmpty Int
hostLLVMVersion =
  fmap read . splitOn '.' $
    let firstLine = takeWhile (/= '\n') $
                      dropWhile isSpace clangMachineVersionOutput
    in case catMaybes (map (`startsWith` "clang version") (tails firstLine)) of
         rest : _ ->
           -- "Ubuntu clang version 14.0.0-1ubuntu1.1" <- we care about the "14.0.0" part only
           takeWhile (\c -> not (isSpace c) && c /= '-')
           $ dropWhile isSpace
           $ rest
         [] -> error $ "Could not extract clang version from `clang -###` output: <" ++ clangMachineVersionOutput ++ ">"
  where
  splitOn :: Eq a => a -> [a] -> NonEmpty [a]
  splitOn _ [] = [] NE.:| []
  splitOn sep (c:cs)
    | c == sep = [] NE.<| splitOn sep cs
    | otherwise = let hd NE.:| tl = splitOn sep cs
                  in (c : hd) NE.:| tl

  startsWith :: Eq a => [a] -> [a] -> Maybe [a]
  long `startsWith` short =
    let (pre, post) = splitAt (length short) long
    in if pre == short then Just post else Nothing

-- | This is a string that contains some debug metadata from clang (clang
-- version, target triple, etc.) as well as the inferred command-line
-- invocation for a simple operation (preprocessor mode). This list of
-- arguments happens to include the CPU name.
--
-- Yes, this does include the requisite information in the requisite format for
-- clang versions (tested) 7, 8, 9, 11, 12, 15, 18 and 19. Probably more.
{-# NOINLINE clangMachineVersionOutput #-}
clangMachineVersionOutput :: String
clangMachineVersionOutput =
  unsafePerformIO $ do
    (_ec, _out, err) <- readProcessWithExitCode "clang" ["-E", "-", "-march=native", "-###"] ""
    return err

-- | Returns trimmed right-hand side
getLinePrefixedBy :: String -> String -> Maybe String
getLinePrefixedBy prefix str =
  case map snd
       . filter ((== prefix) . fst)
       . map (splitAt (length prefix))
       $ lines str of
    rhs : _ -> Just (trim rhs)
    _ -> Nothing
  where
    trim = reverse . dropWhile (== '\n') . reverse
