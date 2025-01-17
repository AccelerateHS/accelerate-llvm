{-# LANGUAGE TypeApplications #-}
module Data.Array.Accelerate.LLVM.Target.ClangInfo where

import qualified Text.LLVM.PP                                       as LP

-- standard library
import qualified Control.Exception                                  as E
import Data.ByteString                                              ( ByteString )
import qualified Data.ByteString.Char8                              as BS8
import Data.ByteString.Short                                        ( ShortByteString )
import qualified Data.ByteString.Short.Char8                        as SBS8
import Data.Char                                                    ( isDigit, isSpace )
import Data.List                                                    ( tails, sortBy )
import Data.List.NonEmpty                                           ( NonEmpty )
import qualified Data.List.NonEmpty                                 as NE
import Data.Maybe                                                   ( fromMaybe, catMaybes, isJust )
import Data.Ord                                                     ( comparing, Down(..) )
import System.Directory                                             ( executable, getPermissions, listDirectory )
import System.Environment                                           ( lookupEnv )
import qualified System.Info                                        as Info
import System.IO                                                    ( hPutStrLn, stderr )
import System.IO.Unsafe
import System.Process


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

llvmverFromTuple :: NonEmpty Int -> Maybe LP.LLVMVer
llvmverFromTuple (3 NE.:| 5 : _) = Just LP.llvmV3_5
llvmverFromTuple (3 NE.:| 6 : _) = Just LP.llvmV3_6
llvmverFromTuple (3 NE.:| 7 : _) = Just LP.llvmV3_7
llvmverFromTuple (3 NE.:| 8 : _) = Just LP.llvmV3_8
llvmverFromTuple (n NE.:| _)
  | n >= 4
  -- Don't compare against the "latest" here, because in practice, LLVM textual
  -- IR is fairly forward-compatible.
  -- , n <= P.llvmVlatest
  = Just n
llvmverFromTuple _ = Nothing

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
    mstderrOutput <- E.try @IOError $ do
      (_ec, _out, err) <- readProcessWithExitCode clangExePath ["-E", "-", "-march=native", "-###"] ""
      return err
    case mstderrOutput of
      Left _ -> do
        let overridden = isJust clangExePathEnvironment
        let msg1 =
              "[accelerate-llvm] The Accelerate LLVM backend requires Clang to be installed. " ++
              "Furthermore, accelerate-llvm-ptx, if you use it, requires clang version >= 16. " ++
              "(Tried to run: '" ++ clangExePath ++ "'."
            msg2 | overridden = ""
                 | otherwise =
                     " To override this choice, set the " ++
                     "ACCELERATE_LLVM_CLANG_PATH environment variable to point to the desired " ++
                     "clang executable."
            msg3 = ")"
        hPutStrLn stderr $ msg1 ++ msg2 ++ msg3
        -- not an IOError because we're in unsafePerformIO
        errorWithoutStackTrace $
          "accelerate-llvm: Clang not found: " ++ clangExePath ++
           (if overridden then "" else " (set ACCELERATE_LLVM_CLANG_PATH to override)")
      Right out -> return out

clangExePath :: FilePath
clangExePath
  | Just path <- clangExePathEnvironment = path
  -- For some reason, Windows is always "mingw32", even if it's actually 64-bit, or whatever.
  | Info.os == "mingw32" = clangExePathWindows
  | otherwise            = "clang"  -- on unix, don't try fancy logic

{-# NOINLINE clangExePathWindows #-}
clangExePathWindows :: FilePath
clangExePathWindows = unsafePerformIO $ do
  let attempts :: [IO (Maybe a)] -> IO (Maybe a)
      attempts [] = return Nothing
      attempts (act:acts) = act >>= maybe (attempts acts) (return . Just)

  -- Tries "$base\LLVM{,-[0-9]+}\bin\clang.exe"
  let tryLLVMdir base = do
        listing <- E.catch @E.IOException
                     (listDirectory base)
                     (\_ -> return [])
        let llvmdirs = filter (\s -> take 4 s == "LLVM") listing
        let plain = filter (== "LLVM") llvmdirs
        let versions = [(s, read (drop 5 s) :: Int)
                       | s <- llvmdirs
                       , take 5 s == "LLVM-"
                       , length s > 5
                       , all isDigit (drop 5 s)]
        -- Prefer a plain "LLVM" directory; if there are only versioned ones,
        -- prefer the highest-versioned.
        attempts
          [do isExe <- E.catch @E.IOException
                         (executable <$> getPermissions exe)
                         (\_ -> return False)
              return $ if isExe then Just exe else Nothing
          | name <- plain ++ map fst (sortBy (comparing (Down . snd)) versions)
          , let exe = base ++ "\\" ++ name ++ "\\bin\\clang.exe"]

  -- TODO: any more places to look?
  mpath <- attempts
    [tryLLVMdir "C:\\Program Files"
    ,tryLLVMdir "C:\\"]
  case mpath of
    Just path -> return path
    Nothing -> return "clang"  -- fall back to the system path

{-# NOINLINE clangExePathEnvironment #-}
clangExePathEnvironment :: Maybe FilePath
clangExePathEnvironment = unsafePerformIO $ lookupEnv "ACCELERATE_LLVM_CLANG_PATH"

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
