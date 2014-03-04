{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Debug
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Debug
  where

import Control.Monad.Trans
import Data.IORef
import Data.Label
import Data.List
import Data.Time.Clock
import Numeric
import System.CPUTime
import System.Console.GetOpt
import System.Environment
import System.IO.Unsafe
import Text.Printf

import Debug.Trace                              ( traceIO, traceEventIO )


-- Pretty-printing
-- ---------------

showFFloatSIBase :: RealFloat a => Maybe Int -> a -> a -> ShowS
showFFloatSIBase p b n
  = showString
  . nubBy (\x y -> x == ' ' && y == ' ')
  $ showFFloat p n' (' ':si_unit)
  where
    n'          = n / (b ^^ pow)
    pow         = (-4) `max` floor (logBase b n) `min` (4 :: Int)
    si_unit     = case pow of
                       -4 -> "p"
                       -3 -> "n"
                       -2 -> "µ"
                       -1 -> "m"
                       0  -> ""
                       1  -> "k"
                       2  -> "M"
                       3  -> "G"
                       4  -> "T"


-- Internals
-- ---------

data Flags = Flags
  {
    _dump_gc           :: !Bool         -- garbage collection & memory management
  , _dump_llvm         :: !Bool         -- dump generated (unoptimised) LLVM code
  , _dump_exec         :: !Bool         -- kernel execution
  , _dump_gang         :: !Bool         -- print information about the gang

    -- general options
  , _verbose           :: !Bool         -- additional status messages
  , _flush_cache       :: !Bool         -- delete the persistent cache directory
  }

flags :: [OptDescr (Flags -> Flags)]
flags =
  [ Option [] ["ddump-gc"]      (NoArg (set dump_gc True))      "print device memory management trace"
  , Option [] ["ddump-llvm"]    (NoArg (set dump_llvm True))    "print generated (unoptimised) LLVM IR"
  , Option [] ["ddump-exec"]    (NoArg (set dump_exec True))    "print kernel execution trace"
  , Option [] ["ddump-gang"]    (NoArg (set dump_gang True))    "print thread gang information"
  , Option [] ["dverbose"]      (NoArg (set verbose True))      "print additional information"
  , Option [] ["fflush-cache"]  (NoArg (set flush_cache True))  "delete the persistent cache directory"
  ]

initialise :: IO Flags
initialise = parse `fmap` getArgs
  where
    defaults      = Flags False False False False False False
    parse         = foldl parse1 defaults
    parse1 opts x = case filter (\(Option _ [f] _ _) -> x `isPrefixOf` ('-':f)) flags of
                      [Option _ _ (NoArg go) _] -> go opts
                      _                         -> opts         -- not specified, or ambiguous


-- Create lenses manually, instead of deriving automatically using Template
-- Haskell. Since llvm-general binds to a C++ library, we can't load it into
-- ghci expect in ghc-7.8.
--
dump_gc, dump_llvm, dump_exec, dump_gang, verbose, flush_cache :: Flags :-> Bool
dump_gc         = lens _dump_gc   (\f x -> x { _dump_gc   = f (_dump_gc x) })
dump_llvm       = lens _dump_llvm (\f x -> x { _dump_llvm = f (_dump_llvm x) })
dump_exec       = lens _dump_exec (\f x -> x { _dump_exec = f (_dump_exec x) })
dump_gang       = lens _dump_gang (\f x -> x { _dump_gang = f (_dump_gang x) })

verbose         = lens _verbose     (\f x -> x { _verbose     = f (_verbose x) })
flush_cache     = lens _flush_cache (\f x -> x { _flush_cache = f (_flush_cache x) })

#ifdef ACCELERATE_DEBUG
setFlag :: (Flags :-> Bool) -> IO ()
setFlag f = modifyIORef options (set f True)

clearFlag :: (Flags :-> Bool) -> IO ()
clearFlag f = modifyIORef options (set f False)
#endif

#ifdef ACCELERATE_DEBUG
{-# NOINLINE options #-}
options :: IORef Flags
options = unsafePerformIO $ newIORef =<< initialise
#endif

{-# INLINE mode #-}
mode :: (Flags :-> Bool) -> Bool
#ifdef ACCELERATE_DEBUG
mode f = unsafePerformIO $ get f `fmap` readIORef options
#else
mode _ = False
#endif

{-# INLINE message #-}
message :: MonadIO m => (Flags :-> Bool) -> String -> m ()
#ifdef ACCELERATE_DEBUG
message f str
  = when f . liftIO
  $ do psec     <- getCPUTime
       let sec   = fromIntegral psec * 1E-12 :: Double
       traceIO   $ showFFloat (Just 2) sec (':':str)
#else
message _ _   = return ()
#endif

{-# INLINE event #-}
event :: MonadIO m => (Flags :-> Bool) -> String -> m ()
#ifdef ACCELERATE_DEBUG
event f str = when f (liftIO $ traceEventIO str)
#else
event _ _   = return ()
#endif

{-# INLINE when #-}
when :: MonadIO m => (Flags :-> Bool) -> m () -> m ()
#ifdef ACCELERATE_DEBUG
when f action
  | mode f      = action
  | otherwise   = return ()
#else
when _ _        = return ()
#endif

{-# INLINE unless #-}
unless :: MonadIO m => (Flags :-> Bool) -> m () -> m ()
#ifdef ACCELERATE_DEBUG
unless f action
  | mode f      = return ()
  | otherwise   = action
#else
unless _ action = action
#endif

{-# INLINE timed #-}
timed :: MonadIO m
      => (Flags :-> Bool)
      -> (Double -> Double -> String)
      -> m ()
      -> m ()
timed _f _msg action
#ifdef ACCELERATE_DEBUG
  | mode _f
  = do
        -- We will measure both wall clock as well as CPU time.
        wall0   <- liftIO getCurrentTime
        cpu0    <- liftIO getCPUTime

        -- Run the action in the main thread.
        action

        wall1   <- liftIO getCurrentTime
        cpu1    <- liftIO getCPUTime

        let wallTime = realToFrac (diffUTCTime wall1 wall0)
            cpuTime  = fromIntegral (cpu1 - cpu0) * 1E-12

        message _f (_msg wallTime cpuTime)

  | otherwise
#endif
  = action

{-# INLINE elapsed #-}
elapsed :: Double -> Double -> String
elapsed wallTime cpuTime
  = printf "wall: %s, cpu: %s, speedup: %.2f"
      (showFFloatSIBase (Just 3) 1000 wallTime "s")
      (showFFloatSIBase (Just 3) 1000 cpuTime  "s")
      (cpuTime / wallTime)

