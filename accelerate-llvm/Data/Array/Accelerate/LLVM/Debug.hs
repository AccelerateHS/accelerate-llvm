{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Debug
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover
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
import Numeric
import System.CPUTime
import System.Console.GetOpt
import System.Environment
import System.IO.Unsafe

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
    _dump_gc            :: !Bool        -- garbage collection & memory management
  , _dump_cc            :: !Bool        -- dump compilation trace
  , _dump_asm           :: !Bool        -- dump generated target-specific assembly
  , _dump_llvm          :: !Bool        -- dump generated LLVM code
  , _dump_exec          :: !Bool        -- kernel execution
  , _dump_gang          :: !Bool        -- print information about the gang
  , _dump_sched         :: !Bool        -- print scheduling information

    -- general options
  , _debug              :: !Bool        -- generate debugging symbols
  , _verbose            :: !Bool        -- additional status messages
  , _flush_cache        :: !Bool        -- delete the persistent cache directory
  }

flags :: [OptDescr (Flags -> Flags)]
flags =
  [ Option [] ["ddump-gc"]      (NoArg (set dump_gc True))      "print device memory management trace"
  , Option [] ["ddump-cc"]      (NoArg (set dump_cc True))      "print compilation phase trace"
  , Option [] ["ddump-asm"]     (NoArg (set dump_asm True))     "print generated target-specific assembly"
  , Option [] ["ddump-llvm"]    (NoArg (set dump_llvm True))    "print generated LLVM"
  , Option [] ["ddump-exec"]    (NoArg (set dump_exec True))    "print kernel execution trace"
  , Option [] ["ddump-gang"]    (NoArg (set dump_gang True))    "print thread gang information"
  , Option [] ["ddump-sched"]   (NoArg (set dump_sched True))   "print thread scheduling information"
  , Option [] ["ddebug"]        (NoArg (set debug True))        "generate debugging symbols"
  , Option [] ["dverbose"]      (NoArg (set verbose True))      "print additional information"
  , Option [] ["fflush-cache"]  (NoArg (set flush_cache True))  "delete the persistent cache directory"
  ]

initialise :: IO Flags
initialise = parse `fmap` getArgs
  where
    parse         = foldl parse1 defaults
    parse1 opts x = case filter (\(Option _ [f] _ _) -> x `isPrefixOf` ('-':f)) flags of
                      [Option _ _ (NoArg go) _] -> go opts
                      _                         -> opts         -- not specified, or ambiguous

    defaults      = Flags {
        _dump_gc        = False
      , _dump_cc        = False
      , _dump_asm       = False
      , _dump_llvm      = False
      , _dump_exec      = False
      , _dump_gang      = False
      , _dump_sched     = False
      , _debug          = False
      , _verbose        = False
      , _flush_cache    = False
      }


-- Create lenses manually, instead of deriving automatically using Template
-- Haskell. Since llvm-general binds to a C++ library, we can't load it into
-- ghci expect in ghc-7.8.
--
dump_gc, dump_cc, dump_asm, dump_llvm, dump_exec, dump_gang, dump_sched :: Flags :-> Bool
dump_gc         = lens _dump_gc     (\f x -> x { _dump_gc     = f (_dump_gc x) })
dump_cc         = lens _dump_cc     (\f x -> x { _dump_cc     = f (_dump_cc x) })
dump_asm        = lens _dump_asm    (\f x -> x { _dump_asm    = f (_dump_asm x) })
dump_llvm       = lens _dump_llvm   (\f x -> x { _dump_llvm   = f (_dump_llvm x) })
dump_exec       = lens _dump_exec   (\f x -> x { _dump_exec   = f (_dump_exec x) })
dump_gang       = lens _dump_gang   (\f x -> x { _dump_gang   = f (_dump_gang x) })
dump_sched      = lens _dump_sched  (\f x -> x { _dump_sched  = f (_dump_sched x) })

debug, verbose, flush_cache :: Flags :-> Bool
debug           = lens _debug       (\f x -> x { _debug       = f (_debug x) })
verbose         = lens _verbose     (\f x -> x { _verbose     = f (_verbose x) })
flush_cache     = lens _flush_cache (\f x -> x { _flush_cache = f (_flush_cache x) })

#ifdef ACCELERATE_DEBUG
setFlag, clearFlag :: (Flags :-> Bool) -> IO ()
setFlag f   = setFlags [f]
clearFlag f = clearFlags [f]

setFlags, clearFlags :: [Flags :-> Bool] -> IO ()
setFlags f   = modifyIORef options (\opt -> foldr (flip set True)  opt f)
clearFlags f = modifyIORef options (\opt -> foldr (flip set False) opt f)
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

