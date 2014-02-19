{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Debug
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Debug
  where

import Numeric
import Data.List
import Data.Label
import Data.IORef
import Control.Monad.Trans
import System.CPUTime
import System.IO.Unsafe
import System.Environment
import System.Console.GetOpt

import Debug.Trace                              ( traceIO, traceEventIO )


-- Pretty-printing
-- ---------------

showFFloatSIBase :: RealFloat a => Maybe Int -> a -> a -> ShowS
showFFloatSIBase p b n
  = showString
  . nubBy (\x y -> x == ' ' && y == ' ')
  $ showFFloat p n' [ ' ', si_unit ]
  where
    n'          = n / (b ^^ (pow-4))
    pow         = max 0 . min 8 . (+) 4 . floor $ logBase b n
    si_unit     = "pnµm kMGT" !! pow


-- Internals
-- ---------

fclabels [d|
  data Flags = Flags
    {
      -- phase control
      dump_gc           :: !Bool        -- garbage collection & memory management
    , dump_cc           :: !Bool        -- compilation & linking
    , debug_cc          :: !Bool        -- compile device code with debug symbols
    , dump_exec         :: !Bool        -- kernel execution

      -- general options
    , verbose           :: !Bool        -- additional status messages
    , flush_cache       :: !Bool        -- delete the persistent cache directory
    }
  |]

flags :: [OptDescr (Flags -> Flags)]
flags =
  [ Option [] ["ddump-gc"]      (NoArg (set dump_gc True))      "print device memory management trace"
  , Option [] ["ddump-cc"]      (NoArg (set dump_cc True))      "print generated code and compilation information"
  , Option [] ["ddebug-cc"]     (NoArg (set debug_cc True))     "generate debug information for device code"
  , Option [] ["ddump-exec"]    (NoArg (set dump_exec True))    "print kernel execution trace"
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

