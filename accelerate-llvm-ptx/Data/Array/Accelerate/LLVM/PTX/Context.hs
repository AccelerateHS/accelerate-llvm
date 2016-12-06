{-# LANGUAGE CPP             #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples   #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Context
-- Copyright   : [2014..2015] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Context (

  Context(..),
  new, raw, destroy, push, pop,

) where

import Data.Array.Accelerate.LLVM.PTX.Analysis.Device
import qualified Data.Array.Accelerate.LLVM.PTX.Debug           as Debug

import qualified Foreign.CUDA.Analysis                          as CUDA
import qualified Foreign.CUDA.Driver                            as CUDA
import qualified Foreign.CUDA.Driver.Context                    as CUDA
import qualified Foreign.CUDA.Driver.Device                     as CUDA

import Control.Monad
import Text.PrettyPrint

import GHC.Exts                                                 ( Ptr(..), mkWeak# )
import GHC.Base                                                 ( IO(..) )
import GHC.Weak                                                 ( Weak(..) )


-- | An execution context, which is tied to a specific device and CUDA execution
-- context.
--
data Context = Context {
    deviceProperties    :: {-# UNPACK #-} !CUDA.DeviceProperties        -- information on hardware resources
  , deviceContext       :: {-# UNPACK #-} !CUDA.Context                 -- device execution context
  , weakContext         :: {-# UNPACK #-} !(Weak CUDA.Context)          -- weak pointer to previous
  }

instance Eq Context where
  c1 == c2 = deviceContext c1 == deviceContext c2


-- | Create a new CUDA execution context
--
new :: CUDA.Device
    -> CUDA.DeviceProperties
    -> [CUDA.ContextFlag]
    -> IO Context
new dev prp flags =
  raw dev prp =<< CUDA.create dev flags

-- | Wrap a raw CUDA execution context
--
raw :: CUDA.Device
    -> CUDA.DeviceProperties
    -> CUDA.Context
    -> IO Context
raw dev prp ctx = do
  weak  <- mkWeakContext ctx $ do
    Debug.traceIO Debug.dump_gc ("gc: finalise context " ++ show (CUDA.useContext ctx))
    CUDA.destroy ctx

  -- The kernels don't use much shared memory, so for devices that support it
  -- prefer using those memory banks as an L1 cache.
  --
  -- TLM: Is this a good idea? For example, external libraries such as cuBLAS
  -- rely heavily on shared memory and thus this could adversely affect
  -- performance. Perhaps we should use 'setCacheConfigFun' for individual
  -- functions which might benefit from this.
  --
  when (CUDA.computeCapability prp >= CUDA.Compute 2 0)
       (CUDA.setCache CUDA.PreferL1)

  -- Display information about the selected device
  Debug.traceIO Debug.verbose (deviceInfo dev prp)

  _     <- CUDA.pop
  return $! Context prp ctx weak


-- | Destroy the specified context. This will fail if the context is more than
-- single attachment.
--
{-# INLINE destroy #-}
destroy :: Context -> IO ()
destroy Context{..} = do
  Debug.traceIO Debug.dump_gc $
    "gc: destroy context: " ++ show (CUDA.useContext deviceContext)
  CUDA.destroy deviceContext


-- | Push the given context onto the CPU's thread stack of current contexts. The
-- context must be floating (via 'pop'), i.e. not attached to any thread.
--
{-# INLINE push #-}
push :: Context -> IO ()
push Context{..} = do
  Debug.traceIO Debug.dump_gc $
    "gc: push context: " ++ show (CUDA.useContext deviceContext)
  CUDA.push deviceContext


-- | Pop the current context.
--
{-# INLINE pop #-}
pop :: IO ()
pop = do
  ctx <- CUDA.pop
  Debug.traceIO Debug.dump_gc $
    "gc: pop context: " ++ show (CUDA.useContext ctx)


-- | Make a weak pointer to a CUDA context. We need to be careful to put the
-- finaliser on the underlying pointer, rather than the box around it as
-- 'mkWeak' will do, because unpacking the context will cause the finaliser to
-- fire prematurely.
--
mkWeakContext :: CUDA.Context -> IO () -> IO (Weak CUDA.Context)
mkWeakContext c@(CUDA.Context (Ptr c#)) = go
  where
    {-# INLINE go #-}
#if __GLASGOW_HASKELL__ >= 800
    go (IO f)  =  -- GHC-8.x
#else
    go f       =  -- GHC-7.x
#endif
      IO $ \s -> case mkWeak# c# c f s of
                   (# s', w #) -> (# s', Weak w #)


-- Debugging
-- ---------

-- Nicely format a summary of the selected CUDA device, example:
--
-- Device 0: GeForce 9600M GT (compute capability 1.1)
--           4 multiprocessors @ 1.25GHz (32 cores), 512MB global memory
--
deviceInfo :: CUDA.Device -> CUDA.DeviceProperties -> String
deviceInfo dev prp = render $ reset <>
  devID <> colon <+> vcat [ name <+> parens compute
                          , processors <+> at <+> text clock <+> parens cores <> comma <+> memory
                          ]
  where
    name        = text (CUDA.deviceName prp)
    compute     = text "compute capatability" <+> text (show $ CUDA.computeCapability prp)
    devID       = text "Device" <+> int (fromIntegral $ CUDA.useDevice dev)     -- hax
    processors  = int (CUDA.multiProcessorCount prp)                              <+> text "multiprocessors"
    cores       = int (CUDA.multiProcessorCount prp * coresPerMultiProcessor prp) <+> text "cores"
    memory      = text mem <+> text "global memory"
    --
    clock       = Debug.showFFloatSIBase (Just 2) 1000 (fromIntegral $ CUDA.clockRate prp * 1000 :: Double) "Hz"
    mem         = Debug.showFFloatSIBase (Just 0) 1024 (fromIntegral $ CUDA.totalGlobalMem prp   :: Double) "B"
    at          = char '@'
    reset       = zeroWidthText "\r"

