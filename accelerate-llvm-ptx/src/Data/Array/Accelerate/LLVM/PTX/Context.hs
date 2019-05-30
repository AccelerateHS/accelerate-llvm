{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Context
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Context (

  Context(..),
  new, raw, withContext,

) where

import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.LLVM.PTX.Analysis.Device
import qualified Data.Array.Accelerate.LLVM.PTX.Debug           as Debug

import qualified Foreign.CUDA.Driver.Device                     as CUDA
import qualified Foreign.CUDA.Driver.Context                    as CUDA

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Hashable
import Text.PrettyPrint
import Prelude                                                  hiding ( (<>) )

import GHC.Base                                                 ( Int(..), addr2Int#, )
import GHC.Ptr                                                  ( Ptr(..) )


-- | An execution context, which is tied to a specific device and CUDA execution
-- context.
--
data Context = Context {
    deviceProperties    :: {-# UNPACK #-} !CUDA.DeviceProperties        -- information on hardware resources
  , deviceContext       :: {-# UNPACK #-} !(Lifetime CUDA.Context)      -- device execution context
  }

instance Eq Context where
  c1 == c2 = deviceContext c1 == deviceContext c2

instance Hashable Context where
  hashWithSalt salt =
    let
        ptrToInt :: Ptr a -> Int
        ptrToInt (Ptr addr#) = I# (addr2Int# addr#)
    in
    hashWithSalt salt . ptrToInt . CUDA.useContext . unsafeGetValue . deviceContext


-- | Create a new CUDA execution context
--
new :: CUDA.Device
    -> CUDA.DeviceProperties
    -> [CUDA.ContextFlag]
    -> IO Context
new dev prp flags = do
  ctx <- raw dev prp =<< CUDA.create dev flags
  _   <- CUDA.pop
  return ctx

-- | Wrap a raw CUDA execution context
--
raw :: CUDA.Device
    -> CUDA.DeviceProperties
    -> CUDA.Context
    -> IO Context
raw dev prp ctx = do
  lft <- newLifetime ctx
  addFinalizer lft $ do
    message $ "finalise context " ++ showContext ctx
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
  Debug.traceIO Debug.dump_phases (deviceInfo dev prp)

  return $! Context prp lft


-- | Push the context onto the CPUs thread stack of current contexts and execute
-- some operation.
--
{-# INLINE withContext #-}
withContext :: Context -> IO a -> IO a
withContext Context{..} action
  = runInBoundThread
  $ withLifetime deviceContext $ \ctx ->
      bracket_ (push ctx) pop action

{-# INLINE push #-}
push :: CUDA.Context -> IO ()
push ctx = do
  message $ "push context: " ++ showContext ctx
  CUDA.push ctx

{-# INLINE pop #-}
pop :: IO ()
pop = do
  ctx <- CUDA.pop
  message $ "pop context: "  ++ showContext ctx


-- Debugging
-- ---------

-- Nicely format a summary of the selected CUDA device, example:
--
-- Device 0: GeForce 9600M GT (compute capability 1.1), 4 multiprocessors @ 1.25GHz (32 cores), 512MB global memory
--
deviceInfo :: CUDA.Device -> CUDA.DeviceProperties -> String
deviceInfo dev prp = render $
  devID <> colon <+> name <+> parens compute
        <> comma <+> processors <+> at <+> text clock <+> parens cores
        <> comma <+> memory
  where
    name        = text (CUDA.deviceName prp)
    compute     = text "compute capability" <+> text (show $ CUDA.computeCapability prp)
    devID       = text "device" <+> int (fromIntegral $ CUDA.useDevice dev)
    processors  = int (CUDA.multiProcessorCount prp)                              <+> text "multiprocessors"
    cores       = int (CUDA.multiProcessorCount prp * coresPerMultiProcessor prp) <+> text "cores"
    memory      = text mem <+> text "global memory"
    --
    clock       = Debug.showFFloatSIBase (Just 2) 1000 (fromIntegral $ CUDA.clockRate prp * 1000 :: Double) "Hz"
    mem         = Debug.showFFloatSIBase (Just 0) 1024 (fromIntegral $ CUDA.totalGlobalMem prp   :: Double) "B"
    at          = char '@'
    -- reset       = zeroWidthText "\r"


{-# INLINE trace #-}
trace :: String -> IO a -> IO a
trace msg next = do
  Debug.traceIO Debug.dump_gc ("gc: " ++ msg)
  next

{-# INLINE message #-}
message :: String -> IO ()
message s = s `trace` return ()

{-# INLINE showContext #-}
showContext :: CUDA.Context -> String
showContext (CUDA.Context c) = show c

