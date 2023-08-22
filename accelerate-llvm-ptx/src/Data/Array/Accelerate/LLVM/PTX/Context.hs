{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Context
-- Copyright   : [2014..2020] The Accelerate Team
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
import qualified Data.Array.Accelerate.LLVM.PTX.Debug               as Debug

import qualified Foreign.CUDA.Driver.Device                         as CUDA
import qualified Foreign.CUDA.Driver.Context                        as CUDA

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.Hashable
import Data.Int
import Data.Primitive.ByteArray
import Data.Text.Lazy.Builder
import Data.Word
import Formatting
import Prettyprinter
import Prettyprinter.Internal
import Prettyprinter.Render.Util.Panic
import Text.Printf
import qualified Data.Text.Lazy.Builder                             as TLB
import Prelude                                                      hiding ( (<>) )

import GHC.Base                                                     ( Int(..), addr2Int#, )
import GHC.Ptr                                                      ( Ptr(..) )


-- | An execution context, which is tied to a specific device and CUDA execution
-- context.
--
data Context = Context {
    deviceProperties    :: {-# UNPACK #-} !CUDA.DeviceProperties        -- information on hardware resources
  , deviceName          :: {-# UNPACK #-} !ByteArray                    -- device name, used for profiling
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
    message ("finalise context " % formatContext) ctx
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

  -- Generate the context name
  let str = printf "[%d] %s\0" (fromIntegral (CUDA.useDevice dev) :: Int) (CUDA.deviceName prp)
  mba <- newPinnedByteArray (length str)

  let go !_ []     = unsafeFreezeByteArray mba
      go !i (x:xs) = do
        writeByteArray mba i (fromIntegral (ord x) :: Word8)
        go (i+1) xs

  nm   <- go 0 str

  -- Display information about the selected device
  Debug.traceM Debug.dump_phases builder (deviceInfo dev prp)

  return $! Context prp nm lft


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
  message ("push context: " % formatContext) ctx
  CUDA.push ctx

{-# INLINE pop #-}
pop :: IO ()
pop = do
  ctx <- CUDA.pop
  message ("pop context: " % formatContext) ctx


-- Debugging
-- ---------

-- Nicely format a summary of the selected CUDA device, example:
--
-- Device 0: GeForce 9600M GT (compute capability 1.1), 4 multiprocessors @ 1.25GHz (32 cores), 512MB global memory
--
deviceInfo :: CUDA.Device -> CUDA.DeviceProperties -> Builder
deviceInfo dev prp = go $ layoutPretty defaultLayoutOptions $
  devID <> colon <+> name <+> parens compute
        <> comma <+> processors <+> at <+> pretty clock <+> parens cores
        <> comma <+> memory
  where
    name        = pretty (CUDA.deviceName prp)
    compute     = "compute capability" <+> unsafeViaShow (CUDA.computeCapability prp)
    devID       = "device" <+> unsafeViaShow (CUDA.useDevice dev)
    processors  = pretty (CUDA.multiProcessorCount prp)                              <+> "multiprocessors"
    cores       = pretty (CUDA.multiProcessorCount prp * coresPerMultiProcessor prp) <+> "cores"
    memory      = pretty mem <+> "global memory"
    ----
    clock       = toLazyText $ Debug.showFFloatSIBase (Just 2) 1000 (fromIntegral $ CUDA.clockRate prp * 1000 :: Double) "Hz"
    mem         = toLazyText $ Debug.showFFloatSIBase (Just 0) 1024 (fromIntegral $ CUDA.totalGlobalMem prp   :: Double) "B"
    at          = pretty '@'

    go = \case
      SFail              -> panicUncaughtFail
      SEmpty             -> mempty
      SChar c rest       -> TLB.singleton c <> go rest
      SText _l t rest    -> TLB.fromText t <> go rest
      SLine i rest       -> TLB.singleton '\n' <> (TLB.fromText (textSpaces i) <> go rest)
      SAnnPush _ann rest -> go rest
      SAnnPop rest       -> go rest


{-# INLINE message #-}
message :: Format (IO ()) a -> a
message fmt = Debug.traceM Debug.dump_gc ("gc: " % fmt)

{-# INLINE formatContext #-}
formatContext :: Format r (CUDA.Context -> r)
formatContext = later $ \(CUDA.Context c) -> bformat shown c

