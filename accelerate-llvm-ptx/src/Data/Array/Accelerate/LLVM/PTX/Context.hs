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
  contextFinalizeResource,

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
import Data.IORef
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

  -- | The number of finalizers currently using the context to free resources,
  -- plus 1 if the Context finalizer does not yet want to destroy the context
  -- itself. See Note: [Finalizing a CUDA Context].
  , deviceFinalizerRefcount :: {-# UNPACK #-} !(IORef Int)
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
  refcountVar <- newIORef 1  -- there is one user of the context: the context itself

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

  lft <- newLifetime ctx  -- on the CUDA context
  let !result = Context prp nm lft refcountVar
  addFinalizer lft $ decrementContext result

  return result


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

decrementContext :: Context -> IO ()
decrementContext ctx = do
  newCount <- atomicModifyIORef' (deviceFinalizerRefcount ctx) (\i -> (i - 1, i - 1))
  message ("decrement context " % formatContext % " to " % shown) (unsafeGetValue (deviceContext ctx)) newCount
  when (newCount == 0) $ CUDA.destroy (unsafeGetValue (deviceContext ctx))

-- | If the underlying CUDA context is already slated for destruction entirely
-- (or has already been destroyed) by its finalizer, this function does
-- nothing. If the CUDA context will live on (for now), the passed @IO@ action
-- is invoked with a lock held so that the CUDA context will not be destroyed
-- while your action runs. Use this in finalizers of CUDA resources such as
-- arrays and linked modules.
--
-- The context is not automatically pushed in the action; if you need to
-- 'withContext', do it yourself.
contextFinalizeResource :: Context -> IO () -> IO ()
contextFinalizeResource ctx action =
  -- See Note: [Finalizing a CUDA Context]
  bracket
    (do newCount <- atomicModifyIORef' (deviceFinalizerRefcount ctx) $ \i ->
                      if i == 0 then (0, 0) else (i + 1, i + 1)
        message ("increment context " % formatContext % " to " % shown) (unsafeGetValue (deviceContext ctx)) newCount
        return (newCount > 0))
    (\contextStillLive ->
        when contextStillLive (decrementContext ctx))
    (\contextStillLive ->
        when contextStillLive action)


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


-- Note: [Finalizing a CUDA Context]
--
-- Both a CUDA context and the resources we allocate within such a context
-- (currently, arrays, executable modules and events) are freed with
-- finalizers; these are invoked by the GC when it detects (after a GC pass)
-- that the Haskell heap objects are no longer reachable.
--
-- In our case, finalizers are attached to 'Lifetime' objects. The problem is
-- that even if a finalizer for Lifetime 1 refers to Lifetime 2, the GC does
-- not guarantee that the finalizer for Lifetime 1 runs to completion before the
-- finalizer of Lifetime 2 starts. (See the documentation for 'touchForeignPtr'
-- in base.) This is a problem for us because we are in this situation: to free
-- a resource within a CUDA context, we need a reference to that context and
-- the context needs to be alive. Thus finalizers for e.g. arrays reference the
-- Lifetime for the CUDA context.
--
-- If we just leave GHC to do finalization as it wishes, this means that a CUDA
-- context may well be destroyed before the resourcees in it are finalized,
-- leading to use-after-free errors and segfaults. We have two choices here:
-- 1. either we let the finalizer for a Context wait until the other finalizers
--    have run, or
-- 2. we free the Context when first we can, and let resource finalizers that
--    come later, do nothing.
-- We choose option 2 because destroying a CUDA context already frees the
-- resources in it, so there is no need to do meticulous manual cleanup here.
--
-- To accomplish this, we use reference counting. The context itself being
-- alive (precisely: its finalizer not yet having run) counts for one "use";
-- the only other "uses" are the finalizers for the CUDA resources in the
-- context. (There is no need to track anything before we start finalizing, so
-- this is enough.) The Context finalizer does nothing more than a decrement on
-- the refcount to release the "use" of the CUDA context by the Context object
-- itself.
--
-- To complete the picture:
-- - When decrementing, if we decremented to zero, we destroy the CUDA context.
-- - When incrementing, we are apparently in a resource finalizer, because that
--   is the only place where we increment. If here we see that the refcount is
--   already zero, we simply do nothing (and *skip* the resource finalizer
--   entirely): the CUDA context has already been destroyed, taking this
--   resource with it, so nothing more needs to be done.
--
-- The resource finalizers use 'contextFinalizeResource' to access this
-- functionality.

