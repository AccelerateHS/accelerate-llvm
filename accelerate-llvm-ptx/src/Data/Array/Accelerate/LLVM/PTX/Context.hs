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

import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Lifetime
import Data.Array.Accelerate.LLVM.PTX.Analysis.Device
import qualified Data.Array.Accelerate.LLVM.PTX.Debug               as Debug

import qualified Foreign.CUDA.Driver.Device                         as CUDA
import qualified Foreign.CUDA.Driver.Context                        as CUDA

import Control.Concurrent
import Control.Concurrent.STM
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

  -- | The number of finalizers currently using the context to free resources.
  -- While this number is > 0, the CUDA.Context must not be destroyed. The
  -- finalizer for the CUDA.Context sets this counter to Nothing to indicate
  -- that the context will be destroyed anyhow, and resources in this context
  -- need not be explicitly finalized any more.
  -- See Note: [Finalizing a CUDA Context].
  , deviceFinalizerRefcount :: {-# UNPACK #-} !(TVar (Maybe Int))
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
raw dev prp ctx@(CUDA.Context ptr) = do
  refcountVar <- newTVarIO (Just 0)  -- number of finalizers currently using this context to free resources; see 'Context'

  lft <- newLifetime ctx
  addFinalizer lft $ do
    message ("finalise context " % formatContext) ctx
    _ <- forkIO $ do
      -- wait until the reference count reaches zero; see Note: [Finalizing a CUDA Context]
      atomically $ do
        count <- readTVar refcountVar
        case count of
          Just n | n > 0 -> retry  -- wait until context is unused by finalizers
                 | n == 0 -> return ()
                 | otherwise -> internalError ("CUDA context refcount < 0 (ptr=" % shown % ", c=" % shown % ")") ptr n
          Nothing -> internalError ("CUDA context finalized twice? (ptr=" % shown % ")") ptr
        -- mark the context as dead; no more finalizers need run as resources will be freed now anyway
        writeTVar refcountVar Nothing
      CUDA.destroy ctx
    return ()

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

  return $! Context prp nm lft refcountVar


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
contextFinalizeResource Context{deviceFinalizerRefcount = var} action =
  -- See Note: [Finalizing a CUDA Context]
  bracket
    (atomically $ do
      count <- readTVar var
      case count of
        Just n -> do
          writeTVar var (Just $! n + 1)
          return True
        Nothing ->
          -- context slated for deletion, no need to free resources in it any more
          return False)
    (\shouldFree ->
      when shouldFree $ do
        ok <- atomically $ do
          count <- readTVar var
          case count of
            Just n -> do
              writeTVar var (Just $! n - 1)
              return True
            Nothing -> return False
        when (not ok) $
          internalError "CUDA Context refcount became Nothing while incremented!")
    (\shouldFree -> when shouldFree action)


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
-- (currently, arrays and executable modules) are freed with finalizers; these
-- are invoked by the GC when it detects (after a GC pass) that the Haskell
-- heap objects are no longer reachable.
--
-- Finalizers are attached to ForeignPtrs. The problem is that even if a
-- finalizer for ForeignPtr 1 refers to ForeignPtr 2, the GC does not guarantee
-- that the finalizer for FP 1 runs to completion before the finalizer of FP 2
-- starts. (See the documentation for 'touchForeignPtr' in base.) This is a
-- problem for us because we are in this situation: to free a resource within a
-- CUDA context, we need a reference to that context, and the context needs to
-- be alive. Thus finalizers for e.g. arrays reference the FP for the CUDA
-- context.
--
-- If we just leave GHC to do finalization as it wishes, this means that a CUDA
-- context may well be destroyed before the resourcees in it are finalized,
-- leading to use-after-free errors and segfaults. We have a choice here:
-- 1. either we let the finalizer for a Context wait until the other finalizers
--    have run, or
-- 2. we let the resource finalizers check if the Context has already been
--    destroyed, and if so, do nothing.
-- We choose option 2 because destroying a CUDA context already frees the
-- resources in it, so there is no need to do meticulous manual cleanup here.
--
-- To accomplish this, a 'Context' contains a reference count (in a TVar) that
-- indicates how many resource finalizers are currently using the context to do
-- their freeing work. The Context finalizer blocks (in a forkIO thread,
-- because finalizers can be run sequentially and we must avoid deadlock -- not
-- sure if this is necessary but let's be safe) until this reference count is 0
-- before destroying the CUDA context. When zero is reached, it communicates
-- that no more resource freeing is necessary from this point by setting the
-- reference count to Nothing.
--
-- A resource finalizer then uses 'contextFinalizeResource', which does the
-- incrementing and decrementing and avoids doing anything if the reference
-- count is Nothing already. After the reference count has become Nothing, it
-- will never become Just again.

