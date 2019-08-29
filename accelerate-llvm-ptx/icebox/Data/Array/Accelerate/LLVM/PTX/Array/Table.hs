{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Array.Table
-- Copyright   : [2014..2015] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Array.Table (

  -- MemoryTable,
  -- new, member, lookup, malloc,

) where

-- accelerate
import Data.Array.Accelerate.Array.Data

import qualified Data.Array.Accelerate.LLVM.Array.Table         as MT
import qualified Data.Array.Accelerate.LLVM.PTX.Debug           as Debug

import Data.Array.Accelerate.LLVM.PTX.Context                  ( Context(..) )

-- CUDA
import Foreign.CUDA.Driver.Error
import qualified Foreign.CUDA.Driver                            as CUDA

-- standard library
import Prelude                                                  hiding ( lookup )
import Control.Exception
import Data.Typeable
import Foreign.Ptr
import Foreign.Storable
import System.Mem.Weak


-- Memory management for the PTX backend utilises the memory table method
--
type MemoryTable = MT.MemoryTable CUDA.DevicePtr


-- | Create a new PTX memory table. This is specific to a given PTX target, as
-- devices arrays are unique to a CUDA context.
--
new :: Context -> IO MemoryTable
new !ctx = MT.new (freeRemote ctx)


-- | Does the host array have a valid entry in the table?
--
member
    :: (ArrayElt e, ArrayPtrs e ~ Ptr a)
    => MemoryTable
    -> ArrayData e
    -> IO Bool
member = MT.member


-- | Lookup the remote array corresponding to the given host-side array
--
lookup
    :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable a, Typeable b)
    => MemoryTable
    -> ArrayData e
    -> IO (Maybe (CUDA.DevicePtr b))
lookup = MT.lookup


-- | Allocate new array on the device. If the allocation fails, attempt to
-- reclaim any stale memory and attempt the allocation again.
--
malloc
    :: (ArrayElt e, ArrayPtrs e ~ Ptr a, Typeable a, Storable a)
    => Context
    -> MemoryTable
    -> ArrayData e
    -> Int
    -> IO (CUDA.DevicePtr a)
malloc !ctx !mt = MT.malloc (freeRemote ctx) (mallocRemote ctx mt) mt

mallocRemote
    :: Storable a
    => Context
    -> MemoryTable
    -> Int
    -> IO (CUDA.DevicePtr a)
mallocRemote !ctx !mt !n =
  CUDA.mallocArray n `catch` \(e :: CUDAException) ->
    case e of
      ExitCode OutOfMemory -> cleanup ctx mt >> CUDA.mallocArray n
      _                    -> throwIO e


-- | Delete an array from the PTX context. Since there may be multiple contexts
-- being used concurrently, we must instantiate the specific context this array
-- exists in before attempting the deallocation.
--
freeRemote :: Context -> CUDA.DevicePtr a -> IO ()
freeRemote !Context{..} !ptr = do
  mc <- deRefWeak weakContext
  case mc of
    Nothing     -> message "free/dead context"
    Just ctx    -> bracket_ (CUDA.push ctx) CUDA.pop (CUDA.free ptr)


-- | Cleanup any stale device memory.
--
cleanup :: Context -> MemoryTable -> IO ()
cleanup !ctx !mt = do
  message "ptx table clean"
  (free,total)  <- CUDA.getMemInfo
  MT.cleanup (freeRemote ctx) mt
  Debug.when Debug.dump_gc $ do
    (free', _)  <- CUDA.getMemInfo
    message $ "cleanup: freed "   ++ showBytes (fromIntegral (free - free'))
                        ++ ", "   ++ showBytes (fromIntegral free')
                        ++ " of " ++ showBytes (fromIntegral total) ++ " remaining"


-- Debug
-- -----

{-# INLINE showBytes #-}
showBytes :: Int -> String
showBytes x = Debug.showFFloatSIBase (Just 0) 1024 (fromIntegral x :: Double) "B"

{-# INLINE trace #-}
trace :: String -> IO a -> IO a
trace msg next = Debug.traceIO Debug.dump_gc ("gc: " ++ msg) >> next

{-# INLINE message #-}
message :: String -> IO ()
message s = s `trace` return ()

