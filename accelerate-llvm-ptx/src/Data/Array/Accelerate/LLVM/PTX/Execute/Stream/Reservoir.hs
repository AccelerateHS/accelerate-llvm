{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Execute.Stream.Reservoir
-- Copyright   : [2016..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Execute.Stream.Reservoir (

  Reservoir,
  new, malloc, insert,

) where

import Data.Array.Accelerate.LLVM.PTX.Context                       ( Context )
import qualified Data.Array.Accelerate.LLVM.PTX.Debug               as Debug

import Control.Concurrent.MVar
import Data.Sequence                                                ( Seq )
import qualified Data.Sequence                                      as Seq
import qualified Foreign.CUDA.Driver.Stream                         as Stream


-- | The reservoir is a place to store CUDA execution streams that are currently
-- inactive. When a new stream is requested one is provided from the reservoir
-- if available, otherwise a fresh execution stream is created.
--
type Reservoir = MVar (Seq Stream.Stream)


-- | Generate a new empty reservoir. It is not necessary to pre-populate it with
-- any streams because stream creation does not cause a device synchronisation.
--
-- Additionally, we do not need to finalise any of the streams. A reservoir is
-- tied to a specific execution context, so when the reservoir dies it is
-- because the PTX state and contained CUDA context have died, so there is
-- nothing more to do.
--
{-# INLINEABLE new #-}
new :: Context -> IO Reservoir
new _ctx = newMVar Seq.empty


-- | Retrieve an execution stream from the reservoir, if one is available.
--
-- Since we put streams back onto the reservoir once we have finished adding
-- work to them, not once they have completed execution of the tasks, we must
-- check for one which has actually completed.
--
-- See note: [Finalising execution streams]
--
{-# INLINEABLE malloc #-}
malloc :: Reservoir -> IO (Maybe Stream.Stream)
malloc !ref =
  modifyMVar ref (search Seq.empty)
  where
    -- scan through the streams in the reservoir looking for the first inactive
    -- one. Optimistically adding the streams to the end of the reservoir as
    -- soon as we stop assigning new work to them (c.f. async), and just
    -- checking they have completed before reusing them, is quicker than having
    -- a finaliser thread block until completion before retiring them.
    --
    search !acc !rsv =
      case Seq.viewl rsv of
        Seq.EmptyL  -> return (acc, Nothing)
        s Seq.:< ss -> do
          done <- Stream.finished s
          case done of
            True  -> return (acc Seq.>< ss, Just s)
            False -> search (acc Seq.|> s) ss


-- | Add a stream to the reservoir
--
{-# INLINEABLE insert #-}
insert :: Reservoir -> Stream.Stream -> IO ()
insert !ref !stream = do
  message ("stash stream " ++ showStream stream)
  modifyMVar_ ref $ \rsv -> return (rsv Seq.|> stream)


-- Debug
-- -----

{-# INLINE trace #-}
trace :: String -> IO a -> IO a
trace msg next = do
  Debug.traceIO Debug.dump_sched ("stream: " ++ msg)
  next

{-# INLINE message #-}
message :: String -> IO ()
message s = s `trace` return ()

{-# INLINE showStream #-}
showStream :: Stream.Stream -> String
showStream (Stream.Stream s) = show s

