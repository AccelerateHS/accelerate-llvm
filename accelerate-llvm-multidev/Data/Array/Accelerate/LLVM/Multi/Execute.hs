{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Multi.Execute
-- Copyright   : [2014] Trevor L. McDonell, Sean Lee, Vinod Grover, NVIDIA Corporation
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Multi.Execute (

  executeAcc, executeAfun1,

) where

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Array, Shape, Elt, size )
import Data.Array.Accelerate.Error                              ( internalError )

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Target
import Data.Array.Accelerate.LLVM.Execute
import Data.Array.Accelerate.LLVM.Execute.Marshal
import Data.Array.Accelerate.LLVM.Execute.Environment           ( AvalR(..) )

import Data.Array.Accelerate.LLVM.Multi.Array.Data
import Data.Array.Accelerate.LLVM.Multi.Target
import Data.Array.Accelerate.LLVM.Multi.Execute.Async
import Data.Array.Accelerate.LLVM.Multi.Execute.Environment

import Data.Range.Range                                         ( Range(..), bisect )
import Control.Parallel.Meta
import Control.Parallel.Meta.Worker

import Data.Array.Accelerate.LLVM.Debug

-- accelerate-llvm-ptx
import Data.Array.Accelerate.LLVM.PTX.Target                    ( PTX, ptxKernel )
import Data.Array.Accelerate.LLVM.PTX.Execute.Async             ( Async(..) )
import qualified Data.Array.Accelerate.LLVM.PTX.Target          as PTX
import qualified Data.Array.Accelerate.LLVM.PTX.Context         as PTX
import qualified Data.Array.Accelerate.LLVM.PTX.Execute         as PTX

-- accelerate-llvm-native
import Data.Array.Accelerate.LLVM.Native.Target                 ( Native )
import qualified Data.Array.Accelerate.LLVM.Native.Execute      as Native

-- standard library
import Prelude                                                  hiding ( map, mapM_, scanl, scanr )
import Data.Foldable                                            ( mapM_ )
import Control.Concurrent                                       ( runInBoundThread )
import Control.Exception                                        ( bracket_ )
import Control.Monad.State                                      ( gets, liftIO, evalStateT )
import Control.Monad.Reader                                     ( runReaderT )
import System.IO.Unsafe


instance Execute Multi where
  map           = simpleOp
  generate      = simpleOp
  transform     = simpleOp
  backpermute   = simpleOp


-- Cooperatively run the default executable for each of the backends
--
simpleOp
    :: (Shape sh, Elt e)
    => ExecutableR Multi
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh
    -> LLVM Multi (Array sh e)
simpleOp MultiR{..} gamma aenv stream sh = do
  let
      ptx = case ptxKernel ptxExecutable of
              k:_ -> k
              _   -> $internalError "execute" "kernel not found"
  --
  out   <- allocateRemote sh
  multi <- gets llvmTarget
  liftIO $ executeOp multi nativeExecutable ptx gamma aenv stream (size sh) out out
  return out


executeOp
    :: (Marshalable Native args, Marshalable PTX args)
    => Multi
    -> ExecutableR Native
    -> PTX.Kernel
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> Int
    -> args
    -> Array sh e
    -> IO ()
executeOp Multi{..} cpu ptx gamma aval stream n args result = do
  let -- Initialise each backend with an equal portion of work
      (u,v)     = bisect (IE 0 n)

      runPTX :: LLVM PTX () -> IO ()
      runPTX f = runInBoundThread (bracket_ setup teardown action)
        where
          setup     = PTX.push (PTX.ptxContext ptxTarget)
          teardown  = PTX.pop
          action    = evalStateT (runReaderT (runLLVM f) undefined) ptxTarget

      poke from to = runPTX $ copyToRemoteR from to Nothing result
      peek from to = runPTX $ copyToHostR   from to Nothing result

  liftIO . gangIO theGang $ \thread ->
    case thread of
      0 -> Native.executeOp nativeTarget cpu (syncWith poke) gamma (avalForNative aval)        u args >> message dump_sched "sched/multi: Native exiting"
      1 -> PTX.executeOp    ptxTarget    ptx (syncWith peek) gamma (avalForPTX    aval) stream v args >> message dump_sched "sched/multi: PTX exiting"
      _ -> error "unpossible"


syncWith :: (Int -> Int -> IO ()) -> Finalise
syncWith copy = Finalise $ mapM_ (\(IE from to) -> copy from to)


-- Busywork to convert the array environment into a representation specific to
-- each backend.
--
avalForNative :: Aval aenv -> AvalR Native aenv
avalForNative Aempty                   = Aempty
avalForNative (Apush aenv (Async _ a)) = avalForNative aenv `Apush` a

avalForPTX :: Aval aenv -> AvalR PTX aenv
avalForPTX Aempty         = Aempty
avalForPTX (Apush aenv a) = avalForPTX aenv `Apush` a


-- A simple worker gang whose only job is to dispatch work to either the CPU or
-- GPU backend. Some lighter-weight concurrency would do as well, but we have
-- this built already...
--
{-# NOINLINE theGang #-}
theGang :: Gang
theGang = unsafePerformIO $ forkGang 2

