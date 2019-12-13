{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Multi.Execute
-- Copyright   : [2014..2015] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
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
import Data.Array.Accelerate.LLVM.Execute
import Data.Array.Accelerate.LLVM.Execute.Marshal
import Data.Array.Accelerate.LLVM.Execute.Environment           ( AvalR(..) )

import Data.Array.Accelerate.LLVM.Multi.Array.Data
import Data.Array.Accelerate.LLVM.Multi.Compile
import Data.Array.Accelerate.LLVM.Multi.Target
import Data.Array.Accelerate.LLVM.Multi.Execute.Async
import Data.Array.Accelerate.LLVM.Multi.Execute.Environment

import Data.Range.Range                                         ( Range(..), bisect )
import Control.Parallel.Meta
import Control.Parallel.Meta.Worker

import Data.Array.Accelerate.Debug

-- accelerate-llvm
import Data.Array.Accelerate.LLVM.PTX.Internal                  ( PTX, Kernel, Async(..) )
import Data.Array.Accelerate.LLVM.Native.Internal               ( Native )
import qualified Data.Array.Accelerate.LLVM.PTX.Internal        as PTX
import qualified Data.Array.Accelerate.LLVM.Native.Internal     as CPU

-- standard library
import Prelude                                                  hiding ( map, mapM_, scanl, scanr )
import Data.Foldable                                            ( mapM_ )
import Control.Concurrent                                       ( runInBoundThread )
import Control.Exception                                        ( bracket_ )
import Control.Monad.State                                      ( gets, liftIO, evalStateT )


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
    -> Kernel
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
          action    = evalStateT (runLLVM f) ptxTarget

      poke from to = runPTX $ copyToRemoteR from to Nothing result
      peek from to = runPTX $ copyToHostR   from to Nothing result

  liftIO . gangIO monitorGang $ \thread ->
    case thread of
      0 -> CPU.executeOp 2048 nativeTarget cpu (syncWith poke) gamma (avalForCPU aval)        u args >> traceIO dump_sched "sched/multi: Native exiting"
      1 -> PTX.executeOp      ptxTarget    ptx (syncWith peek) gamma (avalForPTX aval) stream v args >> traceIO dump_sched "sched/multi: PTX exiting"
      _ -> error "unpossible"


syncWith :: (Int -> Int -> IO ()) -> Finalise
syncWith copy = Finalise $ \_ -> mapM_ (\(IE from to) -> copy from to)


-- Busywork to convert the array environment into a representation specific to
-- each backend.
--
avalForCPU :: Aval aenv -> AvalR Native aenv
avalForCPU Aempty                   = Aempty
avalForCPU (Apush aenv (Async _ a)) = avalForCPU aenv `Apush` a

avalForPTX :: Aval aenv -> AvalR PTX aenv
avalForPTX Aempty         = Aempty
avalForPTX (Apush aenv a) = avalForPTX aenv `Apush` a

