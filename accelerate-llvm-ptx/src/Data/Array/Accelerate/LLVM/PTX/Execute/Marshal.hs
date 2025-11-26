{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Execute.Marshal
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Execute.Marshal (

  module Data.Array.Accelerate.LLVM.Execute.Marshal

) where

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Execute.Marshal

import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.PTX.Execute.Async
import qualified Data.Array.Accelerate.LLVM.PTX.Array.Prim      as Prim

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Data

import qualified Foreign.CUDA.Driver                            as CUDA

import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import qualified Data.DList                                     as DL


instance Marshal PTX where
  type ArgR PTX = CUDA.FunParam
  type MarshalCleanup PTX = IO ()

  marshalInt = CUDA.VArg
  marshalScalarData' t
    | SingleArrayDict <- singleArrayDict t
    = liftPar . fmap (\(ptr, cleanup) -> (DL.singleton (CUDA.VArg ptr), cleanup)) . getCudaDevicePtr t

-- | Return the CUDA device pointer corresponding to the given array, as well
-- as a cleanup IO action that __MUST__ be run once you are done with the
-- pointer (i.e. the GPU kernel has completed). Not calling the cleanup action
-- will result in leaked memory and resources. Calling the action twice will
-- block indefinitely on an MVar.
--
-- This function is a hack. Prim.withDevicePtr is intended to be a wrapping
-- function that retains the resource while the callback is running and
-- releases it when the callback returns. This is all nice, but since the PTX
-- Accelerate runtime is asynchronous, uses of withDevicePtr would not all be
-- neatly nested: the actual array lifetimes are haphazard intervals during
-- program execution.
--
-- Originally, this function just gave up and extracted the DevicePtr by
-- calling withDevicePtr with a trivial body that simply leaks p; this is
-- unsound (which was acknowledged by a 'fixme' comment...) and appears to have
-- been the cause of silent incorrect results (!) on a GTX 1050 Ti on the
-- adbench-gmmgrad test in accelerate-tests [1].
--
-- [1]: https://github.com/tomsmeding/accelerate-tests/blob/master/src/Data/Array/Accelerate/Tests/Prog/ADBenchGMMGrad.hs
--
-- Fortunately, it turns out that the MemoryTable implementation underlying
-- withDevicePtr does not in fact assume lexical nesting of array usages. Thus
-- we can use a hack to let the callback of withDevicePtr live for the correct
-- amount of time without needing to rearchitect the entire PTX backend: let
-- the call run in a forkIO thread and use MVars to communicate when it should
-- return. This means that we now have the possibility to return a
-- self-contained "cleanup" handler from getCudaDevicePtr that does nothing but
-- signal to the withDevicePtr callback that the array's lifetime has ended and
-- the scope can close. All this is possible because the 'LLVM' monad is just a
-- reader monad over IO, so we can unlift it into IO.
--
-- As a final, questionable improvement, we let the "cleanup" handler wait
-- until withDevicePtr has properly returned so that we know the array's
-- refcount has been properly decremented and memory has been released if
-- possible.
getCudaDevicePtr
    :: SingleType e
    -> ArrayData e
    -> LLVM PTX (CUDA.DevicePtr (ScalarArrayDataR e), IO ())
getCudaDevicePtr !t !ad = do
  ptrVar <- liftIO newEmptyMVar
  doneVar <- liftIO newEmptyMVar
  releasedVar <- liftIO newEmptyMVar

  _ <- unliftIOLLVM $ \inLLVM -> forkIO $ inLLVM $ do
    Prim.withDevicePtr t ad $ \p -> liftIO $ do
      putMVar ptrVar p
      takeMVar doneVar
      return (Nothing, ())
    liftIO $ putMVar releasedVar ()

  ptr <- liftIO $ readMVar ptrVar
  return (ptr, putMVar doneVar () >> readMVar releasedVar)

