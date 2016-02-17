{-# LANGUAGE CPP                      #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeOperators            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.Execute
-- Copyright   : [2014..2016] Trevor L. McDonell
--               [2014..2014] Vinod Grover (NVIDIA Corporation)
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.Execute (

  executeAcc, executeAfun1,

  executeOp,

) where

-- accelerate
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Analysis.Match

import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Execute

import Data.Array.Accelerate.LLVM.Native.CodeGen.Fold           ( matchShapeType )
import Data.Array.Accelerate.LLVM.Native.Compile
import Data.Array.Accelerate.LLVM.Native.Execute.Async
import Data.Array.Accelerate.LLVM.Native.Execute.Environment
import Data.Array.Accelerate.LLVM.Native.Execute.Marshal
import Data.Array.Accelerate.LLVM.Native.Target

-- Use work-stealing scheduler
import Data.Range.Range                                         ( Range(..) )
import Control.Parallel.Meta                                    ( runExecutable, Finalise(..) )
import Control.Parallel.Meta.Worker                             ( gangSize )
import Data.Array.Accelerate.LLVM.Native.Execute.LBS

-- library
import Data.Monoid                                              ( mempty )
import Data.Word                                                ( Word8 )
import Control.Monad                                            ( when )
import Control.Monad.State                                      ( gets )
import Control.Monad.Trans                                      ( liftIO )
import Prelude                                                  hiding ( map, scanl, scanr, init )
import qualified Data.Sequence                                  as Seq
import qualified Prelude                                        as P

import Foreign.C
import Foreign.Ptr
import Foreign.Storable

#if !MIN_VERSION_llvm_general(3,3,0)
import Data.Word
import Data.Maybe
import qualified LLVM.General.Context                           as LLVM
#endif


-- Array expression evaluation
-- ---------------------------

-- Computations are evaluated by traversing the AST bottom up, and for each node
-- distinguishing between three cases:
--
--  1. If it is a Use node, we return a reference to the array data. Even though
--     we execute with multiple cores, we assume a shared memory multiprocessor
--     machine.
--
--  2. If it is a non-skeleton node, such as a let binding or shape conversion,
--     then execute directly by updating the environment or similar.
--
--  3. If it is a skeleton node, then we need to execute the generated LLVM
--     code.
--
instance Execute Native where
  map           = simpleOp
  generate      = simpleOp
  transform     = simpleOp
  backpermute   = simpleOp
  fold          = foldOp
  fold1         = fold1Op
  -- permute       = permuteOp
  -- scanl1        = scanl1Op
  stencil1      = stencil1Op
  stencil2      = stencil2Op


-- Skeleton implementation
-- -----------------------

-- Inclusive reductions require at least one element in the input array.
--
fold1Op
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> LLVM Native (Array sh e)
fold1Op kernel gamma aenv stream sh@(_ :. sz)
  = $boundsCheck "fold1" "empty array" (sz > 0)
  $ foldOp' kernel gamma aenv stream sh

-- Exclusive reductions will have at least one element in the output array,
-- so make sure the lower-dimensional component is non-empty.
--
foldOp
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> LLVM Native (Array sh e)
foldOp kernel gamma aenv stream (sh :. sz)
  = foldOp' kernel gamma aenv stream ((listToShape . P.map (max 1) . shapeToList $ sh) :. sz)


-- Execute fold operations. There are two flavours:
--
--   1. If we are collapsing to a single value, then the threads compute an
--   individual partial sum, then a single thread adds the results.
--
--   2. If this is a multidimensional reduction, then the inner dimensions
--   are distributed over the threads, which compute each sequentially.
--
foldOp'
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> LLVM Native (Array sh e)
foldOp' native gamma aenv () (sh :. sz)
  -- Reduction to single value
  | Just REFL <- matchShapeType (undefined::sh) (undefined::Z)
  = foldAllCore native gamma aenv () sz

  -- Multidimensional reduction
  | otherwise
  = foldCore native gamma aenv () sh sz


-- Multidimensional reduction
--
foldCore
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh
    -> Int
    -> LLVM Native (Array sh e)
foldCore kernel gamma aenv () sh stride = do
  native <- gets llvmTarget
  liftIO $ do
    out <- allocateArray sh
    executeOp defaultSmallPPT native kernel mempty gamma aenv (IE 0 (size sh)) (stride, out)
    return out


-- Reduce an array to a single element
--
foldAllCore
    :: forall aenv e. Elt e
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> Int
    -> LLVM Native (Scalar e)
foldAllCore kernel@(NativeR mdl) gamma aenv () sz = do
  native@Native{..} <- gets llvmTarget
  --
  liftIO $ if gangSize theGang == 1
    -- Sequential reduction
    then do
      out <- allocateArray Z
      executeOp defaultLargePPT native kernel mempty gamma aenv (IE 0 sz) out
      return out

    -- parallel reduction
    else
      execute mdl "foldAllP1" $ \p1 ->
      execute mdl "foldAllP2" $ \p2 ->
      execute mdl "foldAllP3" $ \p3 -> do
        let w = gangSize theGang
            n = sz `min` w
        --
        flag <- allocateArray (Z :. w) :: IO (Vector Word8)
        tmp  <- allocateArray (Z :. w) :: IO (Vector e)
        out  <- allocateArray Z

        let fptrs = case flag of Array _ adata -> ptrsOfArrayData adata
        memset fptrs 1 w

        let
            -- In the first step 'init', each thread initialises the 'tmp' array
            -- with a local sum. The subsequent 'main' loop is then executed
            -- over the remaining work units, which reads from and updates the
            -- local sum in 'tmp'.
            --
            init start end tid = p1 =<< marshal native () (start,end,tid,tmp,(gamma,aenv))
            main start end tid = p2 =<< marshal native () (start,end,tid,tmp,(gamma,aenv))

            -- There is a tricky race condition in the above, in that even if
            -- a thread is assigned work, it could be stolen by another thread
            -- before it gets around to executing it. Thus, the thread might
            -- never initialise its spot in the 'tmp' array.
            after              = Finalise $ \tid r -> when (Seq.null r) $ pokeElemOff fptrs tid 0
        --
        runExecutable fillP defaultLargePPT (IE 0 sz) after (Just init) main
        p3 =<< marshal native () (0::Int,n,tmp,flag,out,(gamma,aenv))
        --
        return out



{--
-- Make space for the neutral element
foldOp'
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> LLVM Native (Array sh e)
foldOp' kernel gamma aenv stream (sh :. sz)
  = foldCore' kernel gamma aenv stream ((listToShape . P.map (max 1) . shapeToList $ sh) :. sz)

foldCore'
    :: forall aenv sh e. (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> (sh :. Int)
    -> LLVM Native (Array sh e)
foldCore' (NativeR k) gamma aenv () (sh :. sz) = do
  native@Native{..} <- gets llvmTarget

  -- Either (1) multidimensional reduction; or
  --        (2) sequential reduction
  if dim sh > 0 || gangSize theGang == 1
     then let ppt = defaultSmallPPT `max` (defaultLargePPT `div` sz)
          in
          liftIO $ do
            out <- allocateArray sh
            executeFunction k                                 $ \f ->
              runExecutable fillP ppt (IE 0 (size sh)) mempty $ \start end _ ->
                callFFI f retVoid =<< marshal native () (start, end, sz, out, (gamma,aenv))

            return out

  -- Parallel reduction
     else let chunks = gangSize theGang
              n      = sz `min` chunks
          in
          liftIO $ do
            tmp <- allocateArray (sh :. chunks)         :: IO (Array (sh:.Int) e)
            out <- allocateArray sh
            executeNamedFunction k "fold1"                                     $ \f ->
              runExecutable fillP defaultLargePPT (IE 0 (size sh * sz)) mempty $ \start end tid ->
                callFFI f retVoid =<< marshal native () (start,end,tid,tmp,(gamma,aenv))

            executeNamedFunction k "foldAll" $ \f ->
              callFFI f retVoid =<< marshal native () (0::Int,n,tmp,out,(gamma,aenv))

            return out
--}


{--
-- Forward permutation, specified by an indexing mapping into an array and a
-- combination function to combine elements.
--
permuteOp
    :: (Shape sh, Shape sh', Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh
    -> Array sh' e
    -> LLVM Native (Array sh' e)
permuteOp kernel gamma aenv () shIn dfs = do
  let n                         = size (shape dfs)
      unlocked                  = 0
  --
  out    <- cloneArray dfs
  native <- gets llvmTarget
  liftIO $ do
    barrier@(Array _ adata) <- liftIO $ allocateArray (Z :. n)  :: IO (Vector Word8)
    memset (ptrsOfArrayData adata) unlocked n
    executeOp native kernel mempty gamma aenv (IE 0 (size shIn)) (barrier, out)
  return out
--}
{--
-- Left inclusive scan
--
scanl1Op
    :: forall aenv e. Elt e
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> DIM1
    -> LLVM Native (Vector e)
scanl1Op (NativeR k) gamma aenv () (Z :. sz) = do
  native@Native{..} <- gets llvmTarget

  -- sequential reduction
  if gangSize theGang == 1 || sz < defaultLargePPT
     then liftIO $ do
            out <- allocateArray (Z :. sz)
            executeNamedFunction k "scanl1Seq" $ \f ->
              callFFI f retVoid =<< marshal native () (0::Int, sz, out, (gamma,aenv))

            return out

  -- Parallel reduction
     else let chunkSize = defaultLargePPT
              chunks    = sz `div` chunkSize
          in
          liftIO $ do
            tmp <- allocateArray (Z :. (chunks-1))      :: IO (Vector e)
            out <- allocateArray (Z :. sz)

            executeNamedFunction k "scanl1Pre"           $ \f -> do
              runExecutable fillP 1 (IE 0 chunks) mempty $ \start end _ -> do
                callFFI f retVoid =<< marshal native () (start,end,chunkSize,tmp,(gamma,aenv))

            executeNamedFunction k "scanl1Post"          $ \f ->
              runExecutable fillP 1 (IE 0 chunks) mempty $ \start end _ -> do
                callFFI f retVoid =<< marshal native () (start,end,(chunks-1),chunkSize,sz,tmp,out,(gamma,aenv))

            return out
--}

stencil1Op
    :: (Shape sh, Elt a, Elt b)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> Array sh a
    -> LLVM Native (Array sh b)
stencil1Op kernel gamma aenv stream arr
  = simpleOp kernel gamma aenv stream (shape arr)

stencil2Op
    :: (Shape sh, Elt a, Elt b, Elt c)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> Array sh a
    -> Array sh b
    -> LLVM Native (Array sh c)
stencil2Op kernel gamma aenv stream arr brr
  = simpleOp kernel gamma aenv stream (shape arr `intersect` shape brr)


-- Skeleton execution
-- ------------------

-- Simple kernels just needs to know the shape of the output array.
--
simpleOp
    :: (Shape sh, Elt e)
    => ExecutableR Native
    -> Gamma aenv
    -> Aval aenv
    -> Stream
    -> sh
    -> LLVM Native (Array sh e)
simpleOp kernel gamma aenv () sh = do
  native <- gets llvmTarget
  liftIO $ do
    out <- allocateArray sh
    executeOp defaultLargePPT native kernel mempty gamma aenv (IE 0 (size sh)) out
    return out


-- JIT compile the LLVM code representing this kernel, link to the running
-- executable, and execute the main function using the 'fillP' method to
-- distribute work evenly amongst the threads.
--
executeOp
    :: Marshalable args
    => Int
    -> Native
    -> ExecutableR Native
    -> Finalise
    -> Gamma aenv
    -> Aval aenv
    -> Range
    -> args
    -> IO ()
executeOp ppt native@Native{..} NativeR{..} finish gamma aenv r args =
  executeMain executableR                  $ \f              ->
  runExecutable fillP ppt r finish Nothing $ \start end _tid ->
    f =<< marshal native () (start, end, args, (gamma, aenv))


-- Standard C functions
-- --------------------

memset :: Ptr Word8 -> Word8 -> Int -> IO ()
memset p w s = c_memset p (fromIntegral w) (fromIntegral s) >> return ()

foreign import ccall unsafe "string.h memset" c_memset
    :: Ptr Word8 -> CInt -> CSize -> IO (Ptr Word8)

