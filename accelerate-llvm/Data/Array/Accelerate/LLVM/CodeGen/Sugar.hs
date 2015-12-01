{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Sugar
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Sugar (

  IRExp, IRFun1, IRFun2,
  IROpenExp, IROpenFun1(..), IROpenFun2(..),
  IROpenAcc(..), IRDelayed(..), IRManifest(..),

  IRArray(..),

) where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Module
import {-# SOURCE #-} Data.Array.Accelerate.LLVM.CodeGen.Monad


-- | LLVM IR is in single static assignment, so we need to be able to generate
-- fresh names for each application of a scalar function or expression.
--
type IRExp     arch     aenv t = IROpenExp arch () aenv t
type IROpenExp arch env aenv t = CodeGen (IR t)

type IRFun1 arch aenv t = IROpenFun1 arch () aenv t
type IRFun2 arch aenv t = IROpenFun2 arch () aenv t

data IROpenFun1 arch env aenv t where
  IRFun1 :: { app1 :: IR a -> IROpenExp arch (env,a) aenv b }
         -> IROpenFun1 arch env aenv (a -> b)

data IROpenFun2 arch env aenv t where
  IRFun2 :: { app2 :: IR a -> IR b -> IROpenExp arch ((env,a),b) aenv c }
         -> IROpenFun2 arch env aenv (a -> b -> c)

data IROpenAcc arch aenv arrs where
  IROpenAcc :: [Kernel arch aenv a]             -- TLM: ???
            -> IROpenAcc arch aenv arrs

data IRDelayed arch aenv a where
  IRDelayed :: (Shape sh, Elt e) =>
    { delayedExtent      :: IRExp  arch aenv sh
    , delayedIndex       :: IRFun1 arch aenv (sh -> e)
    , delayedLinearIndex :: IRFun1 arch aenv (Int -> e)
    }
    -> IRDelayed arch aenv (Array sh e)

data IRManifest arch aenv a where
  IRManifest :: Arrays arrs => Idx aenv arrs -> IRManifest arch aenv arrs


data IRArray a where
  IRArray :: (Shape sh, Elt e)
          => { irArrayShape :: IR sh
             , irArrayData  :: IR e             -- TLM: local operand name for array(s) containing elements of type 'e'
             }
          -> IRArray (Array sh e)

