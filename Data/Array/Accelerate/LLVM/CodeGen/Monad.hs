{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Monad
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--


module Data.Array.Accelerate.LLVM.CodeGen.Monad
  where

-- standard library
import Data.Word
import Data.Typeable
import Control.Applicative
import Control.Monad.State
import Data.Sequence                                    ( Seq )
import qualified Data.Sequence                          as Seq

-- llvm-general
import LLVM.General.AST


-- | The code generation state of a scalar function.
--
-- As Accelerate expressions are converted into a sequence of operations, the
-- (named) instructions are added to the sequence `cgf_instructions`. The
-- instruction list is converted to a new basic block and added to `cgf_blocks`
-- whenever we need a new block (to define loops and control flow, etc.?).
--
-- It also includes a counter to generate fresh names. Note that we don't use
-- unnamed (`UnName`) variables, since we don't know how many preceding
-- instructions were defined in the skeleton code that this function is
-- eventually spliced into (unnamed variables must count sequentially in the
-- LLVM IR). Additionally all `Named` instructions we generate produce names
-- (i.e. no `Do` calls to functions returning void).
--
data CGFState = CGFState
  { cgf_blocks          :: !(Seq BasicBlock)
  , cgf_instructions    :: !(Seq (Named Instruction))
  , cgf_next            :: {-# UNPACK #-} !Word
  }
  deriving (Typeable, Show)

newtype CodeGenFunction a = CGF { runCodeGen :: State CGFState a }
  deriving (Functor, Applicative, Monad, MonadState CGFState, Typeable)


generateFunctionCode :: CodeGenFunction a -> (a, CGFState)
generateFunctionCode f = runState (runCodeGen f) (CGFState Seq.empty Seq.empty 0)


-- | Generate a fresh unnamed node.
--
--freshName :: CodeGenFunction Name
--freshName = state $ \s@CGFState{..} ->
--  ( UnName cgf_next, s { cgf_next = cgf_next + 1 })


-- | Add an instruction to the state so that it is computed, and return the
-- operand (LocalReference) that can be used to later refer to it.
--
instr :: Instruction -> CodeGenFunction Operand
instr op = state $ \s@CGFState{..} ->
  let name = UnName cgf_next
  in
  ( LocalReference name
  , s { cgf_instructions = cgf_instructions Seq.|> name := op
      , cgf_next         = cgf_next + 1
      })

