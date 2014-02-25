{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Target
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Target
  where

-- llvm-general
import LLVM.General.AST.DataLayout                              ( DataLayout )

-- accelerate
import Data.Array.Accelerate.Trafo                              ( DelayedOpenAcc )
--import Data.Array.Accelerate.Array.Sugar                        ( Arrays )

--import Data.Array.Accelerate.LLVM.AST                         ( ExecAcc )
import Data.Array.Accelerate.LLVM.State                         ( LLVM )
import Data.Array.Accelerate.LLVM.CodeGen.Environment           ( Gamma )


-- | Describes some target specific information needed for code generation
--
class Target t where
  data ExecutableR t
  targetTriple          :: t {- dummy -} -> Maybe String
  targetDataLayout      :: t {- dummy -} -> Maybe DataLayout

  -- | Compile the given array program for this backend
  compileForTarget      :: DelayedOpenAcc aenv a -> Gamma aenv -> LLVM (ExecutableR t)

  -- | Execute the annotated array program in this target architecture
--  executeAccForTarget   :: Arrays a => ExecAcc t a -> LLVM a


-- | A dummy instance that just carries the LLVM Module. This can be useful for
-- debugging, etc.
--
--data LL
--instance Target LL where
--  data ExecutableR LL = forall aenv a. LL (Module LL aenv a)
--  targetTriple _     = Nothing
--  targetDataLayout _ = Nothing
--  compileForTarget m = return (LL m)


data NonExecutable t
instance Target t => Target (NonExecutable t) where
  data ExecutableR (NonExecutable t) = NonExecutable (ExecutableR t)
  targetTriple _     = targetTriple     (undefined::t)
  targetDataLayout _ = targetDataLayout (undefined::t)

  compileForTarget acc aenv     = NonExecutable `fmap` compileForTarget acc aenv
--  executeAccForTarget _         = error "execution disabled"

