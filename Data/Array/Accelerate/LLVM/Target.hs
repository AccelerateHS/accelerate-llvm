{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies              #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Target
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Target
  where

-- llvm-general
import LLVM.General.AST.DataLayout                      ( DataLayout )

-- accelerate
import Data.Array.Accelerate.LLVM.State                 ( LLVM )
import Data.Array.Accelerate.LLVM.CodeGen.Module        ( Module )


-- | Describes some target specific information needed for code generation
--
class Target t where
  data ExecutableR t
  targetTriple          :: t {- dummy -} -> Maybe String
  targetDataLayout      :: t {- dummy -} -> Maybe DataLayout

  -- | Given an LLVM module and the name of the functions that are defined in
  -- that module, compile the module into something executable on this target.
  --
  -- Note that llvm-general functions with* will clean up the argument to the
  -- continuation after it exits, meaning that it can not be returned from the
  -- operation; i.e. the following is not allowed:
  --
  --    withModuleFromAST m return
  --
  compileForTarget      :: Module t aenv a -> LLVM (ExecutableR t)


-- | A dummy instance that just carries the LLVM Module. This is can be useful
-- for debugging, etc.
--
data LL
instance Target LL where
  data ExecutableR LL = forall aenv a. LL (Module LL aenv a)
  targetTriple _     = Nothing
  targetDataLayout _ = Nothing
  compileForTarget m = return (LL m)

