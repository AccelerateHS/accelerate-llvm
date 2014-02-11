{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies   #-}
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
import LLVM.General                                     ( Module )
import LLVM.General.AST.Name                            ( Name )
import LLVM.General.AST.DataLayout                      ( DataLayout )

-- accelerate
import Data.Array.Accelerate.LLVM.State


-- | Describes some target specific information needed for code generation
--
class Target t where
  data ExecutableR t
  targetTriple          :: t {- dummy -} -> Maybe String
  targetDataLayout      :: t {- dummy -} -> Maybe DataLayout

  -- | Given an LLVM module and the name of the functions that are defined in
  -- that module, compile the module into something executable on this target.
  --
  compileForTarget      :: Module -> [Name] -> LLVM (ExecutableR t)


-- | A dummy instance that just carries the LLVM Module. This is can be useful
-- for debugging, etc.
--
data LL
instance Target LL where
  data ExecutableR LL = LL Module
  targetTriple _     = Nothing
  targetDataLayout _ = Nothing
  compileForTarget m _ = return (LL m)

