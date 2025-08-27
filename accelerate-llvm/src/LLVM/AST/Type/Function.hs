{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : LLVM.AST.Type.Function
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.AST.Type.Function
  where

import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation

-- import qualified LLVM.AST.Attribute                                 as LLVM
-- import qualified LLVM.AST.Global                                    as LLVM
-- import qualified LLVM.AST.Instruction                               as LLVM
import qualified Data.Array.Accelerate.LLVM.Internal.LLVMPretty     as LLVM


-- | Attributes for the function call instruction
--
data FunctionAttribute
  = NoReturn
  | NoUnwind
  | ReadOnly
  | ReadNone
  | AlwaysInline
  | NoDuplicate
  | Convergent
  | InaccessibleMemOnly

-- | Tail call kind for function call instruction
--
data TailCall
  = Tail
  | NoTail
  | MustTail

-- | Parameters for functions
--
data Parameter a where
  Parameter :: PrimType a -> Name a -> Parameter a

-- | Attribute groups are groups of attributes that are referenced by
-- objects within the IR. To use an attribute group, an object must
-- reference its GroupID.
--
data GroupID = GroupID !Word


-- | Functions are arguments to the 'call' instruction; either global
-- functions or inline assembly.
--
data Function kind args t where
  Body :: Type r -> Maybe TailCall -> kind                -> Function kind '[]         r
  Lam  :: PrimType a -> Operand a -> Function kind args t -> Function kind (a ': args) t


instance Downcast FunctionAttribute LLVM.FunAttr where
  downcast NoReturn            = LLVM.Noreturn
  downcast NoUnwind            = LLVM.Nounwind
  downcast ReadOnly            = LLVM.Readonly
  downcast ReadNone            = LLVM.Readnone
  downcast AlwaysInline        = LLVM.Alwaysinline
  downcast NoDuplicate         = LLVM.Noduplicate
  downcast Convergent          = LLVM.Convergent
  downcast InaccessibleMemOnly = LLVM.InaccessibleMemOnly

instance Downcast (Parameter a) (LLVM.Typed LLVM.Ident) where
  -- TODO attributes! llvm-pretty doesn't seem to support them, but we put
  -- [NoAlias, NoCapture] on pointer types.
  -- TODO: Should check if these parameters are necessary (by benchmarking the old backend with llvm-hs), and if so, should send a PR to llvm-pretty
  downcast (Parameter t n) = LLVM.Typed (downcast t) (nameToPrettyI n)

instance Downcast TailCall Bool where
  downcast Tail     = True
  downcast NoTail   = False
  downcast MustTail = error "TODO MustTail"

-- instance Downcast GroupID LLVM.GroupID where
--   downcast (GroupID n) = LLVM.GroupID n

