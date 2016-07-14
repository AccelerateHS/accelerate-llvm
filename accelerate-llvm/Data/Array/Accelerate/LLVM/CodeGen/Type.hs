{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Type
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Type
  where

import LLVM.General.AST.Type.Constant
import LLVM.General.AST.Type.Global
import LLVM.General.AST.Type.Instruction
import LLVM.General.AST.Type.Operand
import LLVM.General.AST.Type.Representation


-- | Does the concrete type represent signed or unsigned values?
--
class IsSigned dict where
  signed   :: dict a -> Bool
  signed   = not . unsigned
  --
  unsigned :: dict a -> Bool
  unsigned = not . signed

instance IsSigned ScalarType where
  signed (NumScalarType t)    = signed t
  signed (NonNumScalarType t) = signed t

instance IsSigned BoundedType where
  signed (IntegralBoundedType t) = signed t
  signed (NonNumBoundedType t)   = signed t

instance IsSigned NumType where
  signed (IntegralNumType t) = signed t
  signed (FloatingNumType t) = signed t

instance IsSigned IntegralType where
  signed t =
    case t of
      TypeInt _    -> True
      TypeInt8 _   -> True
      TypeInt16 _  -> True
      TypeInt32 _  -> True
      TypeInt64 _  -> True
      TypeCShort _ -> True
      TypeCInt _   -> True
      TypeCLong _  -> True
      TypeCLLong _ -> True
      _            -> False

instance IsSigned FloatingType where
  signed _ = True

instance IsSigned NonNumType where
  signed t =
    case t of
      TypeBool _        -> False
      TypeChar _        -> False
      TypeCUChar _      -> False
      TypeCSChar _      -> True
      TypeCChar _       -> True


-- | Extract the reified scalar type dictionary of an operation
--
class TypeOf op where
  typeOf :: op a -> Type a

instance TypeOf Instruction where
  typeOf ins =
    case ins of
      Add _ x _         -> typeOf x
      Sub _ x _         -> typeOf x
      Mul _ x _         -> typeOf x
      Quot _ x _        -> typeOf x
      Rem _ x _         -> typeOf x
      Div _ x _         -> typeOf x
      ShiftL _ x _      -> typeOf x
      ShiftRL _ x _     -> typeOf x
      ShiftRA _ x _     -> typeOf x
      BAnd _ x _        -> typeOf x
      BOr _ x _         -> typeOf x
      BXor _ x _        -> typeOf x
      LAnd _ _          -> type'
      LOr _ _           -> type'
      LNot _            -> type'
      Load t _ _        -> PrimType (ScalarPrimType t)
      Store _ _ _       -> VoidType
      GetElementPtr x _ -> typeOf x
      FTrunc _ t _      -> PrimType (ScalarPrimType (NumScalarType (FloatingNumType t)))
      FExt _ t _        -> PrimType (ScalarPrimType (NumScalarType (FloatingNumType t)))
      Trunc _ t _       -> case t of
                             IntegralBoundedType i -> PrimType (ScalarPrimType (NumScalarType (IntegralNumType i)))
                             NonNumBoundedType n   -> PrimType (ScalarPrimType (NonNumScalarType n))
      Ext _ t _         -> case t of
                             IntegralBoundedType i -> PrimType (ScalarPrimType (NumScalarType (IntegralNumType i)))
                             NonNumBoundedType n   -> PrimType (ScalarPrimType (NonNumScalarType n))
      FPToInt _ t _     -> PrimType (ScalarPrimType (NumScalarType (IntegralNumType t)))
      IntToFP _ t _     -> PrimType (ScalarPrimType (NumScalarType (FloatingNumType t)))
      BitCast t _       -> PrimType (ScalarPrimType t)
      PtrCast t _       -> PrimType t
      Cmp{}             -> type'
      Select t _ _ _    -> PrimType (ScalarPrimType t)
      Phi t _           -> PrimType t
      Call f _          -> funResultType f
        where
          funResultType :: GlobalFunction args t -> Type t
          funResultType (Lam _ _ l) = funResultType l
          funResultType (Body t _)  = t

instance TypeOf Operand where
  typeOf op =
    case op of
      LocalReference t _ -> t
      ConstantOperand c  -> typeOf c

instance TypeOf Constant where
  typeOf c =
    case c of
      ScalarConstant t _        -> PrimType (ScalarPrimType t)
      GlobalReference t _       -> t

