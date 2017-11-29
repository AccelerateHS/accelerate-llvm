{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Type
-- Copyright   : [2015..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Type
  where

import Data.Array.Accelerate.Array.Sugar

import LLVM.AST.Type.Constant
import LLVM.AST.Type.Global
import LLVM.AST.Type.Instruction
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation


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
      Add _ x _             -> typeOf x
      Sub _ x _             -> typeOf x
      Mul _ x _             -> typeOf x
      Quot _ x _            -> typeOf x
      Rem _ x _             -> typeOf x
      Div _ x _             -> typeOf x
      ShiftL _ x _          -> typeOf x
      ShiftRL _ x _         -> typeOf x
      ShiftRA _ x _         -> typeOf x
      BAnd _ x _            -> typeOf x
      BOr _ x _             -> typeOf x
      BXor _ x _            -> typeOf x
      LAnd _ _              -> type'
      LOr _ _               -> type'
      LNot _                -> type'
      ExtractValue t _ _    -> PrimType (ScalarPrimType t)
      Load t _ _            -> PrimType (ScalarPrimType t)
      Store _ _ _           -> VoidType
      GetElementPtr x _     -> typeOf x
      Fence _               -> VoidType
      CmpXchg t _ _ _ _ _ _ -> PrimType $ TupleType
                             $ UnitTuple `PairTuple` SingleTuple (NumScalarType (IntegralNumType t))
                                         `PairTuple` SingleTuple scalarType
      AtomicRMW _ _ _ _ x _ -> typeOf x
      FTrunc _ t _          -> PrimType (ScalarPrimType (NumScalarType (FloatingNumType t)))
      FExt _ t _            -> PrimType (ScalarPrimType (NumScalarType (FloatingNumType t)))
      Trunc _ t _           -> case t of
                                 IntegralBoundedType i -> PrimType (ScalarPrimType (NumScalarType (IntegralNumType i)))
                                 NonNumBoundedType n   -> PrimType (ScalarPrimType (NonNumScalarType n))
      Ext _ t _             -> case t of
                                 IntegralBoundedType i -> PrimType (ScalarPrimType (NumScalarType (IntegralNumType i)))
                                 NonNumBoundedType n   -> PrimType (ScalarPrimType (NonNumScalarType n))
      FPToInt _ t _         -> PrimType (ScalarPrimType (NumScalarType (IntegralNumType t)))
      IntToFP _ t _         -> PrimType (ScalarPrimType (NumScalarType (FloatingNumType t)))
      BitCast t _           -> PrimType (ScalarPrimType t)
      PtrCast t _           -> PrimType t
      Cmp{}                 -> type'
      Select t _ _ _        -> PrimType (ScalarPrimType t)
      Phi t _               -> PrimType t
      Call f _              -> funResultType f
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
      UndefConstant t           -> t
      GlobalReference t _       -> t


-- | Extract some evidence that a reified type implies that type is a valid
-- element
--
data EltDict a where
  EltDict :: Elt a => EltDict a

scalarElt :: ScalarType a -> EltDict a
scalarElt (NumScalarType    t) = numElt t
scalarElt (NonNumScalarType t) = nonNumElt t

numElt :: NumType a -> EltDict a
numElt (IntegralNumType t) = integralElt t
numElt (FloatingNumType t) = floatingElt t

integralElt :: IntegralType a -> EltDict a
integralElt TypeInt{}     = EltDict
integralElt TypeInt8{}    = EltDict
integralElt TypeInt16{}   = EltDict
integralElt TypeInt32{}   = EltDict
integralElt TypeInt64{}   = EltDict
integralElt TypeWord{}    = EltDict
integralElt TypeWord8{}   = EltDict
integralElt TypeWord16{}  = EltDict
integralElt TypeWord32{}  = EltDict
integralElt TypeWord64{}  = EltDict
integralElt TypeCShort{}  = EltDict
integralElt TypeCUShort{} = EltDict
integralElt TypeCInt{}    = EltDict
integralElt TypeCUInt{}   = EltDict
integralElt TypeCLong{}   = EltDict
integralElt TypeCULong{}  = EltDict
integralElt TypeCLLong{}  = EltDict
integralElt TypeCULLong{} = EltDict

floatingElt :: FloatingType a -> EltDict a
floatingElt TypeFloat{}   = EltDict
floatingElt TypeDouble{}  = EltDict
floatingElt TypeCFloat{}  = EltDict
floatingElt TypeCDouble{} = EltDict

nonNumElt :: NonNumType a -> EltDict a
nonNumElt TypeBool{}   = EltDict
nonNumElt TypeChar{}   = EltDict
nonNumElt TypeCChar{}  = EltDict
nonNumElt TypeCSChar{} = EltDict
nonNumElt TypeCUChar{} = EltDict

