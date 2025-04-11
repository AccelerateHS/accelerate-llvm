{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
module LLVM.AST.Type.GetElementPtr where

import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Representation


-- | A @getelementptr@ instruction. The @op@ parameter is the type of operands
-- (in practice, 'LLVM.AST.Type.Operand.Operand' or
-- 'LLVM.AST.Type.Constant.Constant'). The @ptra@ and @ptrb@ type indices are
-- the input and output /pointer/ type.
--
-- The type of indices is unconstrained, which is more flexible than reality,
-- and the kind of operands must be uniform, which is /less/ flexible than
-- reality. Reality is quite cumbersome:
-- * When indexing into a structure (currently unsupported by this data type),
--   the index type must be @i32@ and its value must be constant.
-- * When indexing into an array, the index type may be any /integral/ type,
--   signed or unsigned, which are then treated as signed integers.
--
-- <https://llvm.org/docs/LangRef.html#getelementptr-instruction>
data GetElementPtr op ptra ptrb where
  GEP :: Type a
      -> op (Ptr a)
      -> op i             -- ^ the offset to the initial pointer (counted in pointees, not in bytes)
      -> GEPIndex op a b  -- ^ field/index selection path
      -> GetElementPtr op (Ptr a) (Ptr b)

-- | A convenience pattern synonym for the common case of a full path of length 1.
pattern GEP1 :: ScalarType a -> op (Ptr a) -> op i -> GetElementPtr op (Ptr a) (Ptr a)
pattern GEP1 ty ptr ix <- GEP (PrimType (ScalarPrimType ty)) ptr ix (GEPEmpty (ScalarPrimType _))
  where GEP1 ty ptr ix = GEP (PrimType (ScalarPrimType ty)) ptr ix (GEPEmpty (ScalarPrimType ty))

-- | An index sequence that goes from a 'Ptr a' to a 'Ptr b'. Note that this
-- data type is indexed with the base types of the pointers, not the pointer
-- types themselves.
data GEPIndex op a b where
  GEPEmpty :: PrimType b -> GEPIndex op b b
  GEPArray :: op i       -> GEPIndex op a b -> GEPIndex op (LLArray a) b
  -- TODO: structure indexing

instance (forall i. Downcast (op i) v) => Downcast (GEPIndex op a b) [v] where
  downcast (GEPEmpty _) = []
  downcast (GEPArray i l) = downcast i : downcast l

gepIndexOutType :: GEPIndex op a b -> PrimType b
gepIndexOutType (GEPEmpty t) = t
gepIndexOutType (GEPArray _ l) = gepIndexOutType l

instance TypeOf op => TypeOf (GetElementPtr op ptra) where
  typeOf (GEP _ p _ path) =
    case typeOf p of
      PrimType (PtrPrimType _ addr) -> PrimType (PtrPrimType (gepIndexOutType path) addr)
      _ -> error "Pointer type is not a pointer type"
