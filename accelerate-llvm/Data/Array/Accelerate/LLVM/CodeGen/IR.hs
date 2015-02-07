{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.IR
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.IR (

  IRExp, IRFun1, IRFun2,
  IROpenExp, IROpenFun1(..), IROpenFun2(..),
  IROpenAcc(..), IRDelayed(..), IRManifest(..),

  IR(..), Operands(..),
  IROP(..),

) where

import LLVM.General.AST.Type.Operand

import Data.Array.Accelerate.Type
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Array.Sugar

import Data.Array.Accelerate.LLVM.CodeGen.Monad


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

-- type IRFun     arch     aenv t = IROpenFun arch () aenv t

-- type IRFun1 arch aenv t  = forall a b. IRFun arch aenv (a -> b)
-- type IRFun2 arch aenv t  = forall a b c. IRFun arch aenv (a -> b -> c)

-- type family IROpenFun arch env aenv f where
--   IROpenFun arch env aenv (a -> b -> c) = IR a -> IR b -> IROpenExp arch env aenv c
--   IROpenFun arch env aenv (a -> b)      = IR a -> IROpenExp arch env aenv b


-- type IROpenFun arch env aenv t = IRFunR env t

-- type family IRFunR env t where
--   IRFunR env (a -> b) = IR a -> IRFunR (env,a) b
--   IRFunR env t        = CodeGen (IR t)

-- type family IROpenFun arch env aenv t where
--   IROpenFun arch env aenv (a -> b) = IR a -> IROpenFun arch (env,a) aenv b
--   IROpenFun arch env aenv t        =         IROpenExp arch env     aenv t


--type IROpenFun arch env t = forall a. IR a -> IROpenExp arch

-- data IROpenFun arch env aenv t where
--   IRBody ::          IROpenExp arch env aenv t       -> IROpenFun arch env aenv t
--   IRLam  :: (IR a -> IROpenFun arch (env, a) aenv t) -> IROpenFun arch env aenv (a -> t)

-- TLM: This is annoying to specialise for function arity, it would be better to
--      make this general like AST.PreOpenFun
--
-- type IRFun1 arch aenv f         = forall a b.   IR a         -> IROpenExp arch ((),a)     aenv b
-- type IRFun2 arch aenv f         = forall a b c. IR a -> IR b -> IROpenExp arch (((),a),b) aenv c

-- data IROpenExp arch env aenv t where
--   IRExp  :: CodeGen (IR t) -> IROpenExp arch env aenv t

--data IROpenFun arch env aenv t where
--  IRBody :: IROpenExp arch env      aenv t -> IROpenFun arch env aenv t
--  IRLam  :: IROpenFun arch (env, a) aenv r -> IROpenFun arch env aenv (a -> r)


data IROpenAcc arch aenv a where
  IROpenAcc :: () {- ??? -}
            -> IROpenAcc arch aenv a

data IRDelayed arch aenv a where
  IRDelayed :: (Shape sh, Elt e) =>
    { delayedExtent      :: IRExp  arch aenv sh
    , delayedIndex       :: IRFun1 arch aenv (sh -> e)
    , delayedLinearIndex :: IRFun1 arch aenv (Int -> e)
    }
    -> IRDelayed arch aenv (Array sh e)

data IRManifest arch aenv a where
  IRManifest :: Arrays arrs => Idx aenv arrs -> IRManifest arch aenv arrs


-- | The datatype 'IR' represents the LLVM IR producing a value of type 'a'.
-- Note that the operands comprising this value are stored in representation
-- type.
--
data IR t where
  IR :: Operands (EltRepr t)
     -> IR t

-- We use a data family to represent sequences of LLVM (scalar) operands
-- representing a single Accelerate type. Using a data family rather than a type
-- family means that Operands is bijective.
--
data family Operands e :: *
data instance Operands ()       = OP_Unit
data instance Operands a        = OP_Scalar (Operand a)
data instance Operands (a,b)    = OP_Pair (Operands a) (Operands b)


-- | Given some evidence that 'IR a' represents a scalar type, it can be
-- converted between the IR and Operand data types.
--
class IROP dict where
  op :: dict a -> IR a -> Operand a
  ir :: dict a -> Operand a -> IR a

instance IROP ScalarType where
  op (NumScalarType t)    = op t
  op (NonNumScalarType t) = op t
  --
  ir (NumScalarType t)    = ir t
  ir (NonNumScalarType t) = ir t

instance IROP NumType where
  op (IntegralNumType t) = op t
  op (FloatingNumType t) = op t
  --
  ir (IntegralNumType t) = ir t
  ir (FloatingNumType t) = ir t

instance IROP IntegralType where
  op (TypeInt     _) = unpack
  op (TypeInt8    _) = unpack
  op (TypeInt16   _) = unpack
  op (TypeInt32   _) = unpack
  op (TypeInt64   _) = unpack
  op (TypeWord    _) = unpack
  op (TypeWord8   _) = unpack
  op (TypeWord16  _) = unpack
  op (TypeWord32  _) = unpack
  op (TypeWord64  _) = unpack
  op (TypeCShort  _) = unpack
  op (TypeCUShort _) = unpack
  op (TypeCInt    _) = unpack
  op (TypeCUInt   _) = unpack
  op (TypeCLong   _) = unpack
  op (TypeCULong  _) = unpack
  op (TypeCLLong  _) = unpack
  op (TypeCULLong _) = unpack
  --
  ir (TypeInt     _) = pack
  ir (TypeInt8    _) = pack
  ir (TypeInt16   _) = pack
  ir (TypeInt32   _) = pack
  ir (TypeInt64   _) = pack
  ir (TypeWord    _) = pack
  ir (TypeWord8   _) = pack
  ir (TypeWord16  _) = pack
  ir (TypeWord32  _) = pack
  ir (TypeWord64  _) = pack
  ir (TypeCShort  _) = pack
  ir (TypeCUShort _) = pack
  ir (TypeCInt    _) = pack
  ir (TypeCUInt   _) = pack
  ir (TypeCLong   _) = pack
  ir (TypeCULong  _) = pack
  ir (TypeCLLong  _) = pack
  ir (TypeCULLong _) = pack

instance IROP FloatingType where
  op (TypeFloat   _) = unpack
  op (TypeDouble  _) = unpack
  op (TypeCFloat  _) = unpack
  op (TypeCDouble _) = unpack
  --
  ir (TypeFloat   _) = pack
  ir (TypeDouble  _) = pack
  ir (TypeCFloat  _) = pack
  ir (TypeCDouble _) = pack

instance IROP NonNumType where
  op (TypeBool   _) = unpack
  op (TypeChar   _) = unpack
  op (TypeCChar  _) = unpack
  op (TypeCSChar _) = unpack
  op (TypeCUChar _) = unpack
  --
  ir (TypeBool   _) = pack
  ir (TypeChar   _) = pack
  ir (TypeCChar  _) = pack
  ir (TypeCSChar _) = pack
  ir (TypeCUChar _) = pack

unpack :: (EltRepr a ~ a) => IR a -> Operand a
unpack (IR (OP_Scalar x)) = x

pack :: (EltRepr a ~ a) => Operand a -> IR a
pack x = IR (OP_Scalar x)

