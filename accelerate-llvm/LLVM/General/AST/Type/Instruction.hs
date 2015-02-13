{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}
-- |
-- Module      : LLVM.General.AST.Type.Instruction
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.General.AST.Type.Instruction
  where

import LLVM.General.AST.Type.Bits
import LLVM.General.AST.Type.Name
import LLVM.General.AST.Type.Operand
import LLVM.General.AST.Type.Global

import Data.Array.Accelerate.Type

import Foreign.Ptr


-- | Predicate for comparison instruction
--
data Predicate = EQ | NE | LT | LE | GT | GE

data Volatile = Volatile | NonVolatile

-- | Non-terminating instructions
--
--  * <http://llvm.org/docs/LangRef.html#binaryops>
--
--  * <http://llvm.org/docs/LangRef.html#bitwiseops>
--
--  * <http://llvm.org/docs/LangRef.html#memoryops>
--
--  * <http://llvm.org/docs/LangRef.html#otherops>
--
--
data Instruction a where
  -- Binary Operations
  -- <http://llvm.org/docs/LangRef.html#binary-operations>

  -- | <http://llvm.org/docs/LangRef.html#add-instruction>
  --   <http://llvm.org/docs/LangRef.html#fadd-instruction>
  --
  Add           :: NumType a
                -> Operand a
                -> Operand a
                -> Instruction a

  -- | <http://llvm.org/docs/LangRef.html#sub-instruction>
  --   <http://llvm.org/docs/LangRef.html#fsub-instruction>
  --
  Sub           :: NumType a
                -> Operand a
                -> Operand a
                -> Instruction a

  -- | <http://llvm.org/docs/LangRef.html#mul-instruction>
  --   <http://llvm.org/docs/LangRef.html#fmul-instruction>
  --
  Mul           :: NumType a
                -> Operand a
                -> Operand a
                -> Instruction a

  -- | <http://llvm.org/docs/LangRef.html#udiv-instruction>
  --   <http://llvm.org/docs/LangRef.html#sdiv-instruction>
  --
  Quot          :: IntegralType a
                -> Operand a
                -> Operand a
                -> Instruction a

  -- | <http://llvm.org/docs/LangRef.html#urem-instruction>
  --   <http://llvm.org/docs/LangRef.html#srem-instruction>
  --
  Rem           :: IntegralType a
                -> Operand a
                -> Operand a
                -> Instruction a

  -- | <http://llvm.org/docs/LangRef.html#fdiv-instruction>
  --
  Div           :: FloatingType a
                -> Operand a
                -> Operand a
                -> Instruction a

  -- | <http://llvm.org/docs/LangRef.html#shl-instruction>
  --
  ShiftL        :: IntegralType a
                -> Operand a
                -> Operand a
                -> Instruction a

  -- | <http://llvm.org/docs/LangRef.html#lshr-instruction>
  --
  ShiftRL       :: IntegralType a
                -> Operand a
                -> Operand a
                -> Instruction a

  -- | <http://llvm.org/docs/LangRef.html#ashr-instruction>
  ShiftRA       :: IntegralType a
                -> Operand a
                -> Operand a
                -> Instruction a

  -- Bitwise Binary Operations
  -- <http://llvm.org/docs/LangRef.html#bitwise-binary-operations>

  -- | <http://llvm.org/docs/LangRef.html#and-instruction>
  --
  BAnd          :: IntegralType a
                -> Operand a
                -> Operand a
                -> Instruction a

  LAnd          :: Operand Bool
                -> Operand Bool
                -> Instruction Bool

  -- | <http://llvm.org/docs/LangRef.html#or-instruction>
  --
  BOr           :: IntegralType a
                -> Operand a
                -> Operand a
                -> Instruction a

  LOr           :: Operand Bool
                -> Operand Bool
                -> Instruction Bool

  -- | <http://llvm.org/docs/LangRef.html#xor-instruction>
  --
  BXor          :: IntegralType a
                -> Operand a
                -> Operand a
                -> Instruction a

  LNot          :: Operand Bool
                -> Instruction Bool

  -- Vector Operations
  -- <http://llvm.org/docs/LangRef.html#vector-operations>
  -- ExtractElement
  -- InsertElement
  -- ShuffleVector

  -- Aggregate Operations
  -- <http://llvm.org/docs/LangRef.html#aggregate-operations>
  -- ExtractValue
  -- InsertValue

  -- Memory Access and Addressing Operations
  -- <http://llvm.org/docs/LangRef.html#memory-access-and-addressing-operations>
  -- Alloca

  -- | <http://llvm.org/docs/LangRef.html#load-instruction>
  --
  Load          :: ScalarType a
                -> Volatile
                -> Operand (Ptr a)
                -> Instruction a

  -- | <http://llvm.org/docs/LangRef.html#store-instruction>
  --
  Store         :: Volatile
                -> Operand (Ptr a)
                -> Operand a
                -> Instruction ()

  -- | <http://llvm.org/docs/LangRef.html#getelementptr-instruction>
  --
  GetElementPtr :: Operand a            -- Operand (Ptr a), Name a ??
                -> [Operand Int]
                -> Instruction (Ptr a)

  -- Fence
  -- CmpXchg
  -- AtomicRMW

  -- | <http://llvm.org/docs/LangRef.html#trunc-to-instruction>
  --
  Trunc         :: BoundedType a        -- req: (BitSize a > BitSize b)      -- TLM: expelling this constraint may be tricky
                -> BoundedType b
                -> Operand a
                -> Instruction b

  -- | <http://llvm.org/docs/LangRef.html#fptrunc-to-instruction>
  --
  FTrunc        :: (BitSize a > BitSize b)
                => FloatingType a
                -> FloatingType b
                -> Operand a
                -> Instruction b

  -- | <http://llvm.org/docs/LangRef.html#zext-to-instruction>
  --   <http://llvm.org/docs/LangRef.html#sext-to-instruction>
  --
  Ext           :: BoundedType a        -- Req: (BitSize a < BitSize b)
                -> BoundedType b
                -> Operand a
                -> Instruction b

  -- | <http://llvm.org/docs/LangRef.html#fpext-to-instruction>
  --
  FExt          :: (BitSize a < BitSize b)
                => FloatingType a
                -> FloatingType b
                -> Operand a
                -> Instruction b

  -- | <http://llvm.org/docs/LangRef.html#fptoui-to-instruction>
  --   <http://llvm.org/docs/LangRef.html#fptosi-to-instruction>
  --
  FPToInt       :: FloatingType a
                -> IntegralType b
                -> Operand a
                -> Instruction b

  -- | <http://llvm.org/docs/LangRef.html#uitofp-to-instruction>
  --   <http://llvm.org/docs/LangRef.html#sitofp-to-instruction>
  --
  IntToFP       :: IntegralType a
                -> FloatingType b
                -> Operand a
                -> Instruction b

  -- | <http://llvm.org/docs/LangRef.html#bitcast-to-instruction>
  --
  BitCast       :: ScalarType b         -- precondition: (BitSizeEq a b ~ True)
                -> Operand a
                -> Instruction b

  -- PtrToInt
  -- IntToPtr
  -- AddrSpaceCast

  -- Other Operations
  -- <http://llvm.org/docs/LangRef.html#other-operations>

  -- | <http://llvm.org/docs/LangRef.html#icmp-instruction>
  --   <http://llvm.org/docs/LangRef.html#fcmp-instruction>
  --
  Cmp           :: ScalarType a
                -> Predicate
                -> Operand a
                -> Operand a
                -> Instruction Bool

  -- | <http://llvm.org/docs/LangRef.html#phi-instruction>
  --
  Phi           :: ScalarType a
                -> [(Operand a, Label)]
                -> Instruction a

  -- | <http://llvm.org/docs/LangRef.html#call-instruction>
  --
  Call          :: GlobalFunction args t
                -> [FunctionAttribute]
                -> Instruction t

  -- | <http://llvm.org/docs/LangRef.html#select-instruction>
  Select        :: ScalarType a
                -> Operand Bool
                -> Operand a
                -> Operand a
                -> Instruction a

  -- VAArg
  -- LandingPad


-- | Instances of instructions may be given a name, allowing their results to be
-- referenced as Operands. Instructions returning void (e.g. function calls)
-- don't need names.
--
data Named ins a where
  (:=) :: Name a -> ins a -> Named ins a
  Do   :: ins ()          -> Named ins ()

