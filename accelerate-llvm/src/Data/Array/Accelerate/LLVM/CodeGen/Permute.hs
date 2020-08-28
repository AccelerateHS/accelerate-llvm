{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Permute
-- Copyright   : [2016..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Permute (

  IRPermuteFun(..),
  llvmOfPermuteFun,

  atomicCAS_rmw,
  atomicCAS_cmp,

) where

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.AST.LeftHandSide
import Data.Array.Accelerate.AST.Var
import Data.Array.Accelerate.Debug
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Representation.Type
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.CodeGen.Type
import Data.Array.Accelerate.LLVM.Foreign

import LLVM.AST.Type.AddrSpace
import LLVM.AST.Type.Instruction
import LLVM.AST.Type.Instruction.Atomic
import LLVM.AST.Type.Instruction.RMW                                as RMW
import LLVM.AST.Type.Instruction.Volatile
import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation

import Control.Applicative
import Data.Constraint                                              ( withDict )
import System.IO.Unsafe
import Prelude


-- | A forward permutation might be specialised to use atomic instructions to
-- perform the read-modify-write of the output array directly, rather than
-- separately acquiring a lock. The basic operation is always provided in case
-- a backend does not support the atomic operation at that type, or if it is
-- executing sequentially.
--
-- For the atomicRMW case, the function is applied to the new value before
-- feeding to the atomic instruction to combine with the old.
--
data IRPermuteFun arch aenv t where
  IRPermuteFun :: { combine   :: IRFun2 arch aenv (e -> e -> e)
                  , atomicRMW :: Maybe
                      ( RMWOperation
                      , IRFun1 arch aenv (e -> e)
                      )
                  }
               -> IRPermuteFun arch aenv (e -> e -> e)


-- | Analysis and code generation for forward permutation combination function.
--
-- Specialisation for atomic operations is currently limited to direct
-- applications of the function; that is, we don't dig down underneath
-- let-bindings.
--
llvmOfPermuteFun
    :: forall arch aenv e. Foreign arch
    => Fun aenv (e -> e -> e)
    -> Gamma aenv
    -> IRPermuteFun arch aenv (e -> e -> e)
llvmOfPermuteFun fun aenv = IRPermuteFun{..}
  where
    combine   = llvmOfFun2 fun aenv
    atomicRMW
      -- If the old value is not used (i.e. permute const) then we can just
      -- store the new value directly. Since we do not require the return value
      -- we can do this for any scalar value with a regular Store. However,
      -- as we use an unzipped struct-of-array representation for product
      -- types, the multiple store instructions for the different fields
      -- could come from different threads, so we only allow the non-atomic
      -- version if the flag @-ffast-permute-const@ is set.
      --
      | Lam lhs (Lam (LeftHandSideWildcard tp) (Body body)) <- fun
      , True                  <- fast tp
      , fun'                  <- llvmOfFun1 (Lam lhs (Body body)) aenv
      = Just (Exchange, fun')

      -- LLVM natively supports atomic operations on integral types only.
      -- However different targets may support atomic instructions on other
      -- scalar types (for example the NVPTX target supports atomic add and
      -- subtract on floating point values).
      --
      -- Additionally it is possible to implement atomic instructions using
      -- atomic compare-and-swap, which is likely to be more performant than the
      -- generic spin-lock based approach.
      --
      | Lam lhs@(LeftHandSideSingle _) (Lam (LeftHandSideSingle _) (Body body)) <- fun
      , Just (rmw, x)         <- rmwOp body
      , Just x'               <- strengthenE latest x
      , fun'                  <- llvmOfFun1 (Lam lhs (Body x')) aenv
      = Just (rmw, fun')

      | otherwise
      = Nothing

    fast :: TypeR e -> Bool
    fast tp
      | TupRsingle{} <- tp = True
      | otherwise          = unsafePerformIO (getFlag fast_permute_const)

    -- XXX: This doesn't work for newtypes because the coercion gets in the
    -- way. This should be generalised to work for product types (e.g.
    -- complex numbers) and take this factor into account as well.
    --    TLM-2019-09-27
    --
    rmwOp :: OpenExp (((),e),e) aenv e -> Maybe (RMWOperation, OpenExp (((),e),e) aenv e)
    rmwOp (PrimApp f xs)
      | PrimAdd{}  <- f = (RMW.Add,) <$> extract xs
      | PrimSub{}  <- f = (RMW.Sub,) <$> extract xs
      | PrimMin{}  <- f = (RMW.Min,) <$> extract xs
      | PrimMax{}  <- f = (RMW.Max,) <$> extract xs
      | PrimBOr{}  <- f = (RMW.Or,)  <$> extract xs
      | PrimBAnd{} <- f = (RMW.And,) <$> extract xs
      | PrimBXor{} <- f = (RMW.Xor,) <$> extract xs
    rmwOp _             = Nothing

    -- Determine which argument to a binary function was the new value being
    -- combined. This only works when the old value is used unmodified, but that
    -- is sufficient for us because otherwise it would not be suitable for the
    -- atomic update operation.
    --
    -- In the permutation function, the old value is given as the second
    -- argument, corresponding to ZeroIdx.
    --
    extract :: OpenExp (((),e),e) aenv (e,e) -> Maybe (OpenExp (((),e),e) aenv e)
    extract (Pair x y)
      | Evar (Var _ ZeroIdx) <- x = Just y
      | Evar (Var _ ZeroIdx) <- y = Just x
    extract _
      = Nothing

    -- Used with 'strengthenE' to ensure that the expression does not make use
    -- of the old value except in the combination function.
    latest :: (((),e),e) :?> ((),e)
    latest ZeroIdx      = Nothing
    latest (SuccIdx ix) = Just ix


-- Implementation of atomic RMW operation (e.g. (+), (-)) using atomic
-- compare-and-swap instructions, for targets which do not support the native
-- instruction at this type but do support CAS at this bit width.
--
-- > void casAdd(double *addr, double val)
-- > {
-- >     uint64_t* addr_i = reinterpret_cast<uint64_t*> addr;
-- >     uint64_t old     = *addr_i;
-- >
-- >     do {
-- >       uint64_t expected = old;
-- >       uint64_t new      = reinterpret_cast<uint64_t>(val + reinterpret_cast<double>(expected));
-- >
-- >       uint64_t old      = atomicCAS(addr_i, expected, new);
-- >     }
-- >     while (old != expected);
-- > }
--
atomicCAS_rmw
    :: forall arch e. HasCallStack
    => SingleType e
    -> (Operands e -> CodeGen arch (Operands e))
    -> Operand (Ptr e)
    -> CodeGen arch ()
atomicCAS_rmw t update addr =
  case t of
    NumSingleType (FloatingNumType f) -> floating f
    NumSingleType (IntegralNumType i) -> integral i

  where
    floating :: FloatingType t -> CodeGen arch ()
    floating TypeHalf{}   = atomicCAS_rmw' t (integralType :: IntegralType Word16) update addr
    floating TypeFloat{}  = atomicCAS_rmw' t (integralType :: IntegralType Word32) update addr
    floating TypeDouble{} = atomicCAS_rmw' t (integralType :: IntegralType Word64) update addr

    integral :: IntegralType t -> CodeGen arch ()
    integral i            = atomicCAS_rmw' t i update addr


atomicCAS_rmw'
    :: HasCallStack
    => SingleType t
    -> IntegralType i
    -> (Operands t -> CodeGen arch (Operands t))
    -> Operand (Ptr t)
    -> CodeGen arch ()
atomicCAS_rmw' t i update addr = withDict (integralElt i) $ do
  let si = SingleScalarType (NumSingleType (IntegralNumType i))
  --
  spin  <- newBlock "rmw.spin"
  exit  <- newBlock "rmw.exit"

  addr' <- instr' $ PtrCast (PtrPrimType (ScalarPrimType si) defaultAddrSpace) addr
  init' <- instr' $ Load si NonVolatile addr'
  old'  <- fresh  $ TupRsingle si
  top   <- br spin

  setBlock spin
  old   <- instr' $ BitCast (SingleScalarType t) (op i old')
  val   <- update $ ir t old
  val'  <- instr' $ BitCast si (op t val)
  r     <- instr' $ CmpXchg i NonVolatile addr' (op i old') val' (CrossThread, AcquireRelease) Monotonic
  done  <- instr' $ ExtractValue scalarType PairIdxRight r
  next' <- instr' $ ExtractValue si         PairIdxLeft  r

  -- Since we removed Bool from the set of primitive types Accelerate
  -- supports, we have to do a small hack to have LLVM consider this as its
  -- correct type of a 1-bit integer (rather than the 8-bits it is actually
  -- stored as)
  done' <- case done of
             LocalReference _ (UnName n) -> return $ OP_Bool (LocalReference type' (UnName n))
             _                           -> internalError "expected unnamed local reference"

  bot   <- cbr done' exit spin
  _     <- phi' (TupRsingle si) spin old' [(ir i init',top), (ir i next',bot)]

  setBlock exit


-- Implementation of atomic comparison operators (i.e. min, max) using
-- compare-and-swap, for targets which do not support the native instruction at
-- this type but do support CAS at this bit width. The old value is discarded.
--
-- For example, atomicMin is implemented similarly to the following (however the
-- loop condition is more complex):
--
-- > void casMin(double *addr, double val)
-- > {
-- >     double old      = *addr;
-- >     uint64_t val_i  = reinterpret_cast<uint64_t>(val);
-- >     uint64_t addr_i = reinterpret_cast<uint64_t*>(addr);
-- >
-- >     while (val < old) {
-- >         uint64_t assumed_i = reinterpret_cast<uint64_t>(old);
-- >         uint64_t old_i     = atomicCAS(addr_i, assumed_i, val_i);
-- >         old                = reinterpret_cast<double>(old_i);
-- >     }
-- > }
--
-- If the function returns 'True', then the given value should be written to the
-- address.
--
atomicCAS_cmp
    :: forall arch e. HasCallStack
    => SingleType e
    -> (SingleType e -> Operands e -> Operands e -> CodeGen arch (Operands Bool))
    -> Operand (Ptr e)
    -> Operand e
    -> CodeGen arch ()
atomicCAS_cmp t cmp addr val =
  case t of
    NumSingleType (FloatingNumType f) -> floating f
    NumSingleType (IntegralNumType i) -> integral i

  where
    floating :: FloatingType t -> CodeGen arch ()
    floating TypeHalf{}   = atomicCAS_cmp' t (integralType :: IntegralType Word16) cmp addr val
    floating TypeFloat{}  = atomicCAS_cmp' t (integralType :: IntegralType Word32) cmp addr val
    floating TypeDouble{} = atomicCAS_cmp' t (integralType :: IntegralType Word64) cmp addr val

    integral :: IntegralType t -> CodeGen arch ()
    integral i            = atomicCAS_cmp' t i cmp addr val


atomicCAS_cmp'
    :: HasCallStack
    => SingleType t       -- actual type of elements
    -> IntegralType i     -- unsigned integral type of same bit size as 't'
    -> (SingleType t -> Operands t -> Operands t -> CodeGen arch (Operands Bool))
    -> Operand (Ptr t)
    -> Operand t
    -> CodeGen arch ()
atomicCAS_cmp' t i cmp addr val = withDict (singleElt t) $ do
  let si = SingleScalarType (NumSingleType (IntegralNumType i))
  --
  test  <- newBlock "cas.cmp"
  spin  <- newBlock "cas.retry"
  exit  <- newBlock "cas.exit"

  -- The new value and address to swap cast to integral type
  addr' <- instr' $ PtrCast (PtrPrimType (ScalarPrimType si) defaultAddrSpace) addr
  val'  <- instr' $ BitCast si val
  old   <- fresh  $ TupRsingle $ SingleScalarType t

  -- Read the current value at the address
  start <- instr' $ Load (SingleScalarType t) NonVolatile addr
  top   <- br test

  -- Compare the new value with the current contents at that memory slot. If the
  -- comparison fails (e.g. we are computing atomicMin but the new value is
  -- already larger than the current value) then exit.
  setBlock test
  yes   <- cmp t (ir t val) old
  _     <- cbr yes spin exit

  -- Attempt to exchange the memory at this location with the new value. The
  -- CmpXchg instruction returns the old value together with a flag indicating
  -- whether or not the swap occurred. If the swap is successful we are done,
  -- otherwise reapply the comparison value with the newly acquired value.
  setBlock spin
  old'  <- instr' $ BitCast si (op t old)
  r     <- instr' $ CmpXchg i NonVolatile addr' old' val' (CrossThread, AcquireRelease) Monotonic
  done  <- instr' $ ExtractValue scalarType PairIdxRight r
  next  <- instr' $ ExtractValue si         PairIdxLeft  r
  next' <- instr' $ BitCast (SingleScalarType t) next

  done' <- case done of
             LocalReference _ (UnName n) -> return $ OP_Bool (LocalReference type' (UnName n))
             _                           -> internalError "expected unnamed local reference"

  bot   <- cbr done' exit test
  _     <- phi' (TupRsingle $ SingleScalarType t) test old [(ir t start,top), (ir t next',bot)]

  setBlock exit

