{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeApplications         #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Profile
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Profile (

  with_zone,
  with_zone_name,

  alloc_srcloc, alloc_srcloc_name,
  zone_begin, zone_end,

) where

import LLVM.AST.Type.AddrSpace
import LLVM.AST.Type.Constant
import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Function
import LLVM.AST.Type.Global
import LLVM.AST.Type.Instruction
import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation
import qualified LLVM.AST.Constant                                  as Constant
import qualified LLVM.AST.Global                                    as LLVM
import qualified LLVM.AST.Linkage                                   as LLVM
import qualified LLVM.AST.Type                                      as LLVM

import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad

import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Debug.Internal                         ( profilingIsEnabled, SrcLoc, Zone )

import Control.Monad
import Data.Char


with_zone :: Int -> String -> String -> (Operands Zone -> CodeGen arch a) -> CodeGen arch a
with_zone line src fun k = do
  l <- alloc_srcloc line src fun
  z <- zone_begin l
  r <- k z
  zone_end z
  return r

with_zone_name :: Int -> String -> String -> String -> (Operands Zone -> CodeGen arch a) -> CodeGen arch a
with_zone_name line src fun name k = do
  l <- alloc_srcloc_name line src fun name
  z <- zone_begin l
  r <- k z
  zone_end z
  return r


-- Internals
-- ---------

call' :: GlobalFunction args t -> CodeGen arch (Operands t)
call' f = call f [NoDuplicate]

global_string :: String -> CodeGen arch (Name (Ptr Word8), Word64)
global_string cs = do
  let l  = fromIntegral (length cs)
  --
  nm <- freshGlobalName
  _  <- declare $ LLVM.globalVariableDefaults
    { LLVM.name        = downcast nm
    , LLVM.isConstant  = True
    , LLVM.linkage     = LLVM.Private
    , LLVM.type'       = LLVM.ArrayType l LLVM.i8
    , LLVM.unnamedAddr = Just LLVM.GlobalAddr
    , LLVM.initializer = Just $ Constant.Array LLVM.i8 [ Constant.Int 8 (toInteger (ord c)) | c <- cs ]
    }
  return (nm, l)

alloc_srcloc
    :: Int      -- line
    -> String   -- source file
    -> String   -- function
    -> CodeGen arch (Operands SrcLoc)
alloc_srcloc l src fun
  | not profilingIsEnabled = return (constant (eltR @SrcLoc) 0)
  | otherwise              = do
      (s, sl) <- global_string src
      (f, fl) <- global_string fun
      let
          st         = PtrPrimType (ArrayPrimType sl scalarType) defaultAddrSpace
          ft         = PtrPrimType (ArrayPrimType fl scalarType) defaultAddrSpace
          line       = ConstantOperand $ ScalarConstant scalarType (fromIntegral l :: Word32)
          source     = ConstantOperand $ GlobalReference (PrimType st) s
          function   = ConstantOperand $ GlobalReference (PrimType ft) f
          sourceSz   = ConstantOperand $ ScalarConstant scalarType sl
          functionSz = ConstantOperand $ ScalarConstant scalarType fl
      --
      ps   <- src `null_or` instr' (GetElementPtr source   [num numType 0, num numType 0 :: Operand Int32])
      pf   <- fun `null_or` instr' (GetElementPtr function [num numType 0, num numType 0 :: Operand Int32])
      call' $ Lam primType line
            $ Lam primType ps
            $ Lam primType sourceSz
            $ Lam primType pf
            $ Lam primType functionSz
            $ Body (type' :: Type Word64) (Just Tail) "___tracy_alloc_srcloc"

alloc_srcloc_name
    :: Int      -- line
    -> String   -- source file
    -> String   -- function
    -> String   -- name
    -> CodeGen arch (Operands SrcLoc)
alloc_srcloc_name l src fun nm
  | not profilingIsEnabled = return (constant (eltR @SrcLoc) 0)
  | otherwise              = do
      (s, sl) <- global_string src
      (f, fl) <- global_string fun
      (n, nl) <- global_string nm
      let
          st         = PtrPrimType (ArrayPrimType sl scalarType) defaultAddrSpace
          ft         = PtrPrimType (ArrayPrimType fl scalarType) defaultAddrSpace
          nt         = PtrPrimType (ArrayPrimType nl scalarType) defaultAddrSpace
          line       = ConstantOperand $ ScalarConstant scalarType (fromIntegral l :: Word32)
          source     = ConstantOperand $ GlobalReference (PrimType st) s
          function   = ConstantOperand $ GlobalReference (PrimType ft) f
          name       = ConstantOperand $ GlobalReference (PrimType nt) n
          sourceSz   = ConstantOperand $ ScalarConstant scalarType sl
          functionSz = ConstantOperand $ ScalarConstant scalarType fl
          nameSz     = ConstantOperand $ ScalarConstant scalarType nl
      --
      ps   <- src `null_or` instr' (GetElementPtr source   [num numType 0, num numType 0 :: Operand Int32])
      pf   <- fun `null_or` instr' (GetElementPtr function [num numType 0, num numType 0 :: Operand Int32])
      pn   <- nm  `null_or` instr' (GetElementPtr name     [num numType 0, num numType 0 :: Operand Int32])
      call' $ Lam primType line
            $ Lam primType ps
            $ Lam primType sourceSz
            $ Lam primType pf
            $ Lam primType functionSz
            $ Lam primType pn
            $ Lam primType nameSz
            $ Body (type' :: Type Word64) (Just Tail) "___tracy_alloc_srcloc_name"

zone_begin
    :: Operands SrcLoc
    -> CodeGen arch (Operands Zone)
zone_begin srcloc
  | not profilingIsEnabled = return (constant (eltR @Zone) 0)
  | otherwise              = do
      let active = ConstantOperand $ ScalarConstant scalarType (1 :: Int32)
      call' $ Lam primType (op primType srcloc)
            $ Lam primType active
            $ Body (type' :: Type Word64) (Just Tail) "___tracy_emit_zone_begin_alloc"

zone_end
    :: Operands SrcLoc
    -> CodeGen arch ()
zone_end srcloc
  | not profilingIsEnabled = return ()
  | otherwise              = void $ call' (Lam primType (op primType srcloc) (Body VoidType (Just Tail) "___tracy_emit_zone_end"))

null_or :: String -> CodeGen arch (Operand (Ptr Word8)) -> CodeGen arch (Operand (Ptr Word8))
null_or [] _ = return $ ConstantOperand $ NullPtrConstant type'
null_or _  x = x

