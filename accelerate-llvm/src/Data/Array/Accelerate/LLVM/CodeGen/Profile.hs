{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeApplications         #-}
{-# OPTIONS_HADDOCK hide #-}
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

  zone_begin, zone_begin_alloc,
  zone_end,

) where

import LLVM.AST.Type.AddrSpace
import LLVM.AST.Type.Constant
import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Function
import LLVM.AST.Type.Global
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
import Data.Array.Accelerate.Debug.Internal                         ( debuggingIsEnabled, SrcLoc, Zone )

import Control.Monad
import Data.Char


call' :: GlobalFunction args t -> CodeGen arch (Operands t)
call' f = call f [NoUnwind, NoDuplicate]

global_string :: String -> CodeGen arch (Name (Ptr Word8), Word64)
global_string str = do
  let str0  = str ++ "\0"
      l     = fromIntegral (length str0)
  --
  nm <- freshGlobalName
  _  <- declare $ LLVM.globalVariableDefaults
    { LLVM.name        = downcast nm
    , LLVM.isConstant  = True
    , LLVM.linkage     = LLVM.Private
    , LLVM.type'       = LLVM.ArrayType l LLVM.i8
    , LLVM.unnamedAddr = Just LLVM.GlobalAddr
    , LLVM.initializer = Just $ Constant.Array LLVM.i8 [ Constant.Int 8 (toInteger (ord c)) | c <- str0 ]
    }
  return (nm, l)


-- struct ___tracy_source_location_data
-- {
--     const char* name;
--     const char* function;
--     const char* file;
--     uint32_t line;
--     uint32_t color;
-- };
--
source_location_data :: String -> String -> String -> Int -> Word32 -> CodeGen arch (Name a)
source_location_data nm fun src line colour = do
#if MIN_VERSION_llvm_hs_pure(15,0,0)
  let i8ptr_t = LLVM.ptr
#else
  let i8ptr_t = LLVM.ptr LLVM.i8
#endif
  _       <- typedef "___tracy_source_location_data" . Just $ LLVM.StructureType False [ i8ptr_t, i8ptr_t, i8ptr_t, LLVM.i32, LLVM.i32 ]
  (s, sl) <- global_string src
  (f, fl) <- global_string fun
  (n, nl) <- global_string nm
  let
      st         = PtrPrimType (ArrayPrimType sl scalarType) defaultAddrSpace
      ft         = PtrPrimType (ArrayPrimType fl scalarType) defaultAddrSpace
      nt         = PtrPrimType (ArrayPrimType nl scalarType) defaultAddrSpace
      source     = if null src then NullPtrConstant type' else ConstantGetElementPtr scalarType (GlobalReference (PrimType st) s) [ScalarConstant scalarType 0, ScalarConstant scalarType 0 :: Constant Int32]
      function   = if null fun then NullPtrConstant type' else ConstantGetElementPtr scalarType (GlobalReference (PrimType ft) f) [ScalarConstant scalarType 0, ScalarConstant scalarType 0 :: Constant Int32]
      name       = if null nm  then NullPtrConstant type' else ConstantGetElementPtr scalarType (GlobalReference (PrimType nt) n) [ScalarConstant scalarType 0, ScalarConstant scalarType 0 :: Constant Int32]
  --
  v <- freshGlobalName
  _ <- declare $ LLVM.globalVariableDefaults
    { LLVM.name        = downcast v
    , LLVM.isConstant  = True
    , LLVM.linkage     = LLVM.Internal
    , LLVM.type'       = LLVM.NamedTypeReference "___tracy_source_location_data"
    , LLVM.alignment   = 8
    , LLVM.initializer = Just $
        Constant.Struct
          { Constant.structName   = Just "___tracy_source_location_data"
          , Constant.isPacked     = False
          , Constant.memberValues =
              [ downcast name
              , downcast function
              , downcast source
              , Constant.Int 32 (toInteger line)
              , Constant.Int 32 (toInteger colour)
              ]
          }
    }
  return v


alloc_srcloc_name
    :: Int      -- line
    -> String   -- source file
    -> String   -- function
    -> String   -- name
    -> CodeGen arch (Operands SrcLoc)
alloc_srcloc_name l src fun nm
  | not debuggingIsEnabled = return (constant (eltR @SrcLoc) 0)
  | otherwise              = do
      (s, sl) <- global_string src
      (f, fl) <- global_string fun
      (n, nl) <- global_string nm
      let
#if MIN_VERSION_llvm_hs_pure(15,0,0)
          gep_ix     = [ScalarConstant scalarType 0 :: Constant Int32]
#else
          gep_ix     = [ScalarConstant scalarType 0, ScalarConstant scalarType 0 :: Constant Int32]
#endif
          st         = PtrPrimType (ArrayPrimType sl scalarType) defaultAddrSpace
          ft         = PtrPrimType (ArrayPrimType fl scalarType) defaultAddrSpace
          nt         = PtrPrimType (ArrayPrimType nl scalarType) defaultAddrSpace
          line       = ConstantOperand $ ScalarConstant scalarType (fromIntegral l :: Word32)
          source     = ConstantOperand $ if null src then NullPtrConstant type' else ConstantGetElementPtr scalarType (GlobalReference (PrimType st) s) gep_ix
          function   = ConstantOperand $ if null fun then NullPtrConstant type' else ConstantGetElementPtr scalarType (GlobalReference (PrimType ft) f) gep_ix
          name       = ConstantOperand $ if null nm  then NullPtrConstant type' else ConstantGetElementPtr scalarType (GlobalReference (PrimType nt) n) gep_ix
          sourceSz   = ConstantOperand $ ScalarConstant scalarType (sl-1) -- null
          functionSz = ConstantOperand $ ScalarConstant scalarType (fl-1) -- null
          nameSz     = ConstantOperand $ ScalarConstant scalarType (nl-1) -- null
      --
      call' $ Lam primType line
            $ Lam primType source
            $ Lam primType sourceSz
            $ Lam primType function
            $ Lam primType functionSz
            $ Lam primType name
            $ Lam primType nameSz
            $ Body (type' @SrcLoc) (Just Tail) "___tracy_alloc_srcloc_name"

zone_begin
    :: Int      -- line
    -> String   -- source file
    -> String   -- function
    -> String   -- name
    -> Word32   -- colour
    -> CodeGen arch (Operands Zone)
zone_begin line src fun name colour
  | not debuggingIsEnabled = return (constant (eltR @SrcLoc) 0)
  | otherwise              = do
      srcloc <- source_location_data name fun src line colour
      let srcloc_ty = PtrPrimType (NamedPrimType "___tracy_source_location_data") defaultAddrSpace
      --
      call' $ Lam srcloc_ty (ConstantOperand (GlobalReference (PrimType srcloc_ty) srcloc))
            $ Lam primType (ConstantOperand (ScalarConstant scalarType (1 :: Int32)))
            $ Body (type' @SrcLoc) (Just Tail) "___tracy_emit_zone_begin"

zone_begin_alloc
    :: Int      -- line
    -> String   -- source file
    -> String   -- function
    -> String   -- name
    -> Word32   -- colour
    -> CodeGen arch (Operands Zone)
zone_begin_alloc line src fun name colour
  | not debuggingIsEnabled = return (constant (eltR @Zone) 0)
  | otherwise              = do
      srcloc <- alloc_srcloc_name line src fun name
      zone   <- call' $ Lam primType (op primType srcloc)
                      $ Lam primType (ConstantOperand (ScalarConstant scalarType (1 :: Int32)))
                      $ Body (type' @SrcLoc) (Just Tail) "___tracy_emit_zone_begin_alloc"
      when (colour /= 0) $
        void . call' $ Lam primType (op primType zone)
                     $ Lam primType (ConstantOperand (ScalarConstant scalarType colour))
                     $ Body VoidType (Just Tail) "___tracy_emit_zone_color"
      return zone

zone_end
    :: Operands Zone
    -> CodeGen arch ()
zone_end zone
  | not debuggingIsEnabled = return ()
  | otherwise              =
      void . call' $ Lam primType (op primType zone)
                   $ Body VoidType (Just Tail) "___tracy_emit_zone_end"

