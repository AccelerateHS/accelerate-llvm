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

import LLVM.AST.Type.Constant
import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Function
import LLVM.AST.Type.GetElementPtr
import LLVM.AST.Type.Global
import LLVM.AST.Type.Name
import LLVM.AST.Type.Operand
import LLVM.AST.Type.Representation
import qualified Data.Array.Accelerate.LLVM.Internal.LLVMPretty     as LP

import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Monad

import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Debug.Internal                         ( tracyIsEnabled, SrcLoc, Zone )

import Control.Monad
import Data.Char


call' :: GlobalFunction args t -> CodeGen arch (Operands t)
call' f = call f [NoUnwind, NoDuplicate]

global_string :: String -> CodeGen arch (Name (Ptr (LLArray Word8)), Word64)
global_string str = do
  let str0  = str ++ "\0"
      l     = fromIntegral (length str0)
  --
  nm <- freshGlobalName
  _  <- declareGlobalVar $ LP.Global
    { LP.globalSym = nameToPrettyS nm
    , LP.globalAttrs = LP.GlobalAttrs
        { LP.gaLinkage = Just LP.Private
        , LP.gaVisibility = Nothing
        , LP.gaAddrSpace = LP.defaultAddrSpace
        , LP.gaConstant = True }
    , LP.globalType = LP.Array l (LP.PrimType (LP.Integer 8))
    , LP.globalValue = Just $ LP.ValArray (LP.PrimType (LP.Integer 8)) [ LP.ValInteger (toInteger (ord c)) | c <- str0 ]
    , LP.globalAlign = Nothing
    , LP.globalMetadata = mempty
    }
  return (nm, l)

derefGlobalString :: Word64 -> Name (Ptr (LLArray Word8)) -> Constant (Ptr Word8)
derefGlobalString slen sname =
  -- Global references are _pointers_ to their values. A string is an
  -- [_ x i8], hence the global reference is an [_ x i8]*. The GEP needs
  -- to index the outer pointer (with a 0) and index the array (at index
  -- 0) to address the first i8 in the string; GEP then returns a pointer
  -- to this i8.
  ConstantGetElementPtr $ GEP
    (PrimType (ArrayPrimType slen scalarType))
    (GlobalReference (PrimType (PtrPrimType (ArrayPrimType slen scalarType) defaultAddrSpace)) sname)
    (ScalarConstant scalarType 0 :: Constant Int32)
    (GEPArray (ScalarConstant scalarType 0 :: Constant Int32) (GEPEmpty primType))


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
  let i8ptr_t = LP.PtrTo (LP.PrimType (LP.Integer 8)) defaultAddrSpace
      i32_t = LP.PrimType (LP.Integer 32)
  _       <- typedef "___tracy_source_location_data" $ LP.Struct [ i8ptr_t, i8ptr_t, i8ptr_t, i32_t, i32_t ]
  (s, sl) <- global_string src
  (f, fl) <- global_string fun
  (n, nl) <- global_string nm
  let
      source     = if null src then NullPtrConstant type' else derefGlobalString sl s
      function   = if null fun then NullPtrConstant type' else derefGlobalString fl f
      name       = if null nm  then NullPtrConstant type' else derefGlobalString nl n
  --
  v <- freshGlobalName
  _ <- declareGlobalVar $ LP.Global
    { LP.globalSym = nameToPrettyS v
    , LP.globalAttrs = LP.GlobalAttrs
        { LP.gaLinkage = Just LP.Internal
        , LP.gaVisibility = Nothing
        , LP.gaAddrSpace = LP.defaultAddrSpace
        , LP.gaConstant = True }
    , LP.globalType = LP.Alias (LP.Ident "___tracy_source_location_data")
    , LP.globalValue = Just $
        LP.ValStruct
          [ downcast name
          , downcast function
          , downcast source
          , LP.Typed (LP.PrimType (LP.Integer 32)) (LP.ValInteger (toInteger line))
          , LP.Typed (LP.PrimType (LP.Integer 32)) (LP.ValInteger (toInteger colour))
          ]
    , LP.globalAlign = Just 8
    , LP.globalMetadata = mempty
    }
  return v


alloc_srcloc_name
    :: Int      -- line
    -> String   -- source file
    -> String   -- function
    -> String   -- name
    -> CodeGen arch (Operands SrcLoc)
alloc_srcloc_name l src fun nm
  | not tracyIsEnabled = return (constant (eltR @SrcLoc) 0)
  | otherwise              = do
      (s, sl) <- global_string src
      (f, fl) <- global_string fun
      (n, nl) <- global_string nm
      let
          line       = ConstantOperand $ ScalarConstant scalarType (fromIntegral l :: Word32)
          source     = ConstantOperand $ if null src then NullPtrConstant type' else derefGlobalString sl s
          function   = ConstantOperand $ if null fun then NullPtrConstant type' else derefGlobalString fl f
          name       = ConstantOperand $ if null nm  then NullPtrConstant type' else derefGlobalString nl n
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
  | not tracyIsEnabled = return (constant (eltR @SrcLoc) 0)
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
  | not tracyIsEnabled = return (constant (eltR @Zone) 0)
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
  | not tracyIsEnabled = return ()
  | otherwise              =
      void . call' $ Lam primType (op primType zone)
                   $ Body VoidType (Just Tail) "___tracy_emit_zone_end"

