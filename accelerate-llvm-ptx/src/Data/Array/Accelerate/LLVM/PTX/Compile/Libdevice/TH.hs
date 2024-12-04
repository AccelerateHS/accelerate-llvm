{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Compile.Libdevice.TH
-- Copyright   : [2017..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Compile.Libdevice.TH (

  nvvmReflectModule, -- nvvmReflectBitcode,
  libdeviceBitcode,

) where

import qualified Text.LLVM.AST                                      as LP
import qualified Text.LLVM.Triple.Parse                             as LP

import LLVM.AST.Type.Downcast
import LLVM.AST.Type.Representation

import Data.Array.Accelerate.Error
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.PTX.Target

import Foreign.CUDA.Analysis
import qualified Foreign.CUDA.Driver                                as CUDA
#if MIN_VERSION_nvvm(0,10,0)
import Foreign.NVVM.Path
#else
import Foreign.CUDA.Path
#endif

import Data.ByteString                                              ( ByteString )
import Data.ByteString.Short                                        ( ShortByteString )
import Data.FileEmbed
import Data.List
import Data.Maybe
import Formatting
import Data.Array.Accelerate.TH.Compat                              ( CodeQ )
import System.Directory
import System.FilePath
import Text.Printf
-- import qualified Data.ByteString.Short                              as BS
import qualified Data.ByteString.Short.Char8                        as S8
import qualified Data.ByteString.Short.Extra                        as BS
import qualified Data.Array.Accelerate.TH.Compat                    as TH


-- This is a hacky module that can be linked against in order to provide the
-- same functionality as running the NVVMReflect pass.
--
-- Note: [NVVM Reflect Pass]
--
-- To accommodate various math-related compiler flags that can affect code
-- generation of libdevice code, the library code depends on a special LLVM IR
-- pass (NVVMReflect) to handle conditional compilation within LLVM IR. This
-- pass looks for calls to the @__nvvm_reflect function and replaces them with
-- constants based on the defined reflection parameters.
--
-- libdevice currently uses the following reflection parameters to control code
-- generation:
--
--   * __CUDA_FTZ={0,1}     fast math that flushes denormals to zero
--
-- Since this is currently the only reflection parameter supported, and that we
-- prefer correct results over pure speed, we do not flush denormals to zero. If
-- the list of supported parameters ever changes, we may need to re-evaluate
-- this implementation.
--
nvvmReflectModule :: LP.Module
nvvmReflectModule =
  LP.Module
    { LP.modSourceName = Nothing
    , LP.modTriple     = case targetTriple @PTX of
                           Just s -> LP.parseTriple (S8.unpack s)
                           Nothing -> error "TODO: module target triple"
    , LP.modDataLayout = []  -- TODO: data layout for nvvm reflect module
    , LP.modTypes      = []
    , LP.modNamedMd    = []
    , LP.modUnnamedMd  = []
    , LP.modComdat     = mempty
    , LP.modGlobals    = []
    , LP.modDeclares   = []
    , LP.modDefines    =
        [LP.Define
          { LP.defLinkage    = Nothing
          , LP.defVisibility = Nothing
          , LP.defRetType    = downcast (integralType :: IntegralType Int32)
          , LP.defName       = LP.Symbol "__nvvm_reflect"
          , LP.defArgs       = [ptrParameter scalarType (UnName 0 :: Name (Ptr Int8))]
          , LP.defVarArgs    = False
          , LP.defAttrs      = [LP.Nounwind, LP.Readnone, LP.Alwaysinline]
          , LP.defSection    = Nothing
          , LP.defGC         = Nothing
          , LP.defBody       = []
          , LP.defMetadata   = mempty
          , LP.defComdat     = Nothing
          }]
    , LP.modInlineAsm  = []
    , LP.modAliases    = []
    }


-- Lower the given NVVM Reflect module into bitcode.
--
-- nvvmReflectBitcode :: LP.Module -> CodeQ (ShortByteString, ByteString)
-- nvvmReflectBitcode mdl =
--   let name = "__nvvm_reflect"
--   in
--   TH.runIO (LLVM.withContext $ \ctx -> LLVM.withModuleFromAST ctx mdl LLVM.moduleLLVMAssembly)
--     `TH.bindCode` \bs ->
--       TH.unsafeCodeCoerce $ TH.tupE [ TH.unTypeCode (BS.liftSBS name), bsToExp bs ]
