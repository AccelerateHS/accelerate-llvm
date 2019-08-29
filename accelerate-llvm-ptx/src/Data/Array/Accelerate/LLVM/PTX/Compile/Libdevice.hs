{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Compile.Libdevice
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Compile.Libdevice (

  withLibdeviceNVVM,
  withLibdeviceNVPTX,

) where

-- llvm-hs
import LLVM.Context
import qualified LLVM.Module                                        as LLVM

import LLVM.AST                                                     as AST
import LLVM.AST.Global                                              as G
import LLVM.AST.Linkage

-- accelerate
import Data.Array.Accelerate.LLVM.PTX.Compile.Libdevice.Load
import qualified Data.Array.Accelerate.LLVM.PTX.Debug               as Debug

-- cuda
import Foreign.CUDA.Analysis

-- standard library
import Control.Monad
import Data.ByteString                                              ( ByteString )
import Data.ByteString.Short.Char8                                  ( ShortByteString )
import Data.HashSet                                                 ( HashSet )
import Data.List
import Data.Maybe
import Text.Printf
import qualified Data.ByteString.Short.Char8                        as S8
import qualified Data.ByteString.Short.Extra                        as BS
import qualified Data.HashSet                                       as Set


-- | Lower an LLVM AST to C++ objects and link it against the libdevice module,
-- iff any libdevice functions are referenced from the base module.
--
-- Note: [Linking with libdevice]
--
-- The CUDA toolkit comes with an LLVM bitcode library called 'libdevice' that
-- implements many common mathematical functions. The library can be used as a
-- high performance math library for targets of the LLVM NVPTX backend, such as
-- this one. To link a module 'foo' with libdevice, the following compilation
-- pipeline is recommended:
--
--   1. Save all external functions in module 'foo'
--
--   2. Link module 'foo' with the appropriate 'libdevice_compute_XX.YY.bc'
--
--   3. Internalise all functions not in the list from (1)
--
--   4. Eliminate all unused internal functions
--
--   5. Run the NVVMReflect pass (see note: [NVVM Reflect Pass])
--
--   6. Run the standard optimisation pipeline
--
withLibdeviceNVPTX
    :: DeviceProperties
    -> Context
    -> Module
    -> (LLVM.Module -> IO a)
    -> IO a
withLibdeviceNVPTX dev ctx ast next =
  case Set.null externs of
    True        -> LLVM.withModuleFromAST ctx ast next
    False       ->
      LLVM.withModuleFromAST ctx ast                          $ \mdl  ->
      LLVM.withModuleFromAST ctx nvvmReflect                  $ \refl ->
      LLVM.withModuleFromAST ctx (internalise externs libdev) $ \libd -> do
        LLVM.linkModules mdl refl
        LLVM.linkModules mdl libd
        Debug.traceIO Debug.dump_cc msg
        next mdl
  where
    -- Replace the target triple and datalayout from the libdevice.bc module
    -- with those of the generated code. This avoids warnings such as "linking
    -- two modules of different target triples..."
    libdev      = (libdevice arch) { moduleTargetTriple = moduleTargetTriple ast
                                   , moduleDataLayout   = moduleDataLayout ast
                                   }
    externs     = analyse ast
    arch        = computeCapability dev

    msg         = printf "cc: linking with libdevice: %s"
                $ intercalate ", "
                $ map S8.unpack
                $ Set.toList externs


-- | Lower an LLVM AST to C++ objects and prepare it for linking against
-- libdevice using the nvvm bindings, iff any libdevice functions are referenced
-- from the base module.
--
-- Rather than internalise and strip any unused functions ourselves, allow the
-- nvvm library to do so when linking the two modules together.
--
-- TLM: This really should work with the above method, however for some reason
-- we get a "CUDA Exception: function named symbol not found" error, even though
-- the function is clearly visible in the generated code. hmm...
--
withLibdeviceNVVM
    :: DeviceProperties
    -> Context
    -> Module
    -> ([(ShortByteString, ByteString)] -> LLVM.Module -> IO a)
    -> IO a
withLibdeviceNVVM dev ctx ast next =
  LLVM.withModuleFromAST ctx ast $ \mdl -> do
    when withlib $ Debug.traceIO Debug.dump_cc msg
    next lib mdl
  where
    externs             = analyse ast
    withlib             = not (Set.null externs)
    lib | withlib       = [ nvvmReflect, libdevice arch ]
        | otherwise     = []

    arch        = computeCapability dev

    msg         = printf "cc: linking with libdevice: %s"
                $ intercalate ", "
                $ map S8.unpack
                $ Set.toList externs


-- | Analyse the LLVM AST module and determine if any of the external
-- declarations are intrinsics implemented by libdevice. The set of such
-- functions is returned, and will be used when determining which functions from
-- libdevice to internalise.
--
analyse :: Module -> HashSet ShortByteString
analyse Module{..} =
  let intrinsic (GlobalDefinition Function{..})
        | null basicBlocks
        , Name n        <- name
        , "__nv_"       <- BS.take 5 n
        = Just n

      intrinsic _
        = Nothing
  in
  Set.fromList (mapMaybe intrinsic moduleDefinitions)


-- | Mark all definitions in the module as internal linkage. This means that
-- unused definitions can be removed as dead code. Be careful to leave any
-- declarations as external.
--
internalise :: HashSet ShortByteString -> Module -> Module
internalise externals Module{..} =
  let internal (GlobalDefinition Function{..})
        | Name n <- name
        , not (Set.member n externals)          -- we don't call this function directly; and
        , not (null basicBlocks)                -- it is not an external declaration
        = GlobalDefinition Function { linkage=Internal, .. }

      internal x
        = x
  in
  Module { moduleDefinitions = map internal moduleDefinitions, .. }

