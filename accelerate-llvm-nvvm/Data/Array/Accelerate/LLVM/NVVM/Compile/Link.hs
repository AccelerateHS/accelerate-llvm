{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.Compile.Link
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.Compile.Link (

  withLibdevice

) where

-- llvm-general
import LLVM.General.Context
import qualified LLVM.General.Module                            as LLVM

import LLVM.General.AST                                         as AST
import LLVM.General.AST.Attribute
import LLVM.General.AST.CallingConvention
import LLVM.General.AST.Global                                  as G
import LLVM.General.AST.Linkage

-- accelerate
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Monad                 () -- instance IsString Name

import Data.Array.Accelerate.LLVM.NVVM.Compile.Libdevice

import qualified Data.Array.Accelerate.LLVM.NVVM.Debug          as Debug

-- cuda
import Foreign.CUDA.Analysis

-- standard library
import Control.Monad.Error
import Data.HashSet                                             ( HashSet )
import Data.List
import Data.Monoid
import Text.Printf
import qualified Data.HashSet                                   as Set

#include "accelerate.h"


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
withLibdevice :: DeviceProperties -> Context -> Module -> (LLVM.Module -> IO a) -> IO a
withLibdevice dev ctx (analyse -> (externs, ast)) next =
  case Set.null externs of
    True        -> runError $ LLVM.withModuleFromAST ctx ast next
    False       ->
      runError $ LLVM.withModuleFromAST ctx ast                                    $ \mdl  ->
      runError $ LLVM.withModuleFromAST ctx nvvmReflect                            $ \refl ->
      runError $ LLVM.withModuleFromAST ctx (internalise externs (libdevice arch)) $ \libd -> do
        runError $ LLVM.linkModules False libd refl
        runError $ LLVM.linkModules False libd mdl
        Debug.message Debug.dump_cc msg
        next libd
  where
    arch        = computeCapability dev
    runError e  = either (INTERNAL_ERROR(error) "withLibdeviceNVPTX") id `fmap` runErrorT e

    msg         = printf "cc: linking with libdevice: %s"
                $ intercalate ", " (map (\(Name s) -> s) (Set.toList externs))


-- | Analyse the LLVM AST module and determine if any of the external
-- declarations should be substituted for functions defined in libdevice. The
-- set of functions that must be linked against libdevice are returned.
--
analyse :: Module -> (HashSet Name, Module)
analyse Module{..} =
  let (externs, defs)   = foldr1 (<>) $ map subst moduleDefinitions
  in
  (externs, Module { moduleDefinitions = defs, ..})


-- | Substitute suitable external declarations to calls to libdevice functions.
-- If an appropriate declaration is found (say 'sinf'), then we output two
-- declarations:
--
--   * The first is a replacement for the existing global declaration, that
--     replaces the empty function body with a call to the equivalent libdevice
--     function. The function is marked 'AlwaysInline', which will eliminate the
--     indirection.
--
--   * The second global is a declaration to the invoked libdevice function.
--     During compilation, the module must be linked against the libdevice
--     bitcode file.
--
-- If no substitutions are made then the Definition is returned unaltered. If
-- the substitution is made, the name of the libdevice function is returned in a
-- singleton set as the first parameter.
--
subst :: Definition -> (HashSet Name, [Definition])
subst (GlobalDefinition Function{..})
  | null basicBlocks
  , Set.member name libdeviceIndex
  , let __nv_name       = let Name f = name in Name ("__nv_" ++ f)
        attrs           = AlwaysInline : functionAttributes
        args            = [ n | Parameter _ n _ <- fst parameters ]
        toArgs          = map (\x -> (local x, []))
        blocks          = [ BasicBlock "" [ UnName 0 := Call False C [] (Right (global __nv_name)) (toArgs args) attrs [] ]
                                          ( Do        $ Ret (Just (local (UnName 0))) [])
                          ]
  = ( Set.singleton __nv_name
    , [ GlobalDefinition (Function { basicBlocks = blocks, functionAttributes = attrs, .. })
      , GlobalDefinition (Function { name = __nv_name, .. }) ]
    )

subst x = (Set.empty, [x])


-- | Mark all definitions in the module as internal linkage. This means that
-- unused definitions can be removed as dead code. Be careful to leave any
-- declarations as external.
--
internalise :: HashSet Name -> Module -> Module
internalise externals Module{..} =
  let internal (GlobalDefinition Function{..})
        | not (Set.member name externals)       -- we don't call this function directly; and
        , not (null basicBlocks)                -- it is not an external declaration
        = GlobalDefinition (Function { linkage=Internal, .. })

      internal x
        = x
  in
  Module { moduleDefinitions = map internal moduleDefinitions, .. }

