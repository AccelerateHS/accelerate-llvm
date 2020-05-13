{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Compile.Libdevice.TH
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Compile.Libdevice.TH (

  nvvmReflectModule, nvvmReflectBitcode,
  libdeviceBitcode,

) where

import qualified LLVM.AST                                           as AST
import qualified LLVM.AST.Attribute                                 as AST
import qualified LLVM.AST.Global                                    as AST.G
import qualified LLVM.Context                                       as LLVM
import qualified LLVM.Module                                        as LLVM

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
import Language.Haskell.TH.Syntax                                   ( Q, TExp )
import System.Directory
import System.FilePath
import Text.Printf
import qualified Data.ByteString.Short                              as BS
import qualified Data.ByteString.Short.Char8                        as S8
import qualified Data.ByteString.Short.Extra                        as BS
import qualified Language.Haskell.TH                                as TH
import qualified Language.Haskell.TH.Syntax                         as TH


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
nvvmReflectModule :: AST.Module
nvvmReflectModule =
  AST.Module
    { AST.moduleName            = "nvvm-reflect"
    , AST.moduleSourceFileName  = BS.empty
    , AST.moduleDataLayout      = targetDataLayout @PTX
    , AST.moduleTargetTriple    = targetTriple @PTX
    , AST.moduleDefinitions     = [AST.GlobalDefinition $ AST.G.functionDefaults
      { AST.G.name                = AST.Name "__nvvm_reflect"
      , AST.G.returnType          = downcast (integralType :: IntegralType Int32)
      , AST.G.parameters          = ( [ptrParameter scalarType (UnName 0 :: Name (Ptr Int8))], False )
      , AST.G.functionAttributes  = map Right [AST.NoUnwind, AST.ReadNone, AST.AlwaysInline]
      , AST.G.basicBlocks         = []
      }]
    }


-- Lower the given NVVM Reflect module into bitcode.
--
nvvmReflectBitcode :: AST.Module -> Q (TExp (ShortByteString, ByteString))
nvvmReflectBitcode mdl = do
  let name = "__nvvm_reflect"
  --
  bs <- TH.runIO $ LLVM.withContext $ \ctx ->
                     LLVM.withModuleFromAST ctx mdl LLVM.moduleLLVMAssembly
  TH.unsafeTExpCoerce $ TH.tupE [ TH.unTypeQ (BS.liftSBS name)
                                , bsToExp bs ]


-- Load the libdevice bitcode file for the given compute architecture. The name
-- of the bitcode files follows the format:
--
--   libdevice.compute_XX.YY.bc
--
-- Where XX represents the compute capability, and YY represents a version(?) We
-- search the libdevice PATH for all files of the appropriate compute capability
-- and load the "most recent" (by sort order).
--
libdeviceBitcode :: Compute -> Q (TExp (ShortByteString, ByteString))
libdeviceBitcode compute = do
  let basename
        | CUDA.libraryVersion < 9000
        , Compute m n <- compute     = printf "libdevice.compute_%d%d" m n
        | otherwise                  = "libdevice"
      --
      err     = $internalError "libdevice" (printf "not found: %s.YY.bc" basename)
      best f  = basename `isPrefixOf` f && takeExtension f == ".bc"
#if MIN_VERSION_nvvm(0,10,0)
      base    = nvvmDeviceLibraryPath
#else
      base    = cudaInstallPath </> "nvvm" </> "libdevice"
#endif
  --
  files <- TH.runIO $ getDirectoryContents base
  --
  let name  = fromMaybe err . listToMaybe . sortBy (flip compare) $ filter best files
      path  = base </> name
  --
  TH.unsafeTExpCoerce $ TH.tupE [ TH.unTypeQ (BS.liftSBS (S8.pack name))
                                , embedFile path ]

