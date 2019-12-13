{-# LANGUAGE CPP             #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Embed
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Embed (

  module Data.Array.Accelerate.LLVM.Embed,

) where

import Data.ByteString.Short.Extra                                  as BS

import Data.Array.Accelerate.Lifetime

import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.Embed

import Data.Array.Accelerate.LLVM.PTX.Compile
import Data.Array.Accelerate.LLVM.PTX.Link
import Data.Array.Accelerate.LLVM.PTX.Target
import Data.Array.Accelerate.LLVM.PTX.Context

import qualified Foreign.CUDA.Driver                                as CUDA

import Foreign.Ptr
import GHC.Ptr                                                      ( Ptr(..) )
import Language.Haskell.TH                                          ( Q, TExp )
import System.IO.Unsafe
import qualified Data.ByteString                                    as B
import qualified Data.ByteString.Unsafe                             as B
import qualified Language.Haskell.TH                                as TH
import qualified Language.Haskell.TH.Syntax                         as TH


instance Embed PTX where
  embedForTarget = embed

-- Embed the given object code and set up to be reloaded at execution time.
--
embed :: PTX -> ObjectR PTX -> Q (TExp (ExecutableR PTX))
embed target (ObjectR _ cfg obj) = do
  -- Load the module to recover information such as number of registers and
  -- bytes of shared memory. It may be possible to do this without requiring an
  -- active CUDA context.
  kmd <- TH.runIO $ withContext (ptxContext target) $ do
            jit <- B.unsafeUseAsCString obj $ \p -> CUDA.loadDataFromPtrEx (castPtr p) []
            ks  <- mapM (uncurry (linkFunctionQ (CUDA.jitModule jit))) cfg
            CUDA.unload (CUDA.jitModule jit)
            return ks

  -- Generate the embedded kernel executable. This will load the embedded object
  -- code into the current (at execution time) context.
  [|| unsafePerformIO $ do
        jit <- CUDA.loadDataFromPtrEx $$( TH.unsafeTExpCoerce [| Ptr $(TH.litE (TH.StringPrimL (B.unpack obj))) |] ) []
        fun <- newLifetime (FunctionTable $$(listE (map (linkQ 'jit) kmd)))
        return $ PTXR fun
   ||]
  where
    linkQ :: TH.Name -> (Kernel, Q (TExp (Int -> Int))) -> Q (TExp Kernel)
    linkQ jit (Kernel name _ dsmem cta _, grid) =
      [|| unsafePerformIO $ do
            f <- CUDA.getFun (CUDA.jitModule $$(TH.unsafeTExpCoerce (TH.varE jit))) $$(liftSBS name)
            return $ Kernel $$(liftSBS name) f dsmem cta $$grid
       ||]

    listE :: [Q (TExp a)] -> Q (TExp [a])
    listE xs = TH.unsafeTExpCoerce (TH.listE (map TH.unTypeQ xs))

