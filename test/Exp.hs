{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- llvm-general
import LLVM.General
import LLVM.General.Context
import LLVM.General.PassManager
import qualified LLVM.General.AST                       as AST

-- accelerate
import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.Trafo.Sharing
import Data.Array.Accelerate.Array.Sugar                ( Elt, eltType )
import Data.Array.Accelerate.LLVM.CodeGen
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.CUDA
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type

-- standard library
import Prelude                                          as P
import Control.Monad.Error
import qualified Data.IntMap                            as IM


main :: IO ()
main = do
  let f :: Exp Int32 -> Exp Float
      f x = cos . sin . A.fromIntegral $ (x + 1) * 4 - (x*x)
  --
  putStrLn =<< llvmOfModule (testFun1 f)


testFun1 :: forall a b. (Elt a, Elt b) => (Exp a -> Exp b) -> AST.Module
testFun1 f =
  let ns        = varNames "x" (undefined :: a)
      ty        = llvmOfTupleType (eltType (undefined :: a))
      params    = P.zipWith (\t n -> AST.Parameter t n []) ty ns
      vars      = P.map AST.LocalReference ns
  in
  runLLVM "test" $ do
    body     <- llvmOfFun1 (convertFun True f) IM.empty vars
    return $ globalFunction "test" AST.VoidType params body


-- Perform the most common optimisations
--
opt :: PassSetSpec
opt = defaultCuratedPassSetSpec { optLevel = Just 3 }

-- Lower a Haskell LLVM AST into a C++ objects, then generate LLVM assembly for
-- the module.
--
llvmOfModule :: AST.Module -> IO String
llvmOfModule m =
  fmap (either error id)
    $ withContext $ \ctx ->
        runErrorT $ withModuleFromAST ctx m $ \mdl ->
          withPassManager opt $ \pm -> do
            runPassManager pm mdl       -- returns whether any changes were made
            moduleLLVMAssembly mdl

