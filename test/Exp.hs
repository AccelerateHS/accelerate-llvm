{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-unused-imports      #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}

-- llvm-general
import LLVM.General
import LLVM.General.Context
import LLVM.General.PassManager
import qualified LLVM.General.AST                       as AST
import qualified LLVM.General.AST.Global                as AST

-- accelerate
import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.Trafo -- .Sharing
import Data.Array.Accelerate.Array.Sugar                ( Elt, eltType )
import qualified Data.Array.Accelerate.AST              as AST

import Data.Array.Accelerate.LLVM.AST
import Data.Array.Accelerate.LLVM.CodeGen
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type
import Data.Array.Accelerate.LLVM.Compile
import Data.Array.Accelerate.LLVM.State
import Data.Array.Accelerate.LLVM.Target

import Data.Array.Accelerate.LLVM.Native.Execute

-- standard library
import Prelude                                          as P
import Data.Maybe
import Control.Exception
import Control.Monad.Error
import Control.Monad.Trans
import qualified Data.IntMap                            as IM


main :: IO ()
main = do
  let f :: Exp Int32 -> Exp Float
      f x = cos . sin . A.fromIntegral $ (x + 1) * 4 - (x*x)

      g :: Exp Int32 -> Exp Int32
      g x = let y = f x
            in  y >* 0 ? ( x + 1, x - 1 )

      l :: Exp Int32 -> Exp Int32
      l = A.iterate (constant 10) g

      acc :: Acc (Vector Int32)
      acc = A.map (+1) (use (fromList (Z:.10) [0..]))
  --
  evalLLVM $ do
    eacc <- compileAcc (convertAcc acc)
    v    <- executeAcc eacc
    liftIO $ print v


{--
-- Traverse the annotated AST and dump any LLVM modules found.
--
printModules :: ExecOpenAcc LL aenv a -> LLVM ()
printModules = travA
  where
    printM :: ExecutableR LL -> LLVM ()
    printM (LL m) = liftIO (putStrLn =<< llvmOfModule (unModule m))

    travA :: ExecOpenAcc LL aenv a -> LLVM ()
    travA EmbedAcc{}           = return ()
    travA (ExecAcc mdl _ pacc) =
      case pacc of
           AST.Alet a b     -> travA a >> travA b
           AST.Use _        -> return ()
           AST.Map _ a      -> printM mdl >> travA a


-- Perform the most common optimisations
--
opt :: PassSetSpec
opt = defaultCuratedPassSetSpec -- { optLevel = Just 3 }

-- Lower a Haskell LLVM AST into a C++ objects, then generate LLVM assembly for
-- the module.
--
llvmOfModule :: AST.Module -> IO String
llvmOfModule m =
  return $ show (kernelsOf m)
{--
  fmap (either (\s -> error (s P.++ "\n\n" P.++ show m)) id)
    $ withContext $ \ctx ->
        runErrorT $ withModuleFromAST ctx m $ \mdl ->
--          withPassManager opt $ \pm -> do
--            runPassManager pm mdl       -- returns whether any changes were made
            moduleLLVMAssembly mdl
--}


kernelsOf :: AST.Module -> [AST.Name]
kernelsOf m = mapMaybe extract (AST.moduleDefinitions m)
  where
    extract (AST.GlobalDefinition AST.Function{..})
      | P.not (P.null basicBlocks) = Just name
    extract _                      = Nothing
--}
