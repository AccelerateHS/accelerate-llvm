{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Intrinsic
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Intrinsic (

  Intrinsic(..)

) where

-- accelerate-llvm
import LLVM.AST.Type.Name

-- libraries
import Data.HashMap.Strict                                      ( HashMap )
import qualified Data.HashMap.Strict                            as HashMap


-- | During code generation we need to know the name of functions implementing
-- certain intrinsic maths operations. Depending on the backend, these functions
-- may not be implemented using the standard C math library.
--
-- This class allows a backend to provide a mapping from the C math library
-- function name to the name of the function which should be called instead. The
-- default implementation maps to the llvm intrinsic. For example:
--
--   sqrtf      -> llvm.sqrt.f32
--   sqrt       -> llvm.sqrt.f64
--
class Intrinsic arch where
  intrinsicForTarget :: arch -> HashMap String Label
  intrinsicForTarget _ = llvmIntrinsic


llvmIntrinsic :: HashMap String Label
llvmIntrinsic =
  let floating base rest
          = (base,        Label ("llvm." ++ base ++ ".f64"))
          : (base ++ "f", Label ("llvm." ++ base ++ ".f32"))
          : (base ++ "l", Label ("llvm." ++ base ++ ".f128"))
          : rest
  in
  HashMap.fromList $ foldr floating []
    [ "sqrt"
    , "powi"
    , "sin"
    , "cos"
    , "pow"
    , "exp"
    , "exp2"
    , "log"
    , "log10"
    , "log2"
    , "fma"
    , "fabs"
    , "copysign"
    , "floor"
    , "ceil"
    , "trunc"
    , "rint"
    , "nearbyint"
    , "round"
    ]

