{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Array.Accelerate.LLVM.PTX.NoFib.RunQ where

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.PTX as GPU

import Test.Tasty
import Test.Tasty.HUnit


-- WARNING: This module is duplicated (apart from Native/PTX) between the
-- accelerate-llvm-native and accelerate-llvm-ptx backends. This code is not
-- included in the main Accelerate nofib testsuite because of staging issues:
-- the test can only be defined after runQ is known, and runQ is only built
-- after the 'accelerate' package has already finished building. It would be
-- possible to deduplicate the little Accelerate program in there, but that was
-- not deemed worth the effort.


test_runq :: TestTree
test_runq =
  testGroup "runQ"
    [ testCase "simple" test_simple ]

test_simple :: Assertion
test_simple = do
  let prog :: A.Vector Int -> A.Scalar Int
      !prog = $(GPU.runQ $ \a -> A.sum (A.map (+1) (a :: A.Acc (A.Vector Int))))
  let n = 10000
  prog (A.fromList (A.Z A.:. 10000) [1..]) @=? A.fromList A.Z [n * (n + 1) `div` 2 + n]
