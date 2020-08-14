-- |
-- Module      : GHC.Heap.NormalForm
-- Copyright   : [2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- https://github.com/input-output-hk/cardano-prelude/blob/96e8dcb29dc3c29eee99c0d020152fad6071af6d/src/Cardano/Prelude/GHC/Heap/NormalForm.hs
--
-- This code has been adapted from the module "GHC.AssertNF" of the package
-- <http://hackage.haskell.org/package/ghc-heap-view ghc-heap-view>
-- (<https://github.com/nomeata/ghc-heap-view GitHub>) authored by
-- Joachim Breitner.
--
-- To avoid space leaks and unwanted evaluation behaviour, the programmer
-- might want his data to be fully evaluated at certain positions in the
-- code. This can be enforced, for example, by ample use of
-- "Control.DeepSeq", but this comes at a cost.
--
-- Experienced users hence use 'Control.DeepSeq.deepseq' only to find out
-- about the existence of space leaks and optimize their code to not create
-- the thunks in the first place, until the code no longer shows better
-- performance with 'deepseq'.
--

module GHC.Heap.NormalForm (

  isHeadNormalForm,
  isNormalForm,

) where

import GHC.Exts.Heap

-- Everything is in normal form, unless it is a thunk explicitly marked as
-- such. Indirection are also considered to be in HNF.
--
isHeadNormalForm :: Closure -> IO Bool
isHeadNormalForm c = do
  case c of
    ThunkClosure{}    -> return False
    APClosure{}       -> return False
    SelectorClosure{} -> return False
    BCOClosure{}      -> return False
    _                 -> return True

-- | The function 'isNormalForm' checks whether its argument is fully evaluated
-- and deeply evaluated.
--
-- NOTE 1: If you want to override the behaviour of 'isNormalForm' for specific
-- types (in particular, for specific types that may be /nested/ somewhere
-- inside the @a@), consider using
-- 'Cardano.Prelude.GHC.Heap.NormalForm.Classy.noUnexpectedThunks' instead.
--
-- NOTE 2: The normal form check can be quite brittle, especially with @-O0@.
-- For example, writing something like
--
-- > let !(Value x) = ... in ....
--
-- might translate to
--
-- > let !.. = ... in ... (case ... of Value x -> x)
--
-- which would trivially be @False@. In general, 'isNormalForm' should probably
-- only be used with @-O1@, but even then the answer may still depend on
-- internal decisions made by ghc during compilation.
--
isNormalForm :: a -> IO Bool
isNormalForm x = isNormalFormBoxed (asBox x)

isNormalFormBoxed :: Box -> IO Bool
isNormalFormBoxed b = do
  c  <- getBoxedClosureData b
  nf <- isHeadNormalForm c
  if nf
    then do
      c' <- getBoxedClosureData b
      allM isNormalFormBoxed (allClosures c')
    else do
      return False

-- From Control.Monad.Loops in monad-loops
--
allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ []       = return True
allM p (x : xs) = do
  q <- p x
  if q
    then allM p xs
    else return False

