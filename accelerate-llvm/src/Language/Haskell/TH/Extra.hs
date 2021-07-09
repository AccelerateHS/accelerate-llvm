{-# LANGUAGE CPP                 #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- |
-- Module      : Language.Haskell.TH.Extra
-- Copyright   : [2019..2021] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Language.Haskell.TH.Extra (

  module Language.Haskell.TH,
#if! MIN_VERSION_template_haskell(2,17,0)
  module Language.Haskell.TH.Extra,
#endif

) where

import Language.Haskell.TH

#if !MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH.Syntax                                   ( unTypeQ, unsafeTExpCoerce )
#if MIN_VERSION_template_haskell(2,16,0)
import GHC.Exts                                                     ( RuntimeRep, TYPE )
#endif
#endif


#if !MIN_VERSION_template_haskell(2,17,0)
#if MIN_VERSION_template_haskell(2,16,0)
type Code m (a :: TYPE (r :: RuntimeRep)) = m (TExp a)
type CodeQ (a :: TYPE (r :: RuntimeRep)) = Code Q a

unsafeCodeCoerce :: forall (r :: RuntimeRep) (a :: TYPE r). Q Exp -> CodeQ a
unsafeCodeCoerce = unsafeTExpCoerce

unTypeCode :: forall (r :: RuntimeRep) (a :: TYPE r). CodeQ a -> Q Exp
unTypeCode = unTypeQ

bindCode :: forall m a (r :: RuntimeRep) (b :: TYPE r). Monad m => m a -> (a -> Code m b) -> Code m b
bindCode = (>>=)

#else
type Code m a = m (TExp a)
type CodeQ a = Code Q a

unsafeCodeCoerce :: Q Exp -> Q (TExp a)
unsafeCodeCoerce = unsafeTExpCoerce

unTypeCode :: CodeQ a -> Q Exp
unTypeCode = unTypeQ

bindCode :: Monad m => m a -> (a -> Code m b) -> Code m b
bindCode = (>>=)
#endif
#endif

