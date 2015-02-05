{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : LLVM.General.AST.Type.Bits
-- Copyright   : [2015] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module LLVM.General.AST.Type.Bits
  where

import Data.Bits
import Data.Int
import Data.Type.Equality
import Data.Word
import Foreign.C.Types
import GHC.TypeLits


type a < b = (a + 1) <= b
type a > b = b <= (a + 1)

type family BitSize a :: Nat

type instance BitSize Int8    = 8
type instance BitSize Int16   = 16
type instance BitSize Int32   = 32
type instance BitSize Int64   = 64
type instance BitSize Word8   = 8
type instance BitSize Word16  = 16
type instance BitSize Word32  = 32
type instance BitSize Word64  = 64
type instance BitSize Char    = 32
type instance BitSize Bool    = 1

type instance BitSize CShort  = 16
type instance BitSize CUShort = 16
type instance BitSize CInt    = 32
type instance BitSize CUInt   = 32
type instance BitSize CLLong  = 64
type instance BitSize CULLong = 64
type instance BitSize CChar   = 8
type instance BitSize CUChar  = 8
type instance BitSize CSChar  = 8

type instance BitSize Float   = 32
type instance BitSize CFloat  = 32
type instance BitSize Double  = 64
type instance BitSize CDouble = 64

type instance BitSize Int    = $( case finiteBitSize (undefined::Int) of
                                    32 -> [t| 32 |]
                                    64 -> [t| 64 |]
                                    _  -> error "I don't know what architecture I am"  )

type instance BitSize Word   = $( case finiteBitSize (undefined::Word) of
                                    32 -> [t| 32 |]
                                    64 -> [t| 64 |]
                                    _  -> error "I don't know what architecture I am"  )

type instance BitSize CLong  = $( case finiteBitSize (undefined::CLong) of
                                    32 -> [t| 32 |]
                                    64 -> [t| 64 |]
                                    _  -> error "I don't know what architecture I am"  )


type instance BitSize CULong = $( case finiteBitSize (undefined::CULLong) of
                                    32 -> [t| 32 |]
                                    64 -> [t| 64 |]
                                    _  -> error "I don't know what architecture I am"  )

type family BitSizeEq a b :: Bool
type instance BitSizeEq a b = BitSize a == BitSize b

-- foo :: BitSizeEq a b ~ True => a -> b -> IO ()
-- foo _ _  = putStrLn "yes"
--
-- bar :: (BitSize a < BitSize b) => a -> b -> IO ()
-- bar _ _ = putStrLn "yes"

