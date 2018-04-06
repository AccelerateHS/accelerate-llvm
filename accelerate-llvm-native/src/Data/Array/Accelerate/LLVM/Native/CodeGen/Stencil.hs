{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RebindableSyntax    #-}

module Data.Array.Accelerate.LLVM.Native.CodeGen.Stencil (

    mkStencil1, mkStencil2

) where

import Data.Array.Accelerate.AST                                          as AST
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.Analysis.Match
import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic hiding (negate)
import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Loop
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Stencil
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Compile.Cache

import Data.Array.Accelerate.LLVM.Native.Target
import Data.Array.Accelerate.LLVM.Native.CodeGen.Base
import Data.Array.Accelerate.LLVM.Native.CodeGen.Loop

import Data.Array.Accelerate.LLVM.CodeGen.Skeleton

import qualified LLVM.AST.Global                                          as LLVM

import Data.Proxy
import Data.String

import Prelude
import Control.Monad


-- Parameters for boundary region
faceIndex
    :: ( IR Int                 -- The face index
       , [LLVM.Parameter]
       )
faceIndex =
  let
    faceIndex = "faceIndex"
  in
    ( eltLocal faceIndex
    , eltParameter faceIndex
    )


-- Parameters for inner region
range
    :: (Shape sh)
    => Proxy sh                 -- Dummy type
    -> ( IR sh                  -- The multidimensional start index
       , IR sh                  -- The multidimensional end   index
       , [LLVM.Parameter]
       )
range _ =
  let
    start = "start"
    end   = "end"
  in
    ( eltLocal start
    , eltLocal end
    , join
        [ eltParameter start
        , eltParameter end
        ]
    )


eltLocal = undefined
eltParameter = undefined


mkStencil1
    :: forall aenv stencil a b sh. (Skeleton Native, Stencil sh a stencil, Elt b)
    => Native
    -> UID
    -> Gamma aenv
    -> IRFun1 Native aenv (stencil -> b)
    -> IRBoundary Native aenv (Array sh a)
    -> IRManifest Native aenv (Array sh a)
    -> CodeGen (IROpenAcc Native aenv (Array sh b))
mkStencil1 _arch uid aenv f b1 ir1 =
  let
    (faceN, boundaryParams)                               = faceIndex
    (innerStart :: IR sh, innerEnd :: IR sh, innerParams) = range (Proxy :: Proxy sh)
    (arrOut, paramOut)                                    = mutableArray ("out" :: Name (Array sh b))
    paramEnv                                              = envParam aenv
  in foldr1 (+++) <$> sequence
  [ makeOpenAcc uid "stencil1_boundary" (boundaryParams ++ paramOut ++ paramEnv) $
        mkStencil1_boundary aenv arrOut f b1 ir1 faceN
  , makeOpenAcc uid "stencil1_inner"    (innerParams    ++ paramOut ++ paramEnv) $
        mkStencil1_inner    aenv arrOut f b1 ir1 innerStart innerEnd
  ]


mkStencil1_boundary
  :: forall a b sh aenv stencil. (Shape sh, Stencil sh a stencil, Elt b)
  => Gamma aenv
  -> IRArray (Array sh b)
  -> IRFun1 Native aenv (stencil -> b)
  -> IRBoundary Native aenv (Array sh a)
  -> IRManifest Native aenv (Array sh a)
  -> IR Int
  -> CodeGen ()
mkStencil1_boundary aenv arrOut f b1 ir1@(IRManifest v1) faceN =
  do
    (start, end) <- calculateFace faceN (irArrayShape arrOut) (boundaryThickness (AST.stencil :: StencilR sh a stencil))
    mkStencil1_inner aenv arrOut f b1 ir1 start end


calculateFace
  :: forall sh. (Shape sh)
  => IR Int
  -> IR sh
  -> IR sh
  -> CodeGen (IR sh, IR sh)
calculateFace n (IR e) (IR t) = do
  (sh, sz) <- go n (eltType (undefined::sh)) e t
  return (IR sh, IR sz)

  where
    go :: IR Int -> TupleType t -> Operands t -> Operands t -> CodeGen (Operands t, Operands t)
    go _ TypeRunit OP_Unit OP_Unit
      = return (OP_Unit, OP_Unit)
    --
    go n (TypeRpair tt' tt) (OP_Pair sh' sh) (OP_Pair sz' sz)
      | Just Refl <- matchTupleType tt (eltType (undefined::Int))
      = do
            n' <- sub numType n (int (-2))
            (start', end') <- go n' tt' sh' sz'
            --
            (IR start, IR end) :: (IR Int, IR Int) <- unpair <$>
              if      lt singleType n (int 0) then pair <$> return (int 0)              <*> return (IR sh)
              else if eq singleType n (int 0) then pair <$> return (int 0)              <*> return (IR sz)
              else if eq singleType n (int 1) then pair <$> sub numType (IR sh) (IR sz) <*> return (IR sh)
              else    {- n > 1 -}                  pair <$> return (IR sz)              <*> sub numType (IR sh) (IR sz)


            -- IR (OP_Pair (OP_Pair OP_Unit start) end) :: IR (Int, Int) <-
            --   ifThenElse (lt singleType n (int 0)) (return $ pair     (int 0)                         (IR sh)                    ) $
            --   ifThenElse (eq singleType n (int 0)) (return $ pair     (int 0)                         (IR sz)                    ) $
            --   ifThenElse (eq singleType n (int 1)) (         pair <$> sub numType (IR sh) (IR sz) <*> return (IR sh)             ) $
            --   {- else n > 1 -}                     (         pair <$> return (IR sz)              <*> sub numType (IR sh) (IR sz))
            --
            return (OP_Pair start' start, OP_Pair end' end)


boundaryThickness
  :: StencilR sh a stencil
  -> IR sh
boundaryThickness stencilR = undefined
  where
    go :: StencilR sh a stencil -> sh
    go StencilRunit3 = Z :. 1
    go StencilRunit5 = Z :. 2
    go StencilRunit7 = Z :. 3
    go StencilRunit9 = Z :. 4
    --
    go (StencilRtup3 a b c            ) = maxShapes [go a, go b, go c] :. 1
    go (StencilRtup5 a b c d e        ) = maxShapes [go a, go b, go c, go d, go e] :. 2
    go (StencilRtup7 a b c d e f g    ) = maxShapes [go a, go b, go c, go d, go e, go f, go g] :. 3
    go (StencilRtup9 a b c d e f g h i) = maxShapes [go a, go b, go c, go d, go e, go f, go g, go h, go i] :. 4


maxShapes :: [shape] -> shape
maxShapes = undefined


maxShape :: shape -> shape -> shape
maxShape a b = undefined


magicshapetoir :: sh -> IR sh
magicshapetoir = undefined


mkStencil1_inner
  :: forall a b sh aenv stencil. (Shape sh, Stencil sh a stencil, Elt b)
  => Gamma aenv
  -> IRArray (Array sh b)
  -> IRFun1 Native aenv (stencil -> b)
  -> IRBoundary Native aenv (Array sh a)
  -> IRManifest Native aenv (Array sh a)
  -> IR sh
  -> IR sh
  -> CodeGen ()
mkStencil1_inner aenv arrOut f b1 ir1@(IRManifest v1) start end =
  imapNestedFromTo start end (irArrayShape arrOut) $ \ix i -> do
      s <- stencilAccess b1 (irArray (aprj v1 aenv)) ix
      r <- app1 f s
      writeArray arrOut i r


mkStencil2
    :: forall aenv stencil1 stencil2 a1 a2 b sh. (Skeleton Native, Stencil sh a1 stencil1, Stencil sh a2 stencil2, Elt b)
    => Native
    -> UID
    -> Gamma aenv
    -> IRFun2 Native aenv (stencil1 -> stencil2 -> b)
    -> IRBoundary Native aenv (Array sh a1)
    -> IRManifest Native aenv (Array sh a1)
    -> IRBoundary Native aenv (Array sh a2)
    -> IRManifest Native aenv (Array sh a2)
    -> CodeGen (IROpenAcc Native aenv (Array sh b))
mkStencil2 = undefined