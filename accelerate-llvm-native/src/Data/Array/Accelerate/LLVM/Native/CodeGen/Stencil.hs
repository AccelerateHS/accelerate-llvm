{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE GADTs               #-}

module Data.Array.Accelerate.LLVM.Native.CodeGen.Stencil (

    mkStencil1, mkStencil2

) where

import Data.Array.Accelerate.AST                                          as AST
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.Analysis.Match
import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
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


-- Parameters for boundary region
faceIndex
    :: ( IR Int                 -- The face index
       , [LLVM.Parameter]
       )
faceIndex =
  let
    faceIndex = "faceIndex"
  in
    ( local scalarType faceIndex
    , [ scalarParameter scalarType faceIndex
      ]
    )


-- Parameters for inner region
range
    :: (Shape sh)
    => ( IR sh                  -- The multidimensional start index
       , IR sh                  -- The multidimensional end   index
       , [LLVM.Parameter]
       )
range =
  let
    t     = undefined
    start = "start"
    end   = "end"
  in
    ( local t start
    , local t end
    , [ scalarParameter t start
      , scalarParameter t end
      ]
    )


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
    (innerStart :: IR sh, innerEnd :: IR sh, innerParams) = range
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
  let
    (start, end) = calculateFace faceN (irArrayShape arrOut) (boundaryThickness (AST.stencil :: StencilR sh a stencil))
  in
    mkStencil1_inner aenv arrOut f b1 ir1 start end


calculateFace = undefined
--   :: forall sh. (Shape sh)
--   => Int
--   -> IR sh
--   -> IR sh
--   -> (IR sh, IR sh)
-- calculateFace n e t =
--   let
--     (start, end) = go n e t
--   in
--     (IR start, IR end)
--   where
--     go _ Z _ = (Z, Z)
--     go n extent@(extent' :. e) thickness@(thickness' :. t)
--       | n == 0 = (start' :.      0 , end' :.      t )
--       | n == 1 = (start' :. (e - t), end' :.  e     )
--       | n >  1 = (start' :.      t , end' :. (e - t))
--       where
--         (start', end') = calculateFace (n - 2) extent' thickness'


boundaryThickness = undefined
-- boundaryThickness
  -- :: forall sh a stencil. (Shape sh, Stencil sh a stencil)
  -- => StencilR sh a stencil
  -- -> IR sh
-- boundaryThickness stencilR = (undefined :: Shape sh => sh -> IR sh) (go [stencilR])
--   where
--     -- go :: forall sh a stencil. (Shape sh, Stencil sh a stencil)
--     --    => [StencilR sh a stencil]
--     --    -> sh
--     -- go [] = Z
--     go stencilRs =
--       let
--         (sizes, nested) = Prelude.unzip . Prelude.map aux $ stencilRs
--       in
--         go (Prelude.concat nested) :. maximum sizes
--     --
--     aux StencilRunit3 = (1, [])
--     aux StencilRunit5 = (2, [])
--     aux StencilRunit7 = (3, [])
--     aux StencilRunit9 = (4, [])
--     --
--     aux (StencilRtup3 a b c)             = (1, [a, b, c])
--     aux (StencilRtup5 a b c d e)         = (2, [a, b, c, d, e])
--     aux (StencilRtup7 a b c d e f g)     = (3, [a, b, c, d, e, f, g])
--     aux (StencilRtup9 a b c d e f g h i) = (4, [a, b, c, d, e, f, g, h, i])


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