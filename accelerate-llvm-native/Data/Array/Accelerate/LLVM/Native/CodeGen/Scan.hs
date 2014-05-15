{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}

module Data.Array.Accelerate.LLVM.Native.CodeGen.Scan
  where

-- llvm-general
import LLVM.General.AST
import LLVM.General.Quote.LLVM

-- accelerate
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type

import Data.Array.Accelerate.LLVM.Native.CodeGen.Base

-- standard library
import Control.Monad


mkScanl1
    :: forall t aenv e. (Elt e)
    => Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRDelayed aenv (Vector e)
    -> CodeGen [Kernel t aenv (Vector e)]
mkScanl1 aenv f a = do
  let
      single [i]        = i
      single _          = $internalError "single" "expected single expression"

      arrTmp            = arrayData  (undefined::Vector e) "tmp"
      shTmp             = arrayShape (undefined::Vector e) "tmp"
      tmp               = IRDelayed {
          delayedExtent      = return (map local shTmp)
        , delayedLinearIndex = readArray arrTmp . single <=< toIRExp
        , delayedIndex       = $internalError "mkScanl1" "linear indexing of temporary array only"
        }
  [k1] <- mkScanl1Seq aenv f a
  [k2] <- mkScanl1Pre aenv f a
  [k3] <- mkScanl1Post aenv f a tmp
  return [k1,k2,k3]

mkScanl1Seq
    :: forall t aenv e. (Elt e)
    => Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRDelayed aenv (Vector e)
    -> CodeGen [Kernel t aenv (Vector e)]
mkScanl1Seq aenv combine IRDelayed{..} = do
  let
      (start, end, paramGang)   = gangParam
      paramEnv                  = envParam aenv
      arrOut                    = arrayData  (undefined::Vector e) "out"
      paramOut                  = arrayParam (undefined::Vector e) "out"
      intType                   = (typeOf (integralType :: IntegralType Int))

      ty_acc                    = llvmOfTupleType (eltType (undefined::e))
  k <- [llgM|
  define void @scanl1Seq (
    $params:(paramGang) ,
    $params:(paramOut) ,
    $params:(paramEnv)
    ) {
        $bbsM:("x" .=. delayedLinearIndex ([start]))
        $bbsM:(exec (writeArray arrOut start ("x" :: Name)))
        %start1 = add $type:(intType) $opr:(start), 1
        br label %for

      for:
        for $type:(intType) %i in %start1 to $opr:(end) with $types:(ty_acc) %x as %acc {
          $bbsM:("y" .=. delayedLinearIndex ("i" :: [Operand]))
          $bbsM:("z" .=. (combine ("acc" :: Name) ("y" :: Name)))
          $bbsM:(exec (writeArray arrOut "i" ("z" :: Name)))
          $bbsM:(execRet (return "z"))
        }
      end:
        ret void
    }
  |]
  return $ [Kernel k]

mkScanl1Pre
    :: forall t aenv e. (Elt e)
    => Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRDelayed aenv (Vector e)
    -> CodeGen [Kernel t aenv (Vector e)]
mkScanl1Pre aenv combine IRDelayed{..} = do
  let
      (start, end, paramGang)   = gangParam
      paramEnv                  = envParam aenv
      arrTmp                    = arrayData  (undefined::Vector e) "tmp"
      paramTmp                  = arrayParam (undefined::Vector e) "tmp"
      intType                   = (typeOf (integralType :: IntegralType Int))

      ty_acc                    = llvmOfTupleType (eltType (undefined::e))
  k <- [llgM|
  define void @scanl1Pre (
    $params:(paramGang) ,
    $type:(intType) %chunkSize,
    $params:(paramTmp) ,
    $params:(paramEnv)
    ) {
      for $type:(intType) %i in $opr:(start) to $opr:(end) {
        %ix = mul $type:(intType) %i, %chunkSize
        %last = add $type:(intType) %ix, %chunkSize
        br label %nextblock
        $bbsM:("x" .=. delayedLinearIndex ("ix" :: [Operand]))
        %start1 = add $type:(intType) %ix, 1
        br label %reduce
        reduce:
        for $type:(intType) %j in %start1 to %last with $types:(ty_acc) %x as %acc {
          $bbsM:("y" .=. delayedLinearIndex ("j" :: [Operand]))
          $bbsM:("z" .=. (combine ("acc" :: Name) ("y" :: Name)))
          $bbsM:(execRet (return "z"))
        }
        $bbsM:(execRet_ (writeArray arrTmp "i" ("acc" :: Name)))
      }
      ret void
    }
  |]
  return $ [Kernel k]

mkScanl1Post
    :: forall t aenv e. (Elt e)
    => Gamma aenv
    -> IRFun2    aenv (e -> e -> e)
    -> IRDelayed aenv (Vector e)
    -> IRDelayed aenv (Vector e)
    -> CodeGen [Kernel t aenv (Vector e)]
mkScanl1Post aenv combine inD tmpD = do
  let
      (start, end, paramGang)   = gangParam
      paramEnv                  = envParam aenv
      paramTmp                  = arrayParam (undefined::Vector e) "tmp"
      arrOut                    = arrayData  (undefined::Vector e) "out"
      paramOut                  = arrayParam (undefined::Vector e) "out"
      intType                   = (typeOf (integralType :: IntegralType Int))

      ty_acc                    = llvmOfTupleType (eltType (undefined::e))
  k <- [llgM|
  define void @scanl1Post (
    $params:(paramGang) ,
    $type:(intType) %lastChunk,
    $type:(intType) %chunkSize,
    $type:(intType) %sz,
    $params:(paramTmp) ,
    $params:(paramOut) ,
    $params:(paramEnv)
    ) {
      for $type:(intType) %i in $opr:(start) to $opr:(end) {
        %ix = mul $type:(intType) %i, %chunkSize
        %last1 = add $type:(intType) %ix, %chunkSize
        %c1 = icmp eq $type:(intType) %i, %lastChunk
        %last = select i1 %c1, $type:(intType) %sz, $type:(intType) %last1
        br label %nextblock
        $bbsM:("x1" .=. delayedLinearIndex inD ("ix" :: [Operand]))
        ;; sum up the partial sums to get the first element
        for $type:(intType) %k in 0 to %i with $types:(ty_acc) %x1 as %x {
          %i1 = sub $type:(intType) %i, 1
          br label %nextblock
          $bbsM:("a" .=. delayedLinearIndex tmpD ("i1" :: [Operand]))
          $bbsM:("b" .=. (combine ("x" :: Name) ("a" :: Name)))
          $bbsM:(execRet (return "b"))
        }
        $bbsM:(exec (writeArray arrOut "ix" ("x" :: Name)))
        %start1 = add $type:(intType) %ix, 1
        br label %reduce
        reduce:
        for $type:(intType) %j in %start1 to %last with $types:(ty_acc) %x as %acc {
          $bbsM:("y" .=. delayedLinearIndex inD ("j" :: [Operand]))
          $bbsM:("z" .=. (combine ("acc" :: Name) ("y" :: Name)))
          $bbsM:(exec (writeArray arrOut "j" ("z" :: Name)))
          $bbsM:(execRet (return "z"))
        }
        ret void
      }
      ret void
    }
  |]
  return $ [Kernel k]
