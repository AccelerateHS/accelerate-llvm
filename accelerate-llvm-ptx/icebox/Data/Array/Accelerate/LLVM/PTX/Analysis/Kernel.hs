{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Analysis.Kernel
-- Copyright   : [2017] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Analysis.Kernel (

  getResourceUsage,

) where

import Data.Array.Accelerate.Error

import Foreign.CUDA.Path

import Control.DeepSeq
import Control.Exception
import Data.Attoparsec.ByteString.Char8
import System.Exit
import System.FilePath
import System.IO
import System.Process
import System.Process.Extra
import Text.Printf
import Data.ByteString.Short                                        ( ShortByteString )
import qualified Data.ByteString.Char8                              as B
import qualified Data.ByteString.Short                              as S
import qualified Data.ByteString.Short.Char8                        as S8


-- | Determine the number of resources required for a given kernel by extracting
-- the meta-data from the kernel object file. This does not require loading the
-- module into an active context.
--
getResourceUsage
    :: FilePath             -- path to .sass
    -> ShortByteString      -- kernel function name
    -> IO (Int,Int,Int,Int) -- (registers, shared memory, constant memory, local memory)
getResourceUsage sass name =
  let
      flags = [ "--dump-resource-usage", "--function", S8.unpack name, sass ]
      cp    = (proc (cudaBinPath </> "cuobjdump") flags)
               { std_in  = NoStream
               , std_out = CreatePipe
               , std_err = CreatePipe
               }
  in
  withCreateProcess cp $ \Nothing (Just outh) (Just errh) ph -> do

    -- fork off threads to start consuming stdout and stderr
    out <- B.hGetContents outh  -- as a strict bytestring
    err <-   hGetContents errh  -- regular string

    withForkWait  (evaluate (rnf out)) $ \waitOut ->
     withForkWait (evaluate (rnf err)) $ \waitErr -> do

      -- wait on the output and close the handles (performs implicit flush)
      waitOut
      waitErr

      hClose outh
      hClose errh

    -- wait on the process
    ex <- waitForProcess ph
    case ex of
      ExitFailure r -> $internalError "getResourceUsage" (printf "cuobjdump %s (exit %d)\n%s" (unwords flags) r err)
      ExitSuccess   -> return ()

    -- parse the result
    let check (Done _ r)   = r
        check (Partial k)  = check (k B.empty)
        check (Fail _ _ e) = $internalError "getResourceUsage" (printf "parse failure: %s" e)

    return . check $ parse (usage name) out


-- Parser for the output of 'cuobjdump'. Returns the number of registers, and
-- number of bytes of shared memory, constant memory, and local memory,
-- respectively, required by the kernel.
--
-- The output looks something like this (though we restrict it to return the
-- resource usage of a single function only):
--
-- > Resource usage:
-- >  Common:
-- >   GLOBAL:110 CONSTANT[2]:296 CONSTANT[14]:16
-- >  Function calculate:
-- >   REG:7 STACK:400 SHARED:0 LOCAL:0 CONSTANT[0]:328 TEXTURE:0 SURFACE:0 SAMPLER:0
-- >  Function mysurf_func:
-- >   REG:18 STACK:0 SHARED:0 LOCAL:0 CONSTANT[0]:444 TEXTURE:0 SURFACE:1 SAMPLER:0
-- >  Function mytexsampler_func:
-- >   REG:42 STACK:0 SHARED:0 LOCAL:0 CONSTANT[0]:472 TEXTURE:4 SURFACE:0 SAMPLER:1
-- >  Function mysharedfunc:
-- >   REG:30 STACK:0 SHARED:20 LOCAL:0 CONSTANT[0]:192 CONSTANT[16]:440 TEXTURE:0 SURFACE:0 SAMPLER:0
--
-- see also: <http://docs.nvidia.com/cuda/cuda-binary-utilities/index.html>
--
usage :: ShortByteString -> Parser (Int,Int,Int,Int)
usage f = do
  endOfLine
  _         <- string "Resource usage:" <* skipSpace
  g         <- common         -- are we supposed to include this?
  _         <- function f
  (r,s,c,l) <- requires
  return $! (r,s,g+c,l)

common :: Parser Int
common = do
  _ <- string "Common:" <* endOfLine <* skipSpace
  _ <- global           <* skipSpace
  c <- constant
  return c

function
    :: ShortByteString
    -> Parser ()
function f = do
  _ <- string "Function " *> string (S.fromShort f) <* char ':' <?> "expected function '" ++ S8.unpack f ++ "'"
  endOfLine

requires :: Parser (Int,Int,Int,Int)
requires = do
  skipSpace
  r <- registers  <* skipSpace
  _ <- stack      <* skipSpace
  s <- shared     <* skipSpace
  l <- local      <* skipSpace
  c <- constant   <* skipSpace
  _ <- texture    <* skipSpace
  _ <- surface    <* skipSpace
  _ <- sampler
  endOfLine
  return (r,s,l,c)

global :: Parser Int
global = string "GLOBAL:" *> decimal <?> "global memory (bytes)"

registers :: Parser Int
registers = string "REG:" *> decimal <?> "register count"

stack :: Parser Int
stack = string "STACK:" *> decimal <?> "stack size (bytes)"

shared :: Parser Int
shared = string "SHARED:" *> decimal <?> "static shared memory (bytes)"

local :: Parser Int
local = string "LOCAL:" *> decimal <?> "local memory (bytes)"

constant :: Parser Int
constant = sum . map snd <$> many' (constant' <* skipSpace)

constant' :: Parser (Int, Int)
constant' =
  (,) <$> (string "CONSTANT[" *> decimal)   -- ??
      <*> (string "]:"        *> decimal)   -- number of bytes
      <?> "constant memory (bytes)"

texture :: Parser Int
texture = string "TEXTURE:" *> decimal <?> "texture reference count"

surface :: Parser Int
surface = string "SURFACE:" *> decimal <?> "surface reference count"

sampler :: Parser Int
sampler = string "SAMPLER:" *> decimal <?> "sampler reference count"

