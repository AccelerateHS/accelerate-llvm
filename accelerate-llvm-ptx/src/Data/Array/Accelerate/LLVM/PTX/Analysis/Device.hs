{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Analysis.Device
-- Copyright   : [2008..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Analysis.Device
  where

import Control.Exception
import Data.Function
import Data.List
import Data.Ord
import Foreign.CUDA.Analysis.Device
import Foreign.CUDA.Driver.Context                                  ( Context )
import Foreign.CUDA.Driver.Device
import Foreign.CUDA.Driver.Error
import qualified Foreign.CUDA.Driver                                as CUDA


-- Select the best of the available CUDA capable devices. This prefers devices
-- with higher compute capability, followed by maximum throughput.
--
-- For hosts with multiple devices in Exclusive Process mode, this will select
-- the first of the _available_ devices. If no devices are available, an
-- exception is thrown indicating that no devices are available.
--
selectBestDevice :: IO (Device, DeviceProperties, Context)
selectBestDevice = select =<< enumerateDevices
  where
    select :: [(Device, DeviceProperties)] -> IO (Device, DeviceProperties, Context)
    select []               = cudaErrorIO "No CUDA-capable devices are available"
    select ((dev,prp):rest) = do
      r <- try $ CUDA.create dev [CUDA.SchedAuto]
      case r of
        Right ctx               -> return (dev,prp,ctx)
        Left (_::CUDAException) -> select rest


-- Return the list of all connected CUDA devices, sorted by compute
-- compatibility, followed by maximum throughput.
--
-- Strictly speaking this may not necessary, as the default device enumeration
-- appears to be sorted by some metric already.
--
-- Ignore the possibility of emulation-mode devices, as this has been deprecated
-- as of CUDA v3.0 (compute-capability == 9999.9999)
--
enumerateDevices :: IO [(Device, DeviceProperties)]
enumerateDevices = do
  devs  <- mapM CUDA.device . enumFromTo 0 . subtract 1 =<< CUDA.count
  prps  <- mapM CUDA.props devs
  return $ sortBy (flip compareDevices `on` snd) (zip devs prps)


-- Return a ordering of two device with respect to (estimated) performance
--
compareDevices :: DeviceProperties -> DeviceProperties -> Ordering
compareDevices = cmp
  where
    compute     = computeCapability
    flops d     = multiProcessorCount d * coresPerMultiProcessor d * clockRate d
    cmp x y
      | compute x == compute y  = comparing flops   x y
      | otherwise               = comparing compute x y


-- Number of CUDA cores per streaming multiprocessor for a given architecture
-- revision. This is the number of SIMD arithmetic units per multiprocessor,
-- executing in lockstep in half-warp groupings (16 ALUs).
--
coresPerMultiProcessor :: DeviceProperties -> Int
coresPerMultiProcessor = coresPerMP . deviceResources

