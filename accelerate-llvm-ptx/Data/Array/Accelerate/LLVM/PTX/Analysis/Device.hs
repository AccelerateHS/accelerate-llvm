-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.Analysis.Device
-- Copyright   : [2008..2017] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.Analysis.Device
  where

import Data.Ord
import Data.List
import Data.Function
import Foreign.CUDA.Driver.Device
import Foreign.CUDA.Analysis.Device
import qualified Foreign.CUDA.Driver                            as CUDA


-- Select the best of the available CUDA capable devices. This prefers devices
-- with higher compute capability, followed by maximum throughput. This does not
-- take into account any other factors, such as whether the device is currently
-- in use by another process.
--
-- Ignore the possibility of emulation-mode devices, as this has been deprecated
-- as of CUDA v3.0 (compute-capability == 9999.9999)
--
selectBestDevice :: IO (Device, DeviceProperties)
selectBestDevice = do
  dev   <- mapM CUDA.device . enumFromTo 0 . subtract 1 =<< CUDA.count
  prop  <- mapM CUDA.props dev
  return . minimumBy (flip cmp `on` snd) $ zip dev prop
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

