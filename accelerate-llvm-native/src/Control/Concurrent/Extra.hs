{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# OPTIONS_GHC -fobject-code #-}
-- |
-- Module      : Control.Concurrent.Extra
-- Copyright   : [2021] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Control.Concurrent.Extra (

  getThreadId,

) where

import Data.Int
import Foreign.C.Types
import GHC.Conc                                                     ( ThreadId(..) )
import GHC.Exts                                                     ( ThreadId# )


-- Stolen from GHC.Conc.Sync
--
getThreadId :: ThreadId -> Int32
getThreadId (ThreadId t#) =
  case getThreadId# t# of
    CInt i -> i

foreign import ccall unsafe "rts_getThreadId" getThreadId# :: ThreadId# -> CInt

