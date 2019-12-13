{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : System.Process.Extra
-- Copyright   : [2017..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module System.Process.Extra
  where

-- standard library
import Control.Concurrent
import Control.Exception
import Foreign.C                                                    ( Errno(..), ePIPE )
import GHC.IO.Exception                                             ( IOErrorType(..), IOException(..) )


-- | Fork a thread while doing something else, but kill it if there's an
-- exception.
--
-- This is important because we want to kill the thread that is holding the
-- Handle lock, because when we clean up the process we try to close that
-- handle, which could otherwise deadlock.
--
-- Stolen from the 'process' package.
--
withForkWait :: IO () -> (IO () -> IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `onException` killThread tid

ignoreSIGPIPE :: IO () -> IO ()
ignoreSIGPIPE =
  handle $ \e ->
    case e of
      IOError{..} | ResourceVanished <- ioe_type
                  , Just ioe         <- ioe_errno
                  , Errno ioe == ePIPE
                  -> return ()
      _ -> throwIO e

