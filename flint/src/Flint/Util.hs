module Flint.Util
  ( module Flint.Util
  ) where

import Flint.Prelude

import qualified Data.UUID as UUID
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan, readTChan)
import System.IO.Unsafe (unsafePerformIO)


incUUID :: UUID -> UUID
incUUID = uncurry UUID.fromWords64 . bimap (+1) (+1) . UUID.toWords64

-- | Prints out text to a handle in a single thread to ensure messages aren't jumbled.
-- This is good for printing within concurrent tasks.
{-# NOINLINE sequentialPutTextTo #-}
sequentialPutTextTo :: Handle -> Text -> IO ()
sequentialPutTextTo = unsafePerformIO $ do
  tchan <- newTChanIO :: IO (TChan (Handle, Text))
  void . forkOS $ do
    forever $ atomically (readTChan tchan) >>= uncurry hPutStrLn
  return $ runnerFunc tchan
  where
    runnerFunc :: TChan (Handle, Text) -> Handle -> Text -> IO ()
    runnerFunc tchan h t = atomically $ writeTChan tchan (h, t)

sequentialPutText :: Text -> IO ()
sequentialPutText = sequentialPutTextTo stdout

sequentialWarn :: Text -> IO ()
sequentialWarn = sequentialPutTextTo stderr
