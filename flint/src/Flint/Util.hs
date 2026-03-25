module Flint.Util
  ( module Flint.Util
  ) where

import Flint.Prelude

import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import qualified Data.UUID as UUID
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text.IO as TextIO
import System.IO.Unsafe (unsafePerformIO)


incUUID :: UUID -> UUID
incUUID = offsetUUID 1

offsetUUID :: Word64 -> UUID -> UUID
offsetUUID n = uncurry UUID.fromWords64 . bimap (+n) (+n) . UUID.toWords64

-- from GPT
timeIt :: IO a -> IO (a, NominalDiffTime)
timeIt action = do
  start <- getCurrentTime        -- :: IO UTCTime
  result <- action
  end <- getCurrentTime
  let dt = diffUTCTime end start -- :: NominalDiffTime
  return (result, dt)

{-# NOINLINE samplingTimingEnabled #-}
samplingTimingEnabled :: IORef Bool
samplingTimingEnabled = unsafePerformIO $ newIORef False

enableSamplingTiming :: IO ()
enableSamplingTiming = writeIORef samplingTimingEnabled True

timingLog :: MonadIO m => Text -> m ()
timingLog msg = liftIO $ readIORef samplingTimingEnabled >>= \case
  False -> return ()
  True -> TextIO.hPutStrLn stderr msg
