module Flint.Util
  ( module Flint.Util
  ) where

import Flint.Prelude

import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import qualified Data.UUID as UUID


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
