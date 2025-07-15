-- | Utility functions for tests
module Blaze.Util where

import Blaze.Prelude

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HashMap


getMemoized
  :: (Monad m, Hashable a)
  => (a -> m b)
  -> a
  -> StateT (HashMap a b) m b
getMemoized f old = do
  m <- get
  case HashMap.lookup old m of
    Just new -> return new
    Nothing -> do
      new <- lift $ f old
      modify $ HashMap.insert old new
      return new

writeAsJSON :: ToJSON a => FilePath -> a -> IO ()
writeAsJSON fp = BL.writeFile fp . encode

-- throws error if can't decode
readAsJSON :: FromJSON a => FilePath -> IO a
readAsJSON fp = do
  r <- BL.readFile fp
  case decode r of
    Nothing -> error "Couldn't decode JSON"
    Just x -> return x
