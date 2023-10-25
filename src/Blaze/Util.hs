-- | Utility functions for tests
module Blaze.Util where

import Blaze.Prelude

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
