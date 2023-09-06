module Flint.Prelude
  ( module Exports
  , hoistMaybeM
  ) where

import Blaze.Prelude as Exports

import Control.Monad.Extra as Exports (mapMaybeM)
import System.Random as Exports (randomRIO)
import Control.Concurrent.Async as Exports (replicateConcurrently)


hoistMaybeM :: Monad m => m (Maybe a) -> MaybeT m a
hoistMaybeM m = lift m >>= maybe mzero return

      
