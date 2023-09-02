module Flint.Prelude
  ( module Exports
  , hoistMaybeM
  ) where

import Blaze.Prelude as Exports

import Control.Monad.Extra as Exports (mapMaybeM)

hoistMaybeM :: Monad m => m (Maybe a) -> MaybeT m a
hoistMaybeM m = lift m >>= maybe mzero return
