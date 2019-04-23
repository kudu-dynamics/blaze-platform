{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE LambdaCase    #-}

module Hinja.Prelude
  ( module Exports
  , fromRight
  , liftEitherIO
  , liftMaybeIO
  , liftEitherM
  , liftMaybe
  , liftMaybeM
  ) where


import qualified Prelude as P
import Prelude as Exports (String, id)
import Data.Maybe as Exports (fromJust)
import Protolude as Exports
import Control.Lens.TH as Exports (makeLenses)

liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe e Nothing = throwError e
liftMaybe _ (Just x) = return x

liftMaybeM :: Monad m => e -> m (Maybe a) -> ExceptT e m a
liftMaybeM e m = ExceptT $ m >>= return . maybe (Left e) Right

--sort of redundant, actually...
liftEitherM :: m (Either e a) -> ExceptT e m a
liftEitherM = ExceptT

liftEither :: (MonadError e m) => Either e a -> m a
liftEither (Left e) = throwError e
liftEither (Right x) = return x

liftEitherIO :: (MonadError e m, MonadIO m) => IO (Either e a) -> m a
liftEitherIO m = liftIO m >>= liftEither

liftMaybeIO :: (MonadError e m, MonadIO m) => e -> IO (Maybe a) -> m a
liftMaybeIO e m = liftIO m >>= liftEither . maybe (Left e) Right

fromRight :: Either e a -> a
fromRight (Right x) = x
fromRight (Left _) = P.error "You called fromRight on a Left"
