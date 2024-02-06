module Flint.Prelude
  ( module Exports
  , constrName
  , hoistMaybeM
  , removeNth
  , tryError
  ) where

import Blaze.Prelude as Exports hiding (Symbol)

import Control.Monad.Extra as Exports (mapMaybeM)
import System.Random as Exports (randomRIO)
import Control.Concurrent.Async as Exports (replicateConcurrently, forConcurrently)


hoistMaybeM :: Monad m => m (Maybe a) -> MaybeT m a
hoistMaybeM m = lift m >>= maybe mzero return

removeNth :: Int -> [a] -> [a]
removeNth 0 [] = []
removeNth 0 (_:xs) = xs
removeNth _ [] = []
removeNth n (x:xs) = x : removeNth (n - 1) xs

-- | 'MonadError' analogue to the 'Control.Exception.try' function.
tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)


----- Generic constructor names
constrName :: (HasConstructor (Rep a), Generic a) => a -> Text
constrName = cs . genericConstrName . from

class HasConstructor (f :: Type -> Type) where
  genericConstrName :: f x -> String

instance HasConstructor f => HasConstructor (D1 c f) where
  genericConstrName (M1 x) = genericConstrName x

instance (HasConstructor x, HasConstructor y) => HasConstructor (x :+: y) where
  genericConstrName (L1 l) = genericConstrName l
  genericConstrName (R1 r) = genericConstrName r

instance Constructor c => HasConstructor (C1 c f) where
  genericConstrName = conName
