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
import Control.Concurrent.Async as Exports (replicateConcurrently)

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
  genericConstrName x = conName x


-- newtype MatcherMonad a = MatcherMonad {
--   _runMatcher :: StateT Int (ExceptT () Identity) a
--   }
--   deriving newtype
--     ( Functor
--     , Applicative
--     , Monad
--     , MonadError ()
--     , MonadState Int
--     , Alternative
--     )

-- runMatcher :: MatcherMonad a -> Int -> Either () (a, Int)
-- runMatcher action s
--   = runIdentity
--   . runExceptT
--   . flip runStateT s
--   . _runMatcher
--   $ action

-- add5 :: MatcherMonad ()
-- add5 = modify (+5)

-- add1 :: MatcherMonad ()
-- add1 = modify (+1)

-- foodoo :: MatcherMonad ()
-- foodoo = asum [(add5 >> throwError ()), add1, add5]







-- newtype MatcherMonad a = MatcherMonad {
--   _runMatcher :: ExceptT () (StateT Int Identity) a
--   }
--   deriving newtype
--     ( Functor
--     , Applicative
--     , Monad
--     , MonadError ()
--     , MonadState Int
--     , Alternative
--     )

-- runMatcher :: MatcherMonad a -> Int -> (Either () a, Int)
-- runMatcher action s
--   = runIdentity
--   . flip runStateT s
--   . runExceptT
--   . _runMatcher
--   $ action

-- add5 :: MatcherMonad ()
-- add5 = modify (+5)

-- add1 :: MatcherMonad ()
-- add1 = modify (+1)

-- foodoo :: MatcherMonad ()
-- foodoo = asum $ backtrackOnError <$> [(add5 >> throwError ()), add1, add5]

-- backtrackOnError :: MatcherMonad a -> MatcherMonad a
-- backtrackOnError action = do
--   s <- get
--   action <|> (put s >> throwError ())

-- ifElse :: MonadError e m => m a -> (e -> m b) -> (a -> m b) -> m b
-- ifElse action ehandler ghandler = do
--   catchError (Right <$> action) (return . Left) >>= \case
--     Left e -> ehandler e
--     Right x -> ghandler x
