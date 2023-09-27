module Flint.Prelude
  ( module Exports
  , hoistMaybeM
  , tryError
  ) where

import Blaze.Prelude as Exports

import Control.Monad.Extra as Exports (mapMaybeM)
import System.Random as Exports (randomRIO)
import Control.Concurrent.Async as Exports (replicateConcurrently)


hoistMaybeM :: Monad m => m (Maybe a) -> MaybeT m a
hoistMaybeM m = lift m >>= maybe mzero return

-- | 'MonadError' analogue to the 'Control.Exception.try' function.
tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)

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
