{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Flint.Analysis.Path.Matcher.Logic.Combinators where

import Flint.Prelude hiding (Const, until, Constraint, succ)

import Control.Monad.Logic.Class


insist :: MonadLogic m => Bool -> m ()
insist = bool bad good

good :: MonadLogic m => m ()
good = return ()

bad :: MonadLogic m => m a
bad = empty

-- | returns ([parsed or skipped stmts], result, [remaining stmts])
parseUntil
  :: MonadLogic m
  => (stmt -> m a)
  -> [stmt] -- remaining
  -> m (a, [stmt])
parseUntil _ [] = bad
parseUntil parseStmt (stmt:remaining)
  =   (,remaining) <$> parseStmt stmt
  <|> parseUntil parseStmt remaining

parseUntil_
  :: MonadLogic m
  => (stmt -> m a)
  -> [stmt]
  -> m a
parseUntil_ parseStmt = fmap fst . parseUntil parseStmt

-- | Next stmt must match, or this fails
parseNext
  :: MonadLogic m
  => (stmt -> m a)
  -> [stmt] -- remaining
  -> m (a, [stmt])
parseNext _ [] = bad
parseNext parseStmt (stmt:remaining) = (,remaining) <$> parseStmt stmt

-- stupid and useless?
parseEnd :: MonadLogic m => [stmt] -> m ()
parseEnd [] = good
parseEnd _ = bad

avoidUntil
  :: MonadLogic m
  => (b -> [stmt] -> m ()) -- avoid
  -> ([stmt] -> m (b, [stmt])) -- until, returns  (b, remainingstmts)
  -> [stmt]
  -> m (b, [stmt])
avoidUntil _ _ [] = bad
avoidUntil avoid until stmts = do
  (r, remaining) <- until stmts
  let parsedStmts = take (length stmts - length remaining) stmts
  lnot $ avoid r parsedStmts
  return (r, remaining)

anyOne
  :: MonadLogic m
  => [[stmt] -> m (b, [stmt])]
  -> [stmt]
  -> m (b, [stmt])
anyOne _ [] = bad
anyOne [] _ = bad
anyOne (p:ps) stmts = p stmts <|||> anyOne ps stmts

(<|||>) :: MonadLogic m => m a -> m a -> m a
(<|||>) = interleave

orr :: (Foldable t, MonadLogic m) => t (m a) -> m a
orr = foldr (<|||>) empty

choose :: (Foldable t, MonadLogic m) => t a -> m a
choose = foldr ((<|||>) . pure) empty

-- | Chooses a random element out and returns the rest of the unchosen elements
chooseAndReduce :: (MonadLogic m) => [a] -> m (a, [a])
chooseAndReduce = go []
  where
    go _ [] = empty
    go prev (x:xs) = pure (x, prev <> xs) <|||> go (x:prev) xs

-- | Sequentially matches parsers from a list of parsers
-- with a list of things to parse. If all parsers aren't used,
-- this fails.
parseThroughList
  :: MonadLogic m
  => [stmt -> m a]
  -> [stmt] -- remaining
  -> m ([a], [stmt])
parseThroughList [] xs = return ([], xs)
parseThroughList _parsers [] = bad
parseThroughList (p:ps) xs = do
  (a, xs') <- parseUntil p xs
  over _1 (a:) <$> parseThroughList ps xs'
