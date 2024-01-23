module Blaze.Util.Analysis where

import Blaze.Prelude

untilFixedPoint :: Eq a => Maybe Text -> (a -> a) -> Int -> a -> a
untilFixedPoint mErrorMessage f itersLeft x
  | itersLeft <= 0 = maybe x (error . cs) mErrorMessage
  | x == x' = x
  | otherwise = untilFixedPoint mErrorMessage f (itersLeft - 1) x'
  where
    x' = f x
