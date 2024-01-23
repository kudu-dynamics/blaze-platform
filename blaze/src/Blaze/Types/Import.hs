module Blaze.Types.Import
  ( ImportResult(ImportResult)
  ) where

import Blaze.Prelude

import Blaze.Types.Pil (Ctx)

data ImportResult b a = ImportResult
  { ctx :: Ctx
  , mapping :: b
  , result :: a
  }
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
