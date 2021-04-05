module Blaze.Types.Import
  ( ImportResult(ImportResult)
  ) where

import Blaze.Prelude

import Blaze.Types.Pil (Ctx)

data ImportResult a b = ImportResult
  { ctx :: Ctx
  , result :: a
  , mapping :: b
  }
  deriving (Eq, Ord, Show, Generic)
