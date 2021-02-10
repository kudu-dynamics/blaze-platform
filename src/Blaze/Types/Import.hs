module Blaze.Types.Import where

import Blaze.Prelude

data ImportResult a b = ImportResult
  { result :: a
  , mapping :: b
  }
  deriving (Eq, Ord, Show, Generic)