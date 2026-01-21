module Blaze.Types.Import where

import Blaze.Prelude
import Blaze.Types.Pil (Ctx)
import Blaze.Types.Pil.PilType
import Blaze.Types.Pil.Common (PilVar)

data ImportResult b a = ImportResult
  { ctx :: Ctx
  , mapping :: b
  , result :: a
  }
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)


data ImpType
    = Unknown (Maybe Bits) Text -- for debugging purposes, if we get a datatype that we haven't implemented yet, then we can read it here
    | ImpType (PilType ImpType)
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToJSONKey, FromJSONKey, Hashable)

type TypeHints = HashMap PilVar ImpType