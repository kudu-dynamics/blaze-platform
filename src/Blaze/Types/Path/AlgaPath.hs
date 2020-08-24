module Blaze.Types.Path.AlgaPath (AlgaPath (AlgaPath)) where

import Blaze.Prelude
import Blaze.Types.Graph (Graph)
import Blaze.Types.Graph.Alga (AlgaGraph)
import Blaze.Types.Path
import qualified Blaze.Types.Path as Path
import qualified Prelude as P

newtype AlgaPath = AlgaPath (PathGraph (AlgaGraph () Node))
  deriving (Graph () Node, Path, Ord)

-- converts to list because for some reason two identical graphs aren't equal
-- TODO: this is bad, because toList is slow for AlgaPath
instance Eq AlgaPath where
  (==) a b = Path.toList a == Path.toList b

instance Show AlgaPath where
  show = show . Path.toList