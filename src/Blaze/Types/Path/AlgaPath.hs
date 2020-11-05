module Blaze.Types.Path.AlgaPath
  ( AlgaPath (AlgaPath)
  , empty
  ) where

import Blaze.Prelude hiding (empty)
import Blaze.Types.Graph (Graph)
import qualified Blaze.Types.Graph as G
import Blaze.Types.Graph.Alga (AlgaGraph)
import Blaze.Types.Path
import qualified Blaze.Types.Path as Path
import qualified Prelude as P

newtype AlgaPath = AlgaPath (PathGraph (AlgaGraph () Node))
  deriving Ord
  deriving newtype (Graph () Node, Path)

-- converts to list because for some reason two identical graphs aren't equal
-- TODO: this is bad, because toList is slow for AlgaPath
instance Eq AlgaPath where
  (==) a b = Path.toList a == Path.toList b

instance Show AlgaPath where
  show = show . Path.toList

empty :: AlgaPath
empty = AlgaPath G.empty
