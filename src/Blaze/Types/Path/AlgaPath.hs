module Blaze.Types.Path.AlgaPath (AlgaPath) where

import Blaze.Prelude
import Blaze.Pretty (Pretty, pretty)
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

instance Pretty AlgaPath where
  pretty p = case uncons (Path.toList p) of
    Nothing -> ""
    Just (x, xs) ->
      "========= Starting in: " <> pretty (Path.getNodeFunc x) <> " =========\n"
        <> f (x : xs)
    where
      f [] = ""
      f (x : xs) = pretty x <> "\n" <> f xs
