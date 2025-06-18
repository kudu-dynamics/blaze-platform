module Blaze.Types.Cfg.Analysis where

import Blaze.Prelude
import Blaze.Types.Graph (DescendantsDistanceMap)
import Blaze.Types.Function (Func)
import Blaze.CallGraph (CallGraph)


data PathSearchStats = PathSearchStats
  { numPaths :: Int
  , shortestPathLength :: Int
  , medianPathLength :: Int
  } deriving (Eq, Ord, Show, Generic, Hashable)

-- TODO: add something for reachable through indirect calls
data CallNodeRating
  = Unreachable
  | Reachable { score :: Double
              -- , pathSearchStats :: Maybe PathSearchStats
              }
  deriving (Eq, Ord, Show, Generic, Hashable, ToJSON, FromJSON)

data CallNodeRatingCtx = CallNodeRatingCtx
  { callGraph :: !CallGraph
  , descendantsDistanceMap :: !(DescendantsDistanceMap Func)
  } deriving (Eq, Ord, Show, Generic)

data Target = Target
  { function :: Func
  , address :: Address
  } deriving (Eq, Ord, Show, Generic, Hashable)
