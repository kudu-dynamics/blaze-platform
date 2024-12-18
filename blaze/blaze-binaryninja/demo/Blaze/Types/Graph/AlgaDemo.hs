module Blaze.Types.Graph.AlgaDemo where

import Blaze.Prelude
import Blaze.Types.Graph
import Blaze.Types.Graph.Alga

toEdge :: (a, a) -> Edge a
toEdge (a, b) = Edge a b

demograph :: AlgaGraph () Char Char
demograph = fromEdges
  . fmap (LEdge () . toEdge)
  $ [ ('z', 'a')
    , ('a', 'b')
    , ('a', 'c')
    , ('d', 'c')
    , ('b', 'g')
    , ('b', 'f')
    , ('c', 'e')
    ]

demograph2 :: AlgaGraph () Int Int
demograph2 = fromEdges
  . fmap (LEdge () . toEdge)
  $ [ (0, 3), (3, 0)
    , (0, 1), (1, 0)
    , (1, 8), (8, 1)
    , (99, 88), (88, 99)
    ]
