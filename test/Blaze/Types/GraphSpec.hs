{- HLINT ignore "Redundant do" -}

module Blaze.Types.GraphSpec where

import Blaze.Prelude

import qualified Blaze.Types.Graph as G
import qualified Blaze.Types.Graph.Alga as GA
import Blaze.Types.Graph.Alga (AlgaGraph)
import qualified Data.HashSet as HashSet
import Test.Hspec

graphA :: GA.AlgaGraph () () Text
graphA =
  G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
    [ ("a", "b")
    , ("b", "c")
    , ("c", "b")
    , ("c", "z")
    , ("d", "e")
    , ("e", "z")
    ]

graphSingleNode :: GA.AlgaGraph () () Text
graphSingleNode = G.fromNode "a"

graphSingleEdge :: GA.AlgaGraph () () Text
graphSingleEdge =
  G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
    [("a", "b")]

graphSinglePath :: GA.AlgaGraph () () Text
graphSinglePath =
  G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
    [ ("a", "b")
    , ("b", "c")
    , ("c", "d")
    ]

graphWithSingleLoop :: GA.AlgaGraph () () Text
graphWithSingleLoop =
  G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
    [ ("z", "a")
    , ("a", "b")
    , ("a", "d")
    , ("b", "c")
    , ("b", "e")
    , ("e", "a")
    , ("d", "c")
    ]

graphWithLoops :: GA.AlgaGraph () () Text
graphWithLoops =
  G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
    [ ("z", "a") -- needed to find "source"
    , ("a", "b")
    , ("a", "c")
    , ("b", "f")
    , ("c", "d")
    , ("c", "e")
    , ("d", "f")
    , ("d", "b")
    , ("e", "a")
    , ("e", "f")
    ]

spec :: Spec
spec = describe "Blaze.Util.Graph" $ do
  context "Graph instance" $ do
    it "should remove a node" $ do
      let result = G.addNodes ["a"]
                   . G.fromEdges
                   . fmap (G.fromTupleLEdge . ((),))
                   $ [ ("c", "d") ]
      G.removeNode "b" graphSinglePath `shouldBe` result
  context "sources" $ do
    it "should find source nodes" $ do
      let srcs = G.sources graphA
      srcs `shouldBe` HashSet.fromList ["a", "d"]

  context "findSimplePaths" $ do
    it "should find simple paths" $ do
      let paths = HashSet.fromList $ G.findAllSimplePaths graphA
      paths
        `shouldBe` HashSet.fromList
          [ ["a", "b", "c", "z"]
          , ["d", "e", "z"]
          ]
    it "should find a simple path for a graph with one node" $ do
      let paths = HashSet.fromList $ G.findAllSimplePaths graphSingleNode
      paths `shouldBe` HashSet.fromList [["a"]]

  context "findNonRepeatPaths" $ do
    it "should find a non-repeat path for a graph with one node" $ do
      let paths = HashSet.fromList $ G.findAllNonRepeatPaths graphSingleNode
      paths `shouldBe` HashSet.fromList [["a"]]

    it "should find a non-repeat path for a graph with one node" $ do
      let paths = HashSet.fromList $ G.findAllNonRepeatPaths graphSingleEdge
      paths `shouldBe` HashSet.fromList [["a", "b"]]

    it "should find a non-repeat path for a graph with one node" $ do
      let paths = HashSet.fromList $ G.findAllNonRepeatPaths graphSinglePath
      paths `shouldBe` HashSet.fromList [["a", "b", "c", "d"]]

    it "should find non-repeat paths with single loop" $ do
      let paths = HashSet.fromList $ G.findAllNonRepeatPaths graphWithSingleLoop
      paths
        `shouldBe` HashSet.fromList
          [ ["z", "a", "b", "c"]
          , ["z", "a", "d", "c"]
          , ["z", "a", "b", "e"]
          ]

    it "should find non-repeat paths" $ do
      let paths = HashSet.fromList $ G.findAllNonRepeatPaths graphWithLoops
      paths
        `shouldBe` HashSet.fromList
          [ ["z", "a", "b", "f"]
          , ["z", "a", "c", "d", "f"]
          , ["z", "a", "c", "d", "b", "f"]
          , ["z", "a", "c", "e", "f"]
          ]

  context "connectedNodes" $ do
    it "should return nothing if graph is empty" $ do
      let g = G.empty :: AlgaGraph () () Text
      HashSet.empty `shouldBe` G.connectedNodes "this node doesn't exist" g 

    it "should return single node in singleton graph" $ do
      let g = G.fromNode "hey" :: AlgaGraph () () Text
          r = HashSet.fromList ["hey"]
      r `shouldBe` G.connectedNodes "hey" g 

    it "should return all nodes in a linear graph" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ (1, 2)
              , (2, 3)
              ]
          r = HashSet.fromList [1, 2, 3]
      r `shouldBe` G.connectedNodes 2 (g :: AlgaGraph () () Int)

    it "should ignore nodes in other connected components" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ (1, 2)
              , (2, 3)
              , (8, 9)
              , (9, 10)
              ]
          r = HashSet.fromList [1, 2, 3]
      r `shouldBe` G.connectedNodes 2 (g :: AlgaGraph () () Int)

    it "should handle a loop" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ (1, 2)
              , (2, 3)
              , (3, 1)
              ]
          r = HashSet.fromList [1, 2, 3]
      r `shouldBe` G.connectedNodes 2 (g :: AlgaGraph () () Int)

    it "should ignore nodes that cannot reach" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ (1, 2)
              , (2, 3)
              , (1, 8)
              , (8, 9)
              ]
          r = HashSet.fromList [1, 2, 3]
      r `shouldBe` G.connectedNodes 2 (g :: AlgaGraph () () Int)

    it "should return nodes that can only be reached through loop" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ (1, 2)
              , (2, 3)
              , (1, 8)
              , (8, 9)
              , (3, 1)
              ]
          r = HashSet.fromList [1, 2, 3, 8, 9]
      r `shouldBe` G.connectedNodes 2 (g :: AlgaGraph () () Int)

    it "should be reachable by nodes that must go through loop" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ (1, 2)
              , (2, 3)
              , (1, 8)
              , (8, 9)
              , (9, 1)
              ]
          r = HashSet.fromList [1, 2, 3, 8, 9]
      r `shouldBe` G.connectedNodes 2 (g :: AlgaGraph () () Int)

  context "mkBiDirectional" $ do
    it "should be identity for empty graph" $ do
      let g = G.empty :: AlgaGraph () () Text
      G.mkBiDirectional g  `shouldBe` G.empty

    it "should be identity for singleton graph" $ do
      let g = G.fromNode "hey" :: AlgaGraph () () Text
      G.mkBiDirectional g `shouldBe` g

    it "should make duplicate edge for single edge graph" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ (1, 2) ]
          r = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ (1, 2)
              , (2, 1)
              ]
      G.mkBiDirectional (g :: AlgaGraph () () Int) `shouldBe` r

    it "should leave label if backedge already exists" $ do
      let g = G.fromEdges . fmap G.fromTupleLEdge $
              [ ("hey", (1, 2))
              , ("there", (2, 1))
              ]
          r = g
      G.mkBiDirectional (g :: AlgaGraph Text () Int) `shouldBe` r

  context "getWeaklyConnectedComponents" $ do
    it "should return nothing if graph is empty" $ do
      let g = G.empty :: AlgaGraph () () Text
      G.getWeaklyConnectedComponents g  `shouldBe` [] 

    it "should return single node in singleton graph" $ do
      let g = G.fromNode "hey" :: AlgaGraph () () Text
          r = [HashSet.fromList ["hey"]]
      G.getWeaklyConnectedComponents g  `shouldBe` r

    it "should return two sets of nodes for two components" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ (1, 2)
              , (2, 3)
              , (6, 7)
              , (7, 8)
              ]
          r = [HashSet.fromList [1, 2, 3], HashSet.fromList [6, 7, 8]]
      sort (G.getWeaklyConnectedComponents (g :: AlgaGraph () () Int)) `shouldBe` sort r

  context "getDescendants" $ do
    it "should return an empty set for singleton graph" $ do
      let g = G.fromNode "a" :: AlgaGraph () () Text
          r = HashSet.empty
      G.getDescendants "a" g `shouldBe` r

    it "should return dependent, but not dependee" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ ("a", "b")
              , ("b", "c")
              ] :: AlgaGraph () () Text
          r = HashSet.fromList ["c"]
      G.getDescendants "b" g `shouldBe` r

    it "should return itself, if loop" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ ("a", "b")
              , ("b", "c")
              , ("c", "a")
              ] :: AlgaGraph () () Text
          r = HashSet.fromList ["a", "b", "c"]
      G.getDescendants "b" g `shouldBe` r

    it "should find branched dependents" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ ("a", "b")
              , ("b", "c")
              , ("b", "d")
              ] :: AlgaGraph () () Text
          r = HashSet.fromList ["b", "c", "d"]
      G.getDescendants "a" g `shouldBe` r

  context "getAncestors" $ do
    it "should return empty set for singleton graph" $ do
      let g = G.fromNode "a"  :: AlgaGraph () () Text
          r = HashSet.empty
      G.getAncestors "a" g `shouldBe` r

    it "should return dependee, but not dependent" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ ("a", "b")
              , ("b", "c")
              ] :: AlgaGraph () () Text
          r = HashSet.fromList ["a"]
      G.getAncestors "b" g `shouldBe` r

    it "should return itself, if loop" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ ("a", "b")
              , ("b", "c")
              , ("c", "a")
              ] :: AlgaGraph () () Text
          r = HashSet.fromList ["a", "b", "c"]
      G.getAncestors "b" g `shouldBe` r

    it "should find branched dependees" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ ("a", "b")
              , ("d", "b")
              , ("b", "c")
              ] :: AlgaGraph () () Text
          r = HashSet.fromList ["b", "a", "d"]
      G.getAncestors "c" g `shouldBe` r
