{- HLINT ignore "Redundant do" -}

module Blaze.Types.GraphSpec where

import Blaze.Prelude

import qualified Blaze.Graph as G
import Blaze.Types.Graph.Alga (AlgaGraph)
import qualified Data.HashSet as HashSet
import Test.Hspec

type TextGraph = AlgaGraph () Text Text
type IntGraph = AlgaGraph () Int Int
type LabeledIntGraph = AlgaGraph Text Int Int

graphA :: TextGraph
graphA =
  G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
    [ ("a", "b")
    , ("b", "c")
    , ("c", "b")
    , ("c", "z")
    , ("d", "e")
    , ("e", "z")
    ]

graphSingleNode :: TextGraph
graphSingleNode = G.fromNode "a"

graphSingleEdge :: TextGraph
graphSingleEdge =
  G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
    [("a", "b")]

graphSinglePath :: TextGraph
graphSinglePath =
  G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
    [ ("a", "b")
    , ("b", "c")
    , ("c", "d")
    ]

graphWithSingleLoop :: TextGraph
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

graphWithLoops :: TextGraph
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
      let g = G.empty :: TextGraph
      HashSet.empty `shouldBe` G.connectedNodes "this node doesn't exist" g

    it "should return single node in singleton graph" $ do
      let g = G.fromNode "hey" :: TextGraph
          r = HashSet.fromList ["hey"]
      r `shouldBe` G.connectedNodes "hey" g

    it "should return all nodes in a linear graph" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ (1, 2)
              , (2, 3)
              ]
          r = HashSet.fromList [1, 2, 3]
      r `shouldBe` G.connectedNodes 2 (g :: IntGraph)

    it "should ignore nodes in other connected components" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ (1, 2)
              , (2, 3)
              , (8, 9)
              , (9, 10)
              ]
          r = HashSet.fromList [1, 2, 3]
      r `shouldBe` G.connectedNodes 2 (g :: IntGraph)

    it "should handle a loop" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ (1, 2)
              , (2, 3)
              , (3, 1)
              ]
          r = HashSet.fromList [1, 2, 3]
      r `shouldBe` G.connectedNodes 2 (g :: IntGraph)

    it "should ignore nodes that cannot reach" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ (1, 2)
              , (2, 3)
              , (1, 8)
              , (8, 9)
              ]
          r = HashSet.fromList [1, 2, 3]
      r `shouldBe` G.connectedNodes 2 (g :: IntGraph)

    it "should return nodes that can only be reached through loop" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ (1, 2)
              , (2, 3)
              , (1, 8)
              , (8, 9)
              , (3, 1)
              ]
          r = HashSet.fromList [1, 2, 3, 8, 9]
      r `shouldBe` G.connectedNodes 2 (g :: IntGraph)

    it "should be reachable by nodes that must go through loop" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ (1, 2)
              , (2, 3)
              , (1, 8)
              , (8, 9)
              , (9, 1)
              ]
          r = HashSet.fromList [1, 2, 3, 8, 9]
      r `shouldBe` G.connectedNodes 2 (g :: IntGraph)

  context "mkBiDirectional" $ do
    it "should be identity for empty graph" $ do
      let g = G.empty :: TextGraph
      G.mkBiDirectional g  `shouldBe` G.empty

    it "should be identity for singleton graph" $ do
      let g = G.fromNode "hey" :: TextGraph
      G.mkBiDirectional g `shouldBe` g

    it "should make duplicate edge for single edge graph" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ (1, 2) ]
          r = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ (1, 2)
              , (2, 1)
              ]
      G.mkBiDirectional (g :: IntGraph) `shouldBe` r

    it "should leave label if backedge already exists" $ do
      let g = G.fromEdges . fmap G.fromTupleLEdge $
              [ ("hey", (1, 2))
              , ("there", (2, 1))
              ]
          r = g
      G.mkBiDirectional (g :: LabeledIntGraph) `shouldBe` r

  context "getWeaklyConnectedComponents" $ do
    it "should return nothing if graph is empty" $ do
      let g = G.empty :: TextGraph
      G.getWeaklyConnectedComponents g  `shouldBe` []

    it "should return single node in singleton graph" $ do
      let g = G.fromNode "hey" :: TextGraph
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
      sort (G.getWeaklyConnectedComponents (g :: IntGraph)) `shouldBe` sort r

  context "getDescendants" $ do
    it "should return an empty set for singleton graph" $ do
      let g = G.fromNode "a" :: TextGraph
          r = HashSet.empty
      G.getDescendants "a" g `shouldBe` r

    it "should return dependent, but not dependee" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ ("a", "b")
              , ("b", "c")
              ] :: TextGraph
          r = HashSet.fromList ["c"]
      G.getDescendants "b" g `shouldBe` r

    it "should return itself, if loop" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ ("a", "b")
              , ("b", "c")
              , ("c", "a")
              ] :: TextGraph
          r = HashSet.fromList ["a", "b", "c"]
      G.getDescendants "b" g `shouldBe` r

    it "should find branched dependents" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ ("a", "b")
              , ("b", "c")
              , ("b", "d")
              ] :: TextGraph
          r = HashSet.fromList ["b", "c", "d"]
      G.getDescendants "a" g `shouldBe` r

  context "getAncestors" $ do
    it "should return empty set for singleton graph" $ do
      let g = G.fromNode "a"  :: TextGraph
          r = HashSet.empty
      G.getAncestors "a" g `shouldBe` r

    it "should return dependee, but not dependent" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ ("a", "b")
              , ("b", "c")
              ] :: TextGraph
          r = HashSet.fromList ["a"]
      G.getAncestors "b" g `shouldBe` r

    it "should return itself, if loop" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ ("a", "b")
              , ("b", "c")
              , ("c", "a")
              ] :: TextGraph
          r = HashSet.fromList ["a", "b", "c"]
      G.getAncestors "b" g `shouldBe` r

    it "should find branched dependees" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ ("a", "b")
              , ("d", "b")
              , ("b", "c")
              ] :: TextGraph
          r = HashSet.fromList ["b", "a", "d"]
      G.getAncestors "c" g `shouldBe` r

  context "getTermNodes" $ do
    it "should get root as term for single node graph" $ do
      let g = G.fromNode "a"  :: TextGraph
          r = HashSet.singleton "a"
      G.getTermNodes g `shouldBe` r

    it "should get single term node from simple graph" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ ("a", "b")
              , ("b", "c")
              ] :: TextGraph
          r = HashSet.fromList ["c"]
      G.getTermNodes g `shouldBe` r

    it "should get two term nodes from graph" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ ("a", "b")
              , ("b", "c")
              , ("b", "d")
              ] :: TextGraph
          r = HashSet.fromList ["c", "d"]
      G.getTermNodes g `shouldBe` r

    it "should get self-looping root node as term node" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ ("a", "a")
              ] :: TextGraph
          r = HashSet.fromList ["a"]
      G.getTermNodes g `shouldBe` r

    it "should get self-looping term node" $ do
      let g = G.fromEdges . fmap (G.fromTupleLEdge . ((),)) $
              [ ("a", "b")
              , ("b", "c")
              , ("c", "c")
              ] :: TextGraph
          r = HashSet.fromList ["c"]
      G.getTermNodes g `shouldBe` r
