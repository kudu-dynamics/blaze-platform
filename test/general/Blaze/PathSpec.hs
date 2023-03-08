{- HLINT ignore "Redundant do" -}

module Blaze.PathSpec where

import Blaze.Prelude

import qualified Blaze.Graph as G
import Blaze.Types.Graph (NodeId(NodeId), DescendantsMap)
import Blaze.Types.Graph.Alga (AlgaGraph)
import Blaze.Path (getAllPaths, getPathsContaining_, getPathsContainingAndAvoiding_)
import Blaze.Types.Path (start, (-|), (|-), PathBuilder)
import Blaze.Types.Path.Alga (AlgaPath(AlgaPath, rootNode, graph))

import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import Test.Hspec

type TextGraph = AlgaGraph Int Text Text
type TextPath = AlgaPath Int Text Text
type TextDescendantsMap = DescendantsMap Text

graphEmpty :: TextGraph
graphEmpty = G.empty

graphEmptyDmap :: TextDescendantsMap
graphEmptyDmap = G.calcDescendantsMap graphEmpty

graphSingleNode :: TextGraph
graphSingleNode = G.fromNode "a"

graphSingleNodeDmap :: TextDescendantsMap
graphSingleNodeDmap = G.calcDescendantsMap graphSingleNode

pathSingleNode :: TextPath
pathSingleNode = AlgaPath
  { rootNode = NodeId "a"
  , graph = graphSingleNode
  }

graphSingleEdge :: TextGraph
graphSingleEdge =
  G.fromEdges . fmap G.fromTupleLEdge $
    [(1, ("a", "b"))]

graphSingleEdgeDmap :: TextDescendantsMap
graphSingleEdgeDmap = G.calcDescendantsMap graphSingleEdge

graphSinglePath :: TextGraph
graphSinglePath =
  G.fromEdges . fmap G.fromTupleLEdge $
    [ (1, ("a", "b"))
    , (2, ("b", "c"))
    , (3, ("c", "d"))
    ]

graphSinglePathDmap :: TextDescendantsMap
graphSinglePathDmap = G.calcDescendantsMap graphSinglePath

pathManyNodes :: TextPath
pathManyNodes = AlgaPath
  { rootNode = NodeId "a"
  , graph = graphSinglePath
  }

mkTextPath :: PathBuilder Int Text -> TextPath
mkTextPath pb = AlgaPath
  { rootNode = NodeId startNode_
  , graph = if null (pb ^. #halfEdges)
            then G.fromNode startNode_
            else G.fromEdges edges_
  }
  where
    (startNode_, edges_) = mkLEdges (pb ^. #halfEdges) (pb ^. #endNode) []
    mkLEdges [] n acc = (n, acc)
    mkLEdges ((a, lbl):es) b acc = mkLEdges es a $ G.LEdge lbl (G.Edge a b) : acc

graphTwoPaths :: TextGraph
graphTwoPaths =
  G.fromEdges . fmap G.fromTupleLEdge $
    [ (1, ("a", "b"))
    , (2, ("b", "fin"))
    , (3, ("a", "c"))
    , (4, ("c", "fin"))
    ]

graphTwoPathsDmap :: TextDescendantsMap
graphTwoPathsDmap = G.calcDescendantsMap graphTwoPaths

graphMultiPath :: TextGraph
graphMultiPath =
  G.fromEdges . fmap G.fromTupleLEdge $
    [ (1, ("a", "b"))
    , (2, ("b", "c"))
    , (3, ("c", "d"))
    , (4, ("d", "fin"))

    , (5, ("b", "e"))
    , (6, ("e", "fin"))

    , (7, ("c", "fin"))
    ]

graphMultiPathDmap :: TextDescendantsMap
graphMultiPathDmap = G.calcDescendantsMap graphMultiPath

graphWithSingleLoop :: TextGraph
graphWithSingleLoop =
  G.fromEdges . fmap G.fromTupleLEdge $
    [ (1, ("a", "b"))
    , (2, ("b", "c"))
    , (3, ("c", "d"))
    , (4, ("d", "b"))
    ]

-- | This graph confused an initial implementation of getAllPaths.
graphWithDiamondLoop :: TextGraph
graphWithDiamondLoop =
  G.fromEdges . fmap G.fromTupleLEdge $
    [ (1, ("a", "b"))
    , (2, ("a", "c"))
    , (3, ("b", "d"))
    , (4, ("c", "d"))
    
    , (5, ("d", "b"))
    , (6, ("d", "c"))
    ]


graphWithSingleLoopDmap :: TextDescendantsMap
graphWithSingleLoopDmap = G.calcDescendantsMap graphWithSingleLoop

graphWithSingleLoopAndExit :: TextGraph
graphWithSingleLoopAndExit =
  G.fromEdges . fmap G.fromTupleLEdge $
    [ (1, ("a", "b"))
    , (2, ("b", "c"))
    , (3, ("c", "d"))
    , (4, ("d", "b"))

    , (5, ("c", "e"))
    , (6, ("e", "fin"))
    ]

graphWithSingleLoopAndExitDmap :: TextDescendantsMap
graphWithSingleLoopAndExitDmap = G.calcDescendantsMap graphWithSingleLoopAndExit


spec :: Spec
spec = describe "Blaze.Path" $ do

  let noRevisit _ _ = error "Should not revisit"
      replicateOnRevisit = Text.replicate

  context "getAllPaths" $ do
    
    it "should find no paths if graph is empty" $ do
      let graph = G.empty :: TextGraph
          result = getAllPaths noRevisit 0 "a" graph
          expected = []
      result `shouldBe` (expected :: [TextPath])

    it "should find a singleton path if graph has a single node" $ do
      let graph = graphSingleNode
          result = getAllPaths noRevisit 0 "a" graph
          expected = [pathSingleNode]
      result `shouldBe` expected

    it "should find path with single edge if graph has a single edge" $ do
      let graph = graphSingleEdge
          result = getAllPaths noRevisit 0 "a" graph
          expected = [mkTextPath $ start "a" -| 1 |- "b"]
      result `shouldBe` expected

    it "should find a many-node path if graph has a many-node path" $ do
      let graph = graphSinglePath
          result = getAllPaths noRevisit 0 "a" graph
          expected = [pathManyNodes]
      result `shouldBe` expected

    it "should find two paths if graph has two paths" $ do
      let graph = graphTwoPaths
          result = getAllPaths noRevisit 0 "a" graph
          expected = 
            [ mkTextPath $ start "a" -| 1 |- "b" -| 2 |- "fin"
            , mkTextPath $ start "a" -| 3 |- "c" -| 4 |- "fin"
            ]
      sort result `shouldBe` sort expected

    it "should find multiple paths of varying lengths" $ do
      let graph = graphMultiPath
          result = getAllPaths noRevisit 0 "a" graph
          expected =
            [ mkTextPath $ start "a" -| 1 |- "b" -| 2 |- "c" -| 3 |- "d" -| 4 |- "fin"
            , mkTextPath $ start "a" -| 1 |- "b" -| 2 |- "c" -| 7 |- "fin"
            , mkTextPath $ start "a" -| 1 |- "b" -| 5 |- "e" -| 6 |- "fin"
            ]
      sort result `shouldBe` sort expected

    it "should find not revisit loop node if revisitLimit is zero" $ do
      let graph = graphWithSingleLoop
          result = getAllPaths noRevisit 0 "a" graph
          expected =
            [ mkTextPath $ start "a" -| 1 |- "b" -| 2 |- "c" -| 3 |- "d"
            ]
      result `shouldBe` expected

    it "should not revisit loop node if revisitLimit is zero" $ do
      let graph = graphWithSingleLoop
          result = getAllPaths noRevisit 0 "a" graph
          expected =
            [ mkTextPath $ start "a" -| 1 |- "b" -| 2 |- "c" -| 3 |- "d"
            ]
      result `shouldBe` expected

    it "should revisit loop nodes once if revisitLimit is 1" $ do
      let graph = graphWithSingleLoop
          result = getAllPaths replicateOnRevisit 1 "a" graph
          expected =
            [ mkTextPath $ start "a" -| 1 |- "b" -| 2 |- "c" -| 3 |- "d"
              -| 4 |- "bb" -| 2 |- "cc" -| 3 |- "dd"
            ]
      result `shouldBe` expected

    it "should revisit loop nodes twice if revisitLimit is 2" $ do
      let graph = graphWithSingleLoop
          result = getAllPaths replicateOnRevisit 2 "a" graph
          expected =
            [ mkTextPath $ start "a" -| 1 |- "b" -| 2 |- "c" -| 3 |- "d"
              -| 4 |- "bb" -| 2 |- "cc" -| 3 |- "dd"
              -| 4 |- "bbb" -| 2 |- "ccc" -| 3 |- "ddd"
            ]
      result `shouldBe` expected

    it "should revisit loop nodes thrice if revisitLimit is 3" $ do
      let graph = graphWithSingleLoop
          result = getAllPaths replicateOnRevisit 3 "a" graph
          expected =
            [ mkTextPath $ start "a" -| 1 |- "b" -| 2 |- "c" -| 3 |- "d"
              -| 4 |- "bb" -| 2 |- "cc" -| 3 |- "dd"
              -| 4 |- "bbb" -| 2 |- "ccc" -| 3 |- "ddd"
              -| 4 |- "bbbb" -| 2 |- "cccc" -| 3 |- "dddd"
            ]
      result `shouldBe` expected

    it "should only find two paths in diamond loop when revisit limit is 0" $ do
      let graph = graphWithDiamondLoop
          result = getAllPaths replicateOnRevisit 0 "a" graph
          expected =
            [ mkTextPath $ start "a" -| 1 |- "b" -| 3 |- "d" -| 6 |- "c"
            , mkTextPath $ start "a" -| 2 |- "c" -| 4 |- "d" -| 5 |- "b"
            ]
      sort result `shouldBe` sort expected


  context "getPathsContaining_" $ do
    
    it "should find no paths if graph is empty" $ do
      let graph = G.empty :: TextGraph
          requiredNodes = HashSet.empty
          dmap = graphEmptyDmap
          result = getPathsContaining_ dmap noRevisit 0 "a" graph requiredNodes
          expected = []
      result `shouldBe` (expected :: [TextPath])

    it "should find a singleton path if graph has single node and node is in requiredNodes" $ do
      let graph = graphSingleNode
          requiredNodes = HashSet.singleton "a"
          dmap = graphSingleNodeDmap
          result = getPathsContaining_ dmap noRevisit 0 "a" graph requiredNodes
          expected = [pathSingleNode]
      result `shouldBe` (expected :: [TextPath])

    it "should find no paths if graph has single node requiredNodes has different node" $ do
      let graph = graphSingleNode
          requiredNodes = HashSet.fromList ["oops"]
          dmap = graphSingleNodeDmap
          result = getPathsContaining_ dmap noRevisit 0 "a" graph requiredNodes
          expected = []
      result `shouldBe` (expected :: [TextPath])

    it "should return a path if only the first node is required" $ do
      let graph = graphSinglePath
          requiredNodes = HashSet.singleton "a"
          dmap = graphSinglePathDmap
          result = getPathsContaining_ dmap noRevisit 0 "a" graph requiredNodes
          expected = [pathManyNodes]
      result `shouldBe` (expected :: [TextPath])

    it "should return a path if only a middle node is required" $ do
      let graph = graphSinglePath
          requiredNodes = HashSet.singleton "c"
          dmap = graphSinglePathDmap
          result = getPathsContaining_ dmap noRevisit 0 "a" graph requiredNodes
          expected = [pathManyNodes]
      result `shouldBe` (expected :: [TextPath])

    it "should not return path if it does not contain all required nodes" $ do
      let graph = graphSinglePath
          requiredNodes = HashSet.fromList ["b", "haha"]
          dmap = graphSinglePathDmap
          result = getPathsContaining_ dmap noRevisit 0 "a" graph requiredNodes
          expected = []
      result `shouldBe` (expected :: [TextPath])

    it "should return only paths that contain all required nodes in graph with multiple paths" $ do
      let graph = graphMultiPath
          requiredNodes = HashSet.fromList ["e"]
          dmap = graphMultiPathDmap
          result = getPathsContaining_ dmap noRevisit 0 "a" graph requiredNodes
          expected = [ mkTextPath $ start "a" -| 1 |- "b" -| 5 |- "e" -| 6 |- "fin" ]
      result `shouldBe` (expected :: [TextPath])

    it "should return direct path to required node and not loop if revisit limit is 0" $ do
      let graph = graphWithSingleLoopAndExit
          dmap = graphWithSingleLoopAndExitDmap
          requiredNodes = HashSet.fromList ["e"]
          result = getPathsContaining_ dmap noRevisit 0 "a" graph requiredNodes
          expected = [ mkTextPath $ start "a" -| 1 |- "b" -| 2 |- "c" -| 5 |- "e" -| 6 |- "fin" ]
      result `shouldBe` (expected :: [TextPath])

    it "should go through loop to reach to required node if revisit limit is 1" $ do
      let graph = graphWithSingleLoopAndExit
          dmap = graphWithSingleLoopAndExitDmap
          requiredNodes = HashSet.fromList ["e"]
          result = getPathsContaining_ dmap replicateOnRevisit 1 "a" graph requiredNodes
          expected =
            [ mkTextPath $ start "a" -| 1 |- "b" -| 2 |- "c" -| 5 |- "e" -| 6 |- "fin"
            , mkTextPath $ start "a" -| 1 |- "b" -| 2 |- "c" -| 3 |- "d" -| 4 |-
              "bb" -| 2 |- "cc" -| 5 |- "e" -| 6 |- "fin"
            ]
      result `shouldBe` (expected :: [TextPath])

  context "getPathsContainingAndAvoiding_" $ do

    it "should return only paths that contain req nodes and do not contain avoid nodess" $ do
      let graph = graphMultiPath
          requiredNodes = HashSet.fromList ["c"]
          avoidNodes = HashSet.fromList["d"]
          dmap = graphMultiPathDmap
          result = getPathsContainingAndAvoiding_ dmap noRevisit 0 "a" graph requiredNodes avoidNodes
          expected = [ mkTextPath $ start "a" -| 1 |- "b" -| 2 |- "c" -| 7 |- "fin" ]
      result `shouldBe` (expected :: [TextPath])

