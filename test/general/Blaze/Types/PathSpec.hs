{- HLINT ignore "Redundant do" -}

module Blaze.Types.PathSpec where

import Blaze.Prelude

import qualified Blaze.Graph as G
import Blaze.Types.Graph (NodeId(NodeId))
import Blaze.Types.Graph.Alga (AlgaGraph)
import qualified Blaze.Types.Path as P
import Blaze.Types.Path (start, (-|), (|-), PathBuilder)
import Blaze.Types.Path.Alga (AlgaPath(AlgaPath, rootNode, graph))

import qualified Data.HashSet as HashSet
import qualified Data.List.NonEmpty as NE
import Test.Hspec

type TextGraph = AlgaGraph Int Text Text
type TextPath = AlgaPath Int Text Text

graphEmpty :: TextGraph
graphEmpty = G.empty

graphSingleNode :: TextGraph
graphSingleNode = G.fromNode "a"

pathSingleNode :: TextPath
pathSingleNode = AlgaPath
  { rootNode = NodeId "a"
  , graph = graphSingleNode
  }

graphSingleEdge :: TextGraph
graphSingleEdge =
  G.fromEdges . fmap G.fromTupleLEdge $
    [(1, ("a", "b"))]

graphSinglePath :: TextGraph
graphSinglePath =
  G.fromEdges . fmap G.fromTupleLEdge $
    [ (1, ("a", "b"))
    , (2, ("b", "c"))
    , (3, ("c", "d"))
    ]

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

graphMultiPath :: TextGraph
graphMultiPath =
  G.fromEdges . fmap G.fromTupleLEdge $
    [ (1, ("a", "b"))
    , (2, ("b", "c"))
    , (3, ("c", "d"))
    , (4, ("d", "fin"))

    , (5, ("b", "e"))
    , (6, ("e", "fin"))
    ]

graphWithSingleLoop :: TextGraph
graphWithSingleLoop =
  G.fromEdges . fmap G.fromTupleLEdge $
    [ (1, ("a", "b"))
    , (2, ("b", "c"))
    , (3, ("c", "d"))
    , (4, ("d", "b"))
    ]

spec :: Spec
spec = describe "Blaze.Types.Path" $ do
  context "internal test module" $ do
    context "mkTextPath" $ do
      it "should create singleton text path" $ do
        let result = mkTextPath $ start "a"
            expected = pathSingleNode
        result `shouldBe` expected

      it "should create text path with many nodes" $ do
        let result = mkTextPath $ start "a" -| 1 |- "b" -| 2 |- "c" -| 3 |- "d"
            expected = pathManyNodes
        result `shouldBe` expected
    
  context "Path instance" $ do
    context "root" $ do
      it "should return the root node" $ do
        let path = pathManyNodes
            result = P.root path
            expected = "a"
        result `shouldBe` expected

    context "end" $ do
      it "should return the end node for a singleton path" $ do
        let path = pathSingleNode
            result = P.end path
            expected = "a"
        result `shouldBe` expected

      it "should return the end node for a path with many nodes" $ do
        let path = pathManyNodes
            result = P.end path
            expected = "d"
        result `shouldBe` expected

    context "succ" $ do
      it "should return the successor of a non-end node" $ do
        let path = pathManyNodes
            result = P.succ "b" path
            expected = Just "c"
        result `shouldBe` expected

      it "should fail to return the successor of an end node" $ do
        let path = pathManyNodes
            result = P.succ "d" path
            expected = Nothing
        result `shouldBe` expected

      it "should fail to return the successor of a node that does not exist in the path" $ do
        let path = pathManyNodes
            result = P.succ "z" path
            expected = Nothing
        result `shouldBe` expected

    context "pred" $ do
      it "should return the predecessor of a non-root node" $ do
        let path = pathManyNodes
            result = P.pred "d" path
            expected = Just "c"
        result `shouldBe` expected

      it "should fail to return the predecessor of the root node" $ do
        let path = pathManyNodes
            result = P.pred "a" path
            expected = Nothing
        result `shouldBe` expected

      it "should fail to return the predecessor of a node that does not exist in the path" $ do
        let path = pathManyNodes
            result = P.pred "z" path
            expected = Nothing
        result `shouldBe` expected

    context "succEdge" $ do
      it "should return the successor edge of a non-end node" $ do
        let path = pathManyNodes
            result = P.succEdge "b" path
            expected = Just . G.LEdge 2 $ G.Edge "b" "c"
        result `shouldBe` expected

      it "should fail to return the successor edge of an end node" $ do
        let path = pathManyNodes
            result = P.succEdge "d" path
            expected = Nothing
        result `shouldBe` expected

      it "should fail to return the successor edge of a node that does not exist in the path" $ do
        let path = pathManyNodes
            result = P.succEdge "z" path
            expected = Nothing
        result `shouldBe` expected

    context "predEdge" $ do
      it "should return the predecessor edge of a non-root node" $ do
        let path = pathManyNodes
            result = P.predEdge "d" path
            expected = Just . G.LEdge 3 $ G.Edge "c" "d"
        result `shouldBe` expected

      it "should fail to return the predecessor of the root node" $ do
        let path = pathManyNodes
            result = P.predEdge "a" path
            expected = Nothing
        result `shouldBe` expected

      it "should fail to return the predecessor of a node that does not exist in the path" $ do
        let path = pathManyNodes
            result = P.predEdge "z" path
            expected = Nothing
        result `shouldBe` expected

    context "nodes" $ do
      it "should return a node set containing single node for singleton graph" $ do
        let path = pathSingleNode
            result = P.nodes path
            expected = HashSet.singleton "a"
        result `shouldBe` expected

      it "should return a node set containing all nodes of a path with many nodes" $ do
        let path = pathManyNodes
            result = P.nodes path
            expected = HashSet.fromList ["a", "b", "c", "d"]
        result `shouldBe` expected

    context "fromPathGraph" $ do
      it "should create a singleton path with root node if graph is empty" $ do
        let result = P.fromPathGraph "a" graphEmpty
            expected = Nothing :: Maybe TextPath
        result `shouldBe` expected

      it "should create path from singleton path graph" $ do
        let result = P.fromPathGraph "a" (G.fromNode "a" :: TextGraph)
            expected = Just pathSingleNode
        result `shouldBe` expected

      it "should create path from a path graph with single path" $ do
        let result = P.fromPathGraph "a" graphSinglePath
            expected = Just pathManyNodes
        result `shouldBe` (expected :: Maybe TextPath)

      it "should fail to create path from a path graph with many paths" $ do
        let result = P.fromPathGraph "a" graphMultiPath
            expected = Nothing
        result `shouldBe` (expected :: Maybe TextPath)

      it "should fail to create path from a path graph where the specified root node has preds" $ do
        let result = P.fromPathGraph "b" graphMultiPath
            expected = Nothing
        result `shouldBe` (expected :: Maybe TextPath)

      it "should fail to create path from a graph that loops" $ do
        let result = P.fromPathGraph "a" graphWithSingleLoop
            expected = Nothing
        result `shouldBe` (expected :: Maybe TextPath)


    context "toPathGraph" $ do
      it "should create path graph from a path" $ do
        let path = pathManyNodes
            result = P.toPathGraph path
            expected = ("a", graphSinglePath)
        result `shouldBe` expected

    context "fromEdges" $ do
      it "should create path from empty edge list" $ do
        let result = P.fromEdges "a" []
            expected = Just pathSingleNode
        result `shouldBe` (expected :: Maybe TextPath)

      it "should create path from linear edge list" $ do
        let result = P.fromEdges "a" . G.edges $ graphSinglePath
            expected = Just pathManyNodes
        result `shouldBe` (expected :: Maybe TextPath)

      it "should fail to create path from non linear edge list" $ do
        let result = P.fromEdges "a" . G.edges $ graphMultiPath
            expected = Nothing
        result `shouldBe` (expected :: Maybe TextPath)

    context "toNodeList" $ do
      it "should create node list from singleton path" $ do
        let path = pathSingleNode
            result = P.toNodeList path
            expected = NE.fromList ["a"]
        result `shouldBe` (expected :: NonEmpty Text)

      it "should create node list from path with many nodes" $ do
        let path = pathManyNodes
            result = P.toNodeList path
            expected = NE.fromList ["a", "b", "c", "d"]
        result `shouldBe` (expected :: NonEmpty Text)

    context "toEdgeList" $ do
      it "should create edge list from singleton path" $ do
        let path = pathSingleNode
            result = P.toEdgeList path
            expected = ("a", [])
        result `shouldBe` expected

      it "should create edge list from path with many nodes" $ do
        let path = pathManyNodes
            result = P.toEdgeList path
            expected = ("a"
                       , [ G.LEdge 1 $ G.Edge "a" "b"
                         , G.LEdge 2 $ G.Edge "b" "c"
                         , G.LEdge 3 $ G.Edge "c" "d"
                         ]
                       )
        result `shouldBe` expected

  context "expandNode" $ do
    
    let innerPath = mkTextPath $ start "x" -| 8 |- "y" -| 9 |- "z"
    
    it "should expand a singleton root node" $ do
        let outerPath = pathSingleNode
            result = P.expandNode "a" outerPath innerPath
            expected = innerPath
        result `shouldBe` expected

    it "should expand a root node of a path with many nodes" $ do
        let outerPath = pathManyNodes
            result = P.expandNode "a" outerPath innerPath
            expected = mkTextPath
              $ start "x" -| 8 |- "y" -| 9 |- "z" -| 1 |- "b" -| 2 |- "c" -| 3 |- "d"
        result `shouldBe` expected

    it "should expand a middle node of a path with many nodes" $ do
        let outerPath = pathManyNodes
            result = P.expandNode "c" outerPath innerPath
            expected = mkTextPath
              $ start "a" -| 1 |- "b" -| 2 |- "x" -| 8 |- "y" -| 9 |- "z" -| 3 |- "d"
        result `shouldBe` expected

    it "should expand an end node of a path with many nodes" $ do
        let outerPath = pathManyNodes
            result = P.expandNode "d" outerPath innerPath
            expected = mkTextPath
              $ start "a" -| 1 |- "b" -| 2 |- "c" -| 3 |- "x" -| 8 |- "y" -| 9 |- "z"
        result `shouldBe` expected

  context "append" $ do
    
    let pathSingleNode2 = mkTextPath $ start "x"
        pathManyNodes2 = mkTextPath $ start "x" -| 8 |- "y" -| 9 |- "z"
    
    it "should append two singleton paths" $ do
        let path1 = pathSingleNode
            path2 = pathSingleNode2
            result = P.append path1 99 path2
            expected = mkTextPath $ start "a" -| 99 |- "x"
        result `shouldBe` expected

    it "should append two paths with many nodes each" $ do
        let path1 = pathManyNodes
            path2 = pathManyNodes2
            result = P.append path1 99 path2
            expected = mkTextPath
              $ start "a" -| 1 |- "b" -| 2 |- "c" -| 3 |- "d"
              -| 99 |- "x" -| 8 |- "y" -| 9 |- "z"
        result `shouldBe` expected

  context "removeAfterNode" $ do    
    it "should remove nothing after last node" $ do
        let path = pathSingleNode
            result = P.removeAfterNode "a" path
            expected = mkTextPath $ start "a"
        result `shouldBe` expected

    it "should remove multiple nodes after middle node" $ do
        let path = pathManyNodes
            result = P.removeAfterNode "b" path
            expected = mkTextPath $ start "a" -| 1 |- "b"
        result `shouldBe` expected

  context "removeBeforeNode" $ do    
    it "should remove nothing if root node" $ do
        let path = pathManyNodes
            result = P.removeBeforeNode "a" path
            expected = pathManyNodes
        result `shouldBe` expected

    it "should remove multiple nodes before middle node" $ do
        let path = pathManyNodes
            result = P.removeBeforeNode "c" path
            expected = mkTextPath $ start "c" -| 3 |- "d"
        result `shouldBe` expected

  context "build" $ do    
    it "should build singleton" $ do
        let result = P.build $ start "a"
            expected = pathSingleNode
        result `shouldBe` expected

    it "should build path with many nodes" $ do
        let result = P.build $ start "a" -| 1 |- "b" -| 2 |- "c" -| 3 |- "d"
            expected = pathManyNodes
        result `shouldBe` expected
