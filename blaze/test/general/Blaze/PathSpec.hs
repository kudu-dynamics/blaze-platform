{- HLINT ignore "Redundant do" -}

module Blaze.PathSpec where

import Blaze.Prelude

import qualified Blaze.Graph as G
import Blaze.Types.Graph (NodeId(NodeId), StrictDescendantsMap)
import Blaze.Types.Graph.Alga (AlgaGraph)
import Blaze.Path
import Blaze.Types.Path.Alga (AlgaPath(AlgaPath, rootNode, graph))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import Test.Hspec

type TextGraph = AlgaGraph Int Text Text
type TextPath = AlgaPath Int Text Text
type TextDescendantsMap = StrictDescendantsMap Text

graphEmpty :: TextGraph
graphEmpty = G.empty

graphEmptyDmap :: TextDescendantsMap
graphEmptyDmap = G.calcStrictDescendantsMap graphEmpty

graphSingleNode :: TextGraph
graphSingleNode = G.fromNode "a"

graphSingleNodeDmap :: TextDescendantsMap
graphSingleNodeDmap = G.calcStrictDescendantsMap graphSingleNode

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
graphSingleEdgeDmap = G.calcStrictDescendantsMap graphSingleEdge

graphSinglePath :: TextGraph
graphSinglePath =
  G.fromEdges . fmap G.fromTupleLEdge $
    [ (1, ("a", "b"))
    , (2, ("b", "c"))
    , (3, ("c", "d"))
    ]

graphSinglePathDmap :: TextDescendantsMap
graphSinglePathDmap = G.calcStrictDescendantsMap graphSinglePath

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
graphTwoPathsDmap = G.calcStrictDescendantsMap graphTwoPaths

graphTwoUnevenPaths :: TextGraph
graphTwoUnevenPaths =
  G.fromEdges . fmap G.fromTupleLEdge $
    [ (1, ("a", "b"))
    , (2, ("b", "fin"))
    , (3, ("a", "c"))
    , (4, ("a", "d"))
    , (5, ("d", "fin"))
    ]

graphTwoUnevenPathsDmap :: TextDescendantsMap
graphTwoUnevenPathsDmap = G.calcStrictDescendantsMap graphTwoUnevenPaths

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
graphMultiPathDmap = G.calcStrictDescendantsMap graphMultiPath

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
graphWithSingleLoopDmap = G.calcStrictDescendantsMap graphWithSingleLoop

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
graphWithSingleLoopAndExitDmap = G.calcStrictDescendantsMap graphWithSingleLoopAndExit

-- This is the outling for a similar PIL cfg used to mock out func4
graphWithLoopAndInnerBranch :: TextGraph
graphWithLoopAndInnerBranch =
  G.fromEdges . fmap G.fromTupleLEdge $
  [ (1, ("a", "b"))
  , (2, ("a", "r"))
  
  , (3, ("b", "c"))

  , (4, ("c", "d"))
  , (5, ("c", "r"))

  , (6, ("d", "e"))
  , (7, ("d", "f"))

  , (8, ("e", "g"))
  , (9, ("f", "g"))

  , (10, ("g", "c"))
  ]

graphWithLoopAndInnerBranchDmap :: TextDescendantsMap
graphWithLoopAndInnerBranchDmap = G.calcStrictDescendantsMap graphWithLoopAndInnerBranch

spec :: Spec
spec = describe "Blaze.Path" $ do

  let noRevisit _ _ = error "Should not revisit"
      replicateOnRevisit = Text.replicate
      returnLowest :: (a, a) -> IO a
      returnLowest (a, _) = return a
      returnHighest :: (a, a) -> IO a
      returnHighest (_, b) = return b
      samplePath__ chooser s n = fmap fst . samplePath_ chooser s n

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
      sort result `shouldBe` sort expected

    it "should revisit loop nodes thrice if revisitLimit is 3" $ do
      let graph = graphWithSingleLoop
          result = getAllPaths replicateOnRevisit 3 "a" graph
          expected =
            [ mkTextPath $ start "a" -| 1 |- "b" -| 2 |- "c" -| 3 |- "d"
              -| 4 |- "bb" -| 2 |- "cc" -| 3 |- "dd"
              -| 4 |- "bbb" -| 2 |- "ccc" -| 3 |- "ddd"
              -| 4 |- "bbbb" -| 2 |- "cccc" -| 3 |- "dddd"
            ]
      sort result `shouldBe` sort expected

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
      sort result `shouldBe` sort expected

  context "getPathsContainingAndAvoiding_" $ do

    it "should return only paths that contain req nodes and do not contain avoid nodess" $ do
      let graph = graphMultiPath
          requiredNodes = HashSet.fromList ["c"]
          avoidNodes = HashSet.fromList["d"]
          dmap = graphMultiPathDmap
          result = getPathsContainingAndAvoiding_ dmap noRevisit 0 "a" graph requiredNodes avoidNodes
          expected = [ mkTextPath $ start "a" -| 1 |- "b" -| 2 |- "c" -| 7 |- "fin" ]
      result `shouldBe` (expected :: [TextPath])

  context "roulette" $ do
    it "should always return first item in single item list" $ do
      let xs = (0, "a") :| [] :: NonEmpty (Int, Text)
          pick = 0
          expected = "a"
      roulette xs pick `shouldBe` expected

    it "should return first item when pick is less than first item's val" $ do
      let xs = (1, "a") :| [(1, "b"),(4, "c"), (2, "d")] :: NonEmpty (Int, Text)
          pick = 0
          expected = "a"
      roulette xs pick `shouldBe` expected

    it "should return second item when pick is >= first item and < second" $ do
      let xs = (1, "a") :| [(1, "b"),(4, "c"), (2, "d")] :: NonEmpty (Int, Text)
          pick = 1
          expected = "b"
      roulette xs pick `shouldBe` expected

    it "should return nth item" $ do
      let xs = (1, "a") :| [(1, "b"),(4, "c"), (2, "d")] :: NonEmpty (Int, Text)
          pick = 5
          expected = "c"
      roulette xs pick `shouldBe` expected

    it "should return last item if pick is out of bounds" $ do
      let xs = (1, "a") :| [(1, "b"),(4, "c"), (2, "d")] :: NonEmpty (Int, Text)
          pick = 88888
          expected = "d"
      roulette xs pick `shouldBe` expected

  context "stochasticChoice" $ do
    let pickN :: Int -> ((Int, Int) -> Identity Int)
        pickN n _ = return n

    it "should always return first item in single item list" $ do
      let xs = (0, "a") :| [] :: NonEmpty (Int, Text)
          picker = pickN 999
          expected = "a"
      runIdentity (stochasticChoice picker xs) `shouldBe` expected

    it "should return nth item" $ do
      let xs = (1, "a") :| [(1, "b"),(4, "c"), (2, "d")] :: NonEmpty (Int, Text)
          picker = pickN 5
          expected = "c"
      runIdentity (stochasticChoice picker xs) `shouldBe` expected

  context "chooseChildByDescendantCount" $ do
    it "should return error if child node not found in descendants map" $ do
      let dmap = graphEmptyDmap
          parentNode = "a" :: Text
          childNodes = [((), "b")]
          expected = Left $ ChildNodeNotFoundInDescendantMap "b"
          action = flip runMonadChooser () $ chooseChildByDescendantCount returnLowest dmap parentNode childNodes
      action `shouldReturn` expected


  context "sampleRandomPath_" $ do
    it "should return Left error if graph is empty" $ do
      let graph = G.empty :: TextGraph
          requiredNodes = HashSet.empty
          dmap = graphEmptyDmap
          expected = Left StartNodeNotInGraph
          action = sampleRandomPath_ returnLowest dmap noRevisit 0 "a" graph requiredNodes :: IO (Either (SampleRandomPathError' Text) TextPath)
      action `shouldReturn` expected

    it "should return Left error req node is not in graph" $ do
      let graph = graphSinglePath :: TextGraph
          requiredNodes = HashSet.fromList ["z"]
          dmap = graphSinglePathDmap
          expected = Left . BranchChooserError $ NoReqNodesCanBeReached
          action = sampleRandomPath_ returnLowest dmap noRevisit 0 "a" graph requiredNodes :: IO (Either (SampleRandomPathError' Text) TextPath)
      action `shouldReturn` expected

    it "should return single path for graph with single path" $ do
      let graph = graphSinglePath :: TextGraph
          requiredNodes = HashSet.empty
          dmap = graphSinglePathDmap
          expected = Right . mkTextPath $ start "a" -| 1 |- "b" -| 2 |- "c" -| 3 |- "d"
          action = sampleRandomPath_ returnLowest dmap noRevisit 0 "a" graph requiredNodes :: IO (Either (SampleRandomPathError' Text) TextPath)
      action `shouldReturn` expected

    it "should continue to get path when req node is found, until term node" $ do
      let graph = graphSinglePath :: TextGraph
          requiredNodes = HashSet.fromList ["b"]
          dmap = graphSinglePathDmap
          expected = Right . mkTextPath $ start "a" -| 1 |- "b" -| 2 |- "c" -| 3 |- "d"
          action = sampleRandomPath_ returnLowest dmap noRevisit 0 "a" graph requiredNodes :: IO (Either (SampleRandomPathError' Text) TextPath)
      action `shouldReturn` expected

    it "should get full path after finding one req node, even when remaining req nodecan't be reached" $ do
      let graph = graphSinglePath :: TextGraph
          requiredNodes = HashSet.fromList ["z", "c"]
          dmap = graphSinglePathDmap
          expected = Right . mkTextPath $ start "a" -| 1 |- "b" -| 2 |- "c" -| 3 |- "d"
          action = sampleRandomPath_ returnLowest dmap noRevisit 0 "a" graph requiredNodes :: IO (Either (SampleRandomPathError' Text) TextPath)
      action `shouldReturn` expected

    it "should get path using rigged lowest-of-range random generator" $ do
      let graph = graphMultiPath :: TextGraph
          requiredNodes = HashSet.empty
          dmap = graphMultiPathDmap
          expected = Right . mkTextPath <$>
                     [ start "a" -| 1 |- "b" -| 2 |- "c" -| 7 |- "fin"
                     , start "a" -| 1 |- "b" -| 2 |- "c" -| 3 |- "d" -| 4 |- "fin"
                     ]
          action = sampleRandomPath_ returnLowest dmap noRevisit 0 "a" graph requiredNodes :: IO (Either (SampleRandomPathError' Text) TextPath)
      result <- action
      result `shouldSatisfy` (`elem` expected)

    it "should get path using rigged lowest-of-range random generator with required node" $ do
      let graph = graphMultiPath :: TextGraph
          requiredNodes = HashSet.fromList ["d"]
          dmap = graphMultiPathDmap
          expected = Right . mkTextPath
            $ start "a" -| 1 |- "b" -| 2 |- "c" -| 3 |- "d" -| 4 |- "fin"
          action = sampleRandomPath_ returnLowest dmap noRevisit 0 "a" graph requiredNodes :: IO (Either (SampleRandomPathError' Text) TextPath)
      action `shouldReturn` expected

    it "should get path using rigged highest-of-range random generator" $ do
      let graph = graphMultiPath :: TextGraph
          requiredNodes = HashSet.empty
          dmap = graphMultiPathDmap
          expected = Right . mkTextPath
            $ start "a" -| 1 |- "b" -| 5 |- "e" -| 6 |- "fin"
          action = sampleRandomPath_ returnHighest dmap noRevisit 0 "a" graph requiredNodes :: IO (Either (SampleRandomPathError' Text) TextPath)
      action `shouldReturn` expected

  context "samplePath_ sequence" $ do
    it "should return Left error if graph is empty" $ do
      let graph = G.empty :: TextGraph
          seq' = []
          dmap = graphEmptyDmap
          expected = Left StartNodeNotInGraph
          chooser = toFullChooser $ chooseChildByDescendantCountAndSequence returnLowest dmap
          action = runExceptT $ samplePath__ chooser seq' "a" graph :: IO (Either (SampleRandomPathError' Text) TextPath)
      action `shouldReturn` expected

    it "should return single node path of seq is empty" $ do
      let graph = graphSingleNode :: TextGraph
          seq' = []
          dmap = graphSingleNodeDmap
          startNode = "a"
          chooser = toFullChooser $ chooseChildByDescendantCountAndSequence returnLowest dmap
          action = runExceptT $ samplePath__ chooser seq' startNode graph :: IO (Either (SampleRandomPathError' Text) TextPath)
          expected = Right . mkTextPath $ start "a"
      action `shouldReturn` expected

    it "should error if single seq and single node don't match" $ do
      let graph = graphSingleNode :: TextGraph
          seq' = ["b"]
          dmap = graphSingleNodeDmap
          startNode = "a" 
          chooser = toFullChooser $ chooseChildByDescendantCountAndSequence returnLowest dmap
          action = runExceptT $ samplePath__ chooser seq' startNode graph :: IO (Either (SampleRandomPathError' Text) TextPath)
          expected = Left . BranchChooserError $ ReqNodeCannotBeReached
      action `shouldReturn` expected

    it "should succeed if single seq and single node match" $ do
      let graph = graphSingleNode :: TextGraph
          seq' = ["a"]
          dmap = graphSingleNodeDmap
          startNode = "a" 
          chooser = toFullChooser $ chooseChildByDescendantCountAndSequence returnLowest dmap
          action = runExceptT $ samplePath__ chooser seq' startNode graph :: IO (Either (SampleRandomPathError' Text) TextPath)
          expected = Right . mkTextPath $ start "a"
      action `shouldReturn` expected

    it "should find path that matches seq" $ do
      let graph = graphMultiPath :: TextGraph
          dmap = graphMultiPathDmap
          seq' = ["a", "d"]
          startNode = "a"
          chooser = toFullChooser $ chooseChildByDescendantCountAndSequence returnLowest dmap
          action = runExceptT $ samplePath__ chooser seq' startNode graph :: IO (Either (SampleRandomPathError' Text) TextPath)
          expected = Right . mkTextPath $ start "a" -| 1 |- "b" -| 2 |- "c" -| 3 |- "d"
      action `shouldReturn` expected

    it "should fail if order cannot be matched" $ do
      let graph = graphMultiPath :: TextGraph
          dmap = graphMultiPathDmap
          seq' = ["a", "d", "c"]
          startNode = "a"
          chooser = toFullChooser $ chooseChildByDescendantCountAndSequence returnLowest dmap
          action = runExceptT $ samplePath__ chooser seq' startNode graph :: IO (Either (SampleRandomPathError' Text) TextPath)
          expected = Left . BranchChooserError $ ReqNodeCannotBeReached
      action `shouldReturn` expected

  context "chooseChildByLeastVisitedDescendantCount" $ do
    -- run the chooser 10 times with random in range of 0.0-1.0 and records
    -- number of times each child node is chosen
    let getChooserResults :: StrictDescendantsMap Text -> VisitCounts Text -> Text -> [(Int, Text)] -> HashMap (Int, Text) Int
        getChooserResults dmap visitCounts parentNode choices = foldl' tryChooser HashMap.empty ([0..99] :: [Int])
          where
            tryChooser results n = let randDouble = return $ fromIntegral n / 100.0 in
              case runIdentity . flip runMonadChooser visitCounts $ chooseChildByLeastVisitedDescendantCount randDouble dmap parentNode choices of
                Left err -> error $ show err
                Right (Nothing, _) -> results
                Right (Just x, _) -> HashMap.alter (maybe (Just 1) (Just . (+1))) x results
        hasGreaterChance m a b = fromJust (HashMap.lookup a m) > fromJust (HashMap.lookup b m)
    
    it "should choose child with 50/50 chance if desc count is equal and no visitcounts" $ do
      let dmap = graphTwoPathsDmap
          visitCounts = emptyVisitCounts
          parentNode = "a"
          choices = [(1, "b"), (3, "c")]
          
          action = getChooserResults dmap visitCounts parentNode choices
          expected = HashMap.fromList
            [((1, "b"), 50)
            ,((3, "c"), 50)
            ]
      action `shouldBe` expected

    it "should favor child with descs that have not been visited" $ do
      let dmap = graphTwoPathsDmap
          visitCounts = visitCountsFromList [("b", 1)]
          parentNode = "a"
          choices = [(1, "b"), (3, "c")]
          
          action = getChooserResults dmap visitCounts parentNode choices
      hasGreaterChance action (3, "c") (1, "b") `shouldBe` True
