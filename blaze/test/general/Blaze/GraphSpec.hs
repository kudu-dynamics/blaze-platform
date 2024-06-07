{- HLINT ignore "Redundant do" -}

module Blaze.GraphSpec where

import Blaze.Prelude

import qualified Blaze.Types.Graph as G
import qualified Blaze.Graph as G
import Blaze.Graph (StrictDescendantsMap(StrictDescendantsMap), InterDescendantsMap(InterDescendantsMap), RouteAction(..), calcInterDescendantsMapForCfg, makeRoutes)
import Blaze.Types.Graph.Alga (AlgaGraph)

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Test.Hspec


newtype FakeFunc = FakeFunc Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

data FakeNode = BasicNode Int
              | CallNode Int FakeFunc
  deriving (Eq, Ord, Show, Generic, Hashable)

instance G.Identifiable FakeNode Int where
  getNodeId (BasicNode n) = G.NodeId n
  getNodeId (CallNode n _) = G.NodeId n

bb :: Int -> FakeNode
bb = BasicNode

cn :: Int -> FakeFunc -> FakeNode
cn = CallNode

type TextGraph = AlgaGraph () Text Text

type FakeGraph = AlgaGraph () Int FakeNode

type FakeCfg = (FakeNode, FakeGraph)

fromTuples ::
  ( Ord i
  , Hashable i
  , Hashable n
  , G.Identifiable n i
  )
  => [(n, n)] -> AlgaGraph () i n
fromTuples = G.fromEdges . fmap (G.fromTupleLEdge . ((),))

fooFunc :: FakeFunc
fooFunc = FakeFunc "foo"

fooCfg :: FakeCfg
fooCfg =
  ( bb 8
  , fromTuples
    [ (bb 8, bb 9)
    ]
  )

barFunc :: FakeFunc
barFunc = FakeFunc "bar"

barCfg :: FakeCfg
barCfg =
  ( bb 0
  , fromTuples
    [ (bb 0, bb 1)
    , (bb 1, bb 5)
    , (bb 0, bb 2)
    , (bb 2, cn 3 fooFunc)
    , (cn 3 fooFunc, bb 5)
    , (cn 3 fooFunc, bb 4)
    , (bb 4, bb 2)
    ]
  )

buzzFunc :: FakeFunc
buzzFunc = FakeFunc "buzz"

buzzCfg :: FakeCfg
buzzCfg =
  ( bb 20
  , fromTuples
    [ (bb 20, cn 21 fooFunc)
    , (cn 21 fooFunc, bb 25)

    , (bb 20, cn 22 barFunc)
    , (cn 22 barFunc, bb 23)
    , (bb 23, bb 25)

    , (cn 22 barFunc, cn 24 fooFunc)
    , (cn 24 fooFunc, bb 25)
    ]
  )

jenkinsFunc :: FakeFunc
jenkinsFunc = FakeFunc "jenkins"

jenkinsCfg :: FakeCfg
jenkinsCfg =
  ( bb 30
  , fromTuples
    [ (bb 30, cn 31 fooFunc)
    , (bb 30, cn 32 fooFunc)
    , (cn 31 fooFunc, bb 33)
    , (cn 32 fooFunc, bb 33)
    ]
  )

bilboFunc :: FakeFunc
bilboFunc = FakeFunc "bilbo"

bilboCfg :: FakeCfg
bilboCfg =
  ( bb 40
  , fromTuples
    [ (bb 40, cn 41 fooFunc)
    , (cn 41 fooFunc, cn 42 fooFunc)
    , (cn 42 fooFunc, bb 43)
    ]
  )

twigoFunc :: FakeFunc
twigoFunc = FakeFunc "twigo"

twigoCfg :: FakeCfg
twigoCfg =
  ( bb 50
  , fromTuples
    [ (bb 50, cn 51 bigoFunc)
    , (cn 51 bigoFunc, bb 53)
    
    , (bb 50, cn 52 fooFunc)
    , (cn 52 fooFunc, bb 53)
    ]
  )

bigoFunc :: FakeFunc
bigoFunc = FakeFunc "bigo"

bigoCfg :: FakeCfg
bigoCfg =
  ( bb 60
  , fromTuples
    [ (bb 60, cn 61 fooFunc)
    , (cn 61 fooFunc, bb 62)
    ]
  )

fakeCfgs :: HashMap FakeFunc FakeCfg
fakeCfgs = HashMap.fromList
  [ (fooFunc, fooCfg)
  , (barFunc, barCfg)
  , (buzzFunc, buzzCfg)
  , (jenkinsFunc, jenkinsCfg)
  , (bilboFunc, bilboCfg)
  , (twigoFunc, twigoCfg)
  , (bigoFunc, bigoCfg)
  ]

fakeStartNodes :: HashMap FakeFunc FakeNode
fakeStartNodes = fst <$> fakeCfgs

fakeDmaps :: HashMap FakeFunc (StrictDescendantsMap FakeNode)
fakeDmaps = G.calcStrictDescendantsMap . snd <$> fakeCfgs

fakeOuterNodeDescendants :: G.OuterNodeDescendants FakeFunc FakeNode
fakeOuterNodeDescendants = HashMap.fromList
  [ ( fooFunc
    , getNodes fooCfg
    )
  , ( barFunc
    , getNodes barCfg <> getNodes fooCfg
    )
  , ( buzzFunc
    , getNodes buzzCfg <> getNodes barCfg <> getNodes fooCfg
    )
  , ( jenkinsFunc
    , getNodes jenkinsCfg <> getNodes fooCfg
    )
  , ( bilboFunc
    , getNodes bilboCfg <> getNodes fooCfg
    )
  , ( twigoFunc
    , getNodes twigoCfg <> getNodes bigoCfg <> getNodes fooCfg
    )
  , ( bigoFunc
    , getNodes bigoCfg <> getNodes fooCfg
    )
  ]
  where
    getNodes = G.nodes . snd

spec :: Spec
spec = describe "Blaze.Graph" $ do
  let toLedge = G.fromTupleLEdge . ((),)
  context "getPostDominators_" $ do
    it "should get empty PostDominators for singleton graph" $ do
      let g :: TextGraph
          g = G.fromNode "a"
          termNode = "a"
          expected = G.PostDominators . HashMap.fromList $ []

      G.getPostDominators_ termNode g `shouldBe` expected

    it "should get single PostDominator for graph with one edge" $ do
      let g :: TextGraph
          g = G.fromEdges . fmap toLedge $
            [("a", "b")]
          termNode = "b"
          expected = G.PostDominators . HashMap.fromList $
            [("a", HashSet.fromList ["b"])]

      G.getPostDominators_ termNode g `shouldBe` expected

    it "should get empty PostDominator when root node is supplied as term node" $ do
      let g :: TextGraph
          g = G.fromEdges . fmap toLedge $
            [("a", "b")]
          termNode = "a"
          expected = G.PostDominators . HashMap.fromList $ []

      G.getPostDominators_ termNode g `shouldBe` expected

    it "should get two PostDominator entries for two-to-one graph" $ do
      let g :: TextGraph
          g = G.fromEdges . fmap toLedge $
            [ ("a", "c")
            , ("b", "c")
            ]
          termNode = "c"
          expected = G.PostDominators . HashMap.fromList $
            [ ("a", HashSet.fromList ["c"])
            , ("b", HashSet.fromList ["c"])
            ]

      G.getPostDominators_ termNode g `shouldBe` expected

  context "getPostDominators" $ do
    let dummyTermNode = "z"
        dummyTermEdgeLabel = ()
        getPostDominators = G.getPostDominators dummyTermNode dummyTermEdgeLabel

    it "should get empty PostDominators for singleton graph" $ do
      let g :: TextGraph
          g = G.fromNode "a"
          expected = G.PostDominators . HashMap.fromList $ []

      getPostDominators g `shouldBe` expected

    it "should get single PostDominator for graph with one edge" $ do
      let g :: TextGraph
          g = G.fromEdges . fmap toLedge $
            [("a", "b")]
          expected = G.PostDominators . HashMap.fromList $
            [("a", HashSet.fromList ["b"])]

      getPostDominators g `shouldBe` expected

    it "should get two PostDominator entries for two-to-one graph" $ do
      let g :: TextGraph
          g = G.fromEdges . fmap toLedge $
            [ ("a", "c")
            , ("b", "c")
            ]
          expected = G.PostDominators . HashMap.fromList $
            [ ("a", HashSet.fromList ["c"])
            , ("b", HashSet.fromList ["c"])
            ]

      getPostDominators g `shouldBe` expected

    it "should find post dominators when graphs have multiple term nodes" $ do
      let g :: TextGraph
          g = G.fromEdges . fmap toLedge $
            [ ("a", "c")
            , ("c", "e")
            , ("c", "f")
            , ("b", "d")
            , ("d", "f")
            ]
          expected = G.PostDominators . HashMap.fromList $
            [ ("a", HashSet.fromList ["c"])
            , ("b", HashSet.fromList ["d", "f"])
            , ("d", HashSet.fromList ["f"])
            ]

      getPostDominators g `shouldBe` expected

    it "should find post dominators for double diamond graph" $ do
      let g :: TextGraph
          g = G.fromEdges . fmap toLedge $
            [ ("a", "b1")
            , ("a", "b2")
            , ("b1", "c")
            , ("b2", "c")
            , ("c", "d1")
            , ("c", "d2")
            , ("d1", "e")
            , ("d2", "e")
            ]
          expected = G.PostDominators . HashMap.fromList $
            [ ("a", HashSet.fromList ["c", "e"])
            , ("b1", HashSet.fromList ["c", "e"])
            , ("b2", HashSet.fromList ["c", "e"])
            , ("c", HashSet.fromList ["e"])
            , ("d1", HashSet.fromList ["e"])
            , ("d2", HashSet.fromList ["e"])
            ]

      getPostDominators g `shouldBe` expected

  context "getDominators" $ do
    it "should get empty Dominators for singleton graph" $ do
      let g :: TextGraph
          g = G.fromNode "a"
          rootNode = "a"
          expected = G.Dominators . HashMap.fromList $ []

      G.getDominators rootNode g `shouldBe` expected

    it "should get single Dominator for graph with one edge" $ do
      let g :: TextGraph
          g = G.fromEdges . fmap toLedge $
            [("a", "b")]
          rootNode = "a"
          expected = G.Dominators . HashMap.fromList $
            [("b", HashSet.fromList ["a"])]

      G.getDominators rootNode g `shouldBe` expected

  context "reachable" $ do
    it "should reach nothing if node is not in graph" $ do
      let g :: TextGraph
          g = G.fromNode "a"
          expected = []

      G.reachable "n" g `shouldBe` expected

    it "should include search node in results" $ do
      let g :: TextGraph
          g = G.fromNode "a"
          expected = ["a"]

      G.reachable "a" g `shouldBe` expected

  context "calcOuterNodeDescendants" $ do
    
    it "should return empty result if ancestors map is empty" $ do
      let getInnerNodes _ = HashSet.empty
          ancestorsMap = HashMap.empty
          expected = HashMap.empty :: HashMap Text (HashSet Int)
          action = G.calcOuterNodeDescendants getInnerNodes ancestorsMap

      action `shouldBe` expected

    it "should return single key with inner nodes" $ do
      let getInnerNodes k = fromMaybe HashSet.empty . HashMap.lookup k . HashMap.fromList $ innerNodes
          innerNodes =
            [ ("a", HashSet.fromList [1, 2 ,3]) ]
            
          ancestorsMap = HashMap.fromList [("a", HashSet.empty)]
          
          action = G.calcOuterNodeDescendants getInnerNodes ancestorsMap
          
          expected :: HashMap Text (HashSet Int)
          expected = HashMap.fromList
            [ ("a", HashSet.fromList [1, 2 ,3]) ]

      action `shouldBe` expected

    it "should add children nodes to ancestors" $ do
      let getInnerNodes k = fromMaybe HashSet.empty . HashMap.lookup k . HashMap.fromList $ innerNodes
          innerNodes =
            [ ("a", HashSet.fromList [1, 2 ,3])
            , ("b", HashSet.fromList [4, 5])
            ]
            
          ancestorsMap = HashMap.fromList [ ("a", HashSet.fromList ["b"])
                                          , ("b", HashSet.empty)
                                          ]
          
          action = G.calcOuterNodeDescendants getInnerNodes ancestorsMap
          
          expected :: HashMap Text (HashSet Int)
          expected = HashMap.fromList
            [ ("a", HashSet.fromList [1, 2 ,3])
            , ("b", HashSet.fromList [1, 2, 3, 4, 5])
            ]
      action `shouldBe` expected

  context "calcInterDescendantsMap" $ do
    let getTransToOuter (BasicNode _) = Nothing
        getTransToOuter (CallNode _ func) = Just func
        bn = BasicNode
        ff = FakeFunc

    it "should return empty result if descendants map is empty" $ do
      let dmap = StrictDescendantsMap HashMap.empty
          outerNodeDescendants = HashMap.empty
          action = calcInterDescendantsMapForCfg getTransToOuter dmap outerNodeDescendants
          expected = InterDescendantsMap HashMap.empty :: InterDescendantsMap FakeNode
      action `shouldBe` expected

    it "should return the internal descendants if no nodes trans to outer" $ do
      let dmap_ = HashMap.fromList
                  [ (bn 0, HashSet.fromList [bn 1, bn 2, bn 3])
                  , (bn 1, HashSet.fromList [bn 3])
                  , (bn 2, HashSet.fromList [bn 3])
                  , (bn 3, HashSet.empty)
                  ]
          dmap = StrictDescendantsMap dmap_
          outerNodeDescendants = HashMap.fromList
            [ (ff "a", HashSet.fromList [bn 0, bn 1, bn 2, bn 3])
            ]
          action = calcInterDescendantsMapForCfg getTransToOuter dmap outerNodeDescendants
          expected = InterDescendantsMap dmap_
      action `shouldBe` expected

    it "should include outer's descendants only for ancestors of trans node and nodes descending from call site should be descendants of nodes inside calls " $ do
      let dmap_ = HashMap.fromList
            [ (bn 0, HashSet.fromList [bn 1, bn 2, cn 3 (ff "b"), cn 4 (ff "c"), bn 5])
            , (bn 1, HashSet.fromList [cn 3 (ff "b"), bn 5])
            , (bn 2, HashSet.fromList [cn 4 (ff "c"), bn 5])
            , (cn 3 (ff "b"), HashSet.fromList [bn 5])
            , (cn 4 (ff "c"), HashSet.fromList [bn 5])
            , (bn 5, HashSet.empty)
            ]
          dmap = StrictDescendantsMap dmap_
          outerNodeDescendants = HashMap.fromList
            [ (ff "a", HashSet.fromList [bn 1, bn 2, cn 3 (ff "b"), cn 4 (ff "c"), bn 5])
            , (ff "b", HashSet.fromList [bn 80, bn 81])
            , (ff "c", HashSet.fromList [bn 90])
            ]
          action = calcInterDescendantsMapForCfg getTransToOuter dmap outerNodeDescendants
          expected = InterDescendantsMap $ HashMap.fromList
            [ (bn 0, HashSet.fromList [bn 1, bn 2, cn 3 (ff "b"), cn 4 (ff "c"), bn 5, bn 80, bn 81, bn 90])
            , (bn 1, HashSet.fromList [cn 3 (ff "b"), bn 5, bn 80, bn 81])
            , (bn 2, HashSet.fromList [cn 4 (ff "c"), bn 5, bn 90])
            , (cn 3 (ff "b"), HashSet.fromList [bn 5])
            , (cn 4 (ff "c"), HashSet.fromList [bn 5])
            , (bn 5, HashSet.empty)
            ]                  
      action `shouldBe` expected

    it "should handle recursive call" $ do
      let dmap_ = HashMap.fromList
            [ (bn 0, HashSet.fromList [bn 1, bn 2, cn 3 (ff "a"), bn 4, bn 5])
            , (bn 1, HashSet.fromList [cn 3 (ff "a"), bn 5])
            , (bn 2, HashSet.fromList [bn 4, bn 5])
            , (cn 3 (ff "a"), HashSet.fromList [bn 5])
            , (bn 4, HashSet.fromList [bn 5])
            , (bn 5, HashSet.empty)
            ]
          dmap = StrictDescendantsMap dmap_
          outerNodeDescendants = HashMap.fromList
            [ (ff "a", HashSet.fromList [bn 1, bn 2, cn 3 (ff "a"), bn 4, bn 5])
            ]
          action = calcInterDescendantsMapForCfg getTransToOuter dmap outerNodeDescendants
          expected = InterDescendantsMap $ HashMap.fromList
            [ (bn 0, HashSet.fromList [bn 1, bn 2, cn 3 (ff "a"), bn 4, bn 5])
            -- Includes all nodes because recursive call
            , (bn 1, HashSet.fromList [cn 3 (ff "a"), bn 5, bn 1, bn 2, bn 4])
            , (bn 2, HashSet.fromList [bn 4, bn 5])
            , (cn 3 (ff "a"), HashSet.fromList [bn 5])
            , (bn 4, HashSet.fromList [bn 5])
            , (bn 5, HashSet.empty)
            ]                  
      action `shouldBe` expected

  context "makeRoutes" $ do
    let getTransNodeContext' (BasicNode _) = Nothing
        getTransNodeContext' (CallNode _ x) = Just x
        routeMakerCtx = G.RouteMakerCtx
          { getTransNodeContext = getTransNodeContext'
          , getStartNode = fakeStartNodes
          , getDescendantsMap = fakeDmaps
          , outerContextNodeDescendants = fakeOuterNodeDescendants
          , maxCallDepth = 3
          }

    it "should return a single 'finished' route if req sequence is empty" $ do
      let ctx = routeMakerCtx
      
          reqSeq = []
          currentOuterContext = fooFunc
          currentNode = bb 8
          
          action = makeRoutes ctx currentOuterContext currentNode reqSeq
          expected = [[Finished]]
      action `shouldBe` expected

    it "should return [] if seq cannot be found" $ do
      let ctx = routeMakerCtx
      
          reqSeq = [bb 99]
          currentOuterContext = fooFunc
          currentNode = bb 8
          
          action = makeRoutes ctx currentOuterContext currentNode reqSeq
          expected = []
      action `shouldBe` expected

    it "should find seq in func with single path" $ do
      let ctx = routeMakerCtx
      
          reqSeq = [bb 9]
          currentOuterContext = fooFunc
          currentNode = bb 8
          
          action = makeRoutes ctx currentOuterContext currentNode reqSeq
          expected = [ [InnerNode $ bb 9, Finished]
                     ]
      action `shouldBe` expected

    it "should find seq of two nodes in func with single path" $ do
      let ctx = routeMakerCtx
      
          currentOuterContext = fooFunc
          currentNode = bb 8

          reqSeq = [bb 8, bb 9]
          
          action = makeRoutes ctx currentOuterContext currentNode reqSeq
          expected = [ [InnerNode $ bb 8, InnerNode $ bb 9, Finished]
                     ]
      action `shouldBe` expected

    it "should reject seq with correct nodes but incorrect order" $ do
      let ctx = routeMakerCtx
      
          currentOuterContext = fooFunc
          currentNode = bb 8

          reqSeq = [bb 9, bb 8]
          
          action = makeRoutes ctx currentOuterContext currentNode reqSeq
          expected = []
      action `shouldBe` expected

    it "should not consume duplicate root nodes without loop" $ do
      let ctx = routeMakerCtx
      
          currentOuterContext = fooFunc
          currentNode = bb 8

          reqSeq = [bb 8, bb 8]
          
          action = makeRoutes ctx currentOuterContext currentNode reqSeq
          expected = []

      action `shouldBe` expected

    it "should not consume duplicate non-root nodes without loop" $ do
      let ctx = routeMakerCtx
      
          currentOuterContext = fooFunc
          currentNode = bb 8

          reqSeq = [bb 9, bb 9]
          
          action = makeRoutes ctx currentOuterContext currentNode reqSeq
          expected = []

      action `shouldBe` expected

    it "should consume duplicate nodes using loop" $ do
      let ctx = routeMakerCtx
      
          currentOuterContext = barFunc
          currentNode = bb 0

          reqSeq = [bb 2, bb 2]
          
          action = makeRoutes ctx currentOuterContext currentNode reqSeq
          expected = [ [InnerNode $ bb 2, InnerNode $ bb 2, Finished]
                     ]

      action `shouldBe` expected

    it "should enter call to reach node and finish if it is last node in seq" $ do
      let ctx = routeMakerCtx
      
          currentOuterContext = barFunc
          currentNode = bb 0

          reqSeq = [bb 9]
          
          action = makeRoutes ctx currentOuterContext currentNode reqSeq
          expected = [ [ EnterContext (cn 3 fooFunc) fooFunc
                       , InnerNode $ bb 9
                       , Finished
                       ]
                     ]

      action `shouldBe` expected

    it "should enter call to reach node and exit context if not last node in seq" $ do
      let ctx = routeMakerCtx
      
          currentOuterContext = barFunc
          currentNode = bb 0

          reqSeq = [bb 9, bb 5]
          
          action = makeRoutes ctx currentOuterContext currentNode reqSeq
          expected = [ [ EnterContext (cn 3 fooFunc) fooFunc
                       , InnerNode $ bb 9
                       , ExitContext fooFunc
                       , InnerNode $ bb 5
                       , Finished
                       ]
                     ]

      action `shouldBe` expected


    it "should go through loop to reach node then enter call above in loop" $ do
      let ctx = routeMakerCtx
      
          currentOuterContext = barFunc
          currentNode = bb 0

          reqSeq = [bb 4, bb 9]
          
          action = makeRoutes ctx currentOuterContext currentNode reqSeq
          expected = [ [ InnerNode $ bb 4
                       , EnterContext (cn 3 fooFunc) fooFunc
                       , InnerNode $ bb 9
                       , Finished
                       ]
                     ]

      action `shouldBe` expected

    it "should find no routes if seq nodes are split in two sibling branches without loop" $ do
      let ctx = routeMakerCtx
      
          currentOuterContext = barFunc
          currentNode = bb 0

          reqSeq = [bb 1, bb 4]
          
          action = makeRoutes ctx currentOuterContext currentNode reqSeq
          expected = []

      action `shouldBe` expected

    it "should find two routes for function that calls same target func in parallel" $ do
      let ctx = routeMakerCtx
      
          currentOuterContext = jenkinsFunc
          currentNode = bb 30

          reqSeq = [bb 9]
          
          action = makeRoutes ctx currentOuterContext currentNode reqSeq
          expected = [ [ EnterContext (cn 31 fooFunc) fooFunc
                       , InnerNode $ bb 9
                       , Finished
                       ]
                     , [ EnterContext (cn 32 fooFunc) fooFunc
                       , InnerNode $ bb 9
                       , Finished
                       ]
                     ]

      sort action `shouldBe` sort expected


    it "should find two routes for function that calls same target func in sequence" $ do
      let ctx = routeMakerCtx
      
          currentOuterContext = bilboFunc
          currentNode = bb 40

          reqSeq = [bb 9]
          
          action = makeRoutes ctx currentOuterContext currentNode reqSeq
          expected = [ [ EnterContext (cn 41 fooFunc) fooFunc
                       , InnerNode $ bb 9
                       , Finished
                       ]
                     , [ EnterContext (cn 42 fooFunc) fooFunc
                       , InnerNode $ bb 9
                       , Finished
                       ]
                     ]

      sort action `shouldBe` sort expected

    it "should split and back out of inner func that has both sequence nodes just in case outer context also has second sequence node" $ do
      let ctx = routeMakerCtx
      
          currentOuterContext = bilboFunc
          currentNode = bb 40

          reqSeq = [bb 8, bb 9]
          
          action = makeRoutes ctx currentOuterContext currentNode reqSeq
          expected = [ [ EnterContext (cn 41 fooFunc) fooFunc
                       , InnerNode $ bb 8
                       , InnerNode $ bb 9
                       , Finished
                       ]
                     , [ EnterContext (cn 42 fooFunc) fooFunc
                       , InnerNode $ bb 8
                       , InnerNode $ bb 9
                       , Finished
                       ]
                     , [ EnterContext (cn 41 fooFunc) fooFunc
                       , InnerNode $ bb 8
                       , ExitContext fooFunc
                       , EnterContext (cn 42 fooFunc) fooFunc
                       , InnerNode $ bb 9
                       , Finished
                       ]
                     ]

      sort action `shouldBe` sort expected


    it "should find multiple routes to reach node in function called in multiple locations at various depths" $ do
      let ctx = routeMakerCtx
      
          currentOuterContext = buzzFunc
          currentNode = bb 20

          reqSeq = [bb 9]
          
          action = makeRoutes ctx currentOuterContext currentNode reqSeq
          expected = [ [ EnterContext (cn 21 fooFunc) fooFunc
                       , InnerNode $ bb 9
                       , Finished
                       ]
                     , [ EnterContext (cn 24 fooFunc) fooFunc
                       , InnerNode $ bb 9
                       , Finished
                       ]
                     , [ EnterContext (cn 22 barFunc) barFunc
                       , EnterContext (cn 3 fooFunc) fooFunc
                       , InnerNode $ bb 9
                       , Finished
                       ]
                     ]

      sort action `shouldBe` sort expected

    it "should limit routes that exceed call depth" $ do
      let ctx = routeMakerCtx
      
          currentOuterContext = twigoFunc
          currentNode = bb 50

          reqSeq = [bb 9]

          ctx' = ctx & #maxCallDepth .~ (2 :: Word64)
          
          action = makeRoutes ctx' currentOuterContext currentNode reqSeq
          expected = [ [ EnterContext (cn 52 fooFunc) fooFunc
                       , InnerNode $ bb 9
                       , Finished
                       ]
                     ]

          action' = makeRoutes ctx currentOuterContext currentNode reqSeq
          expected' = expected
            <> [ [ EnterContext (cn 51 bigoFunc) bigoFunc
                 , EnterContext (cn 61 fooFunc) fooFunc
                 , InnerNode $ bb 9
                 , Finished
                 ]
               ]

      sort action `shouldBe` sort expected
      sort action' `shouldBe` sort expected'

    it "should limit routes that exceed call depth in complex function" $ do
      let ctx = routeMakerCtx
      
          currentOuterContext = buzzFunc
          currentNode = bb 20

          reqSeq = [bb 9]

          ctx' = ctx & #maxCallDepth .~ (2 :: Word64)
          
          action = makeRoutes ctx' currentOuterContext currentNode reqSeq
          expected = [ [ EnterContext (cn 21 fooFunc) fooFunc
                       , InnerNode $ bb 9
                       , Finished
                       ]
                     , [ EnterContext (cn 24 fooFunc) fooFunc
                       , InnerNode $ bb 9
                       , Finished
                       ]
                     ]

      sort action `shouldBe` sort expected
