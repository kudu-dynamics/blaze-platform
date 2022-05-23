{- HLINT ignore "Redundant do" -}

module Blaze.Types.Cfg.GroupingSpec where

import qualified Blaze.Cfg as Cfg
import Blaze.Function (Function (Function))
import Blaze.Prelude
import Blaze.Graph (getNodeId)
import Blaze.Types.Cfg (
  BasicBlockNode (BasicBlockNode),
  BranchType (..),
  CfNode (Grouping),
  Cfg,
  GroupingNode (GroupingNode),
 )
import Blaze.Types.Cfg.Grouping (
  GroupSpec (GroupSpec),
  GroupingTree,
  extractGroupingNode,
  findNodesInGroup,
  foldGroups,
  initialNode,
  makeGrouping,
  deepTerminalNode,
  unfoldGroups,
 )
import Blaze.Types.Pil (Ctx (Ctx))
import Blaze.Util (prettyShouldBe)
import Blaze.Util.Spec (mkUuid2)
import qualified Data.HashSet as HashSet
import Test.Hspec

ctx :: Ctx
ctx = Ctx func 0
  where
    func = Function Nothing "foo" 0x00 []

groupbb :: a -> CfNode a -> Cfg (CfNode a) -> CfNode a
groupbb ndata end subcfg =
  Grouping
    GroupingNode
      { termNodeId = getNodeId end
      , uuid = Cfg.getNodeUUID . Cfg.getRootNode $ subcfg
      , grouping = subcfg
      , nodeData = ndata
      }

gbb :: Address -> Address -> a -> CfNode a
cbb :: Address -> Address -> a -> CfNode a
(gbb, cbb) = (go Cfg.BasicBlock, go Cfg.BasicBlock)
  where
    go f startAddr endAddr x = f $ BasicBlockNode
      { ctx = Ctx (Function Nothing "f" 0 []) 0
      , start = startAddr
      , end = endAddr
      , uuid = uuid'
      , nodeData = x
      }
      where
        uuid' = mkUuid2 startAddr endAddr

gbbn :: Text -> CfNode [Text]
cbbn :: Text -> CfNode [Text]
(gbbn, cbbn) = (go gbb, go cbb)
  where
    go f n = let x = fromIntegral $ hash n in f x x [n]

spec :: Spec
spec = describe "Blaze.Types.Cfg.Grouping" $ do
  let ungrouped =
        Cfg.mkCfg 0
          (cbbn "root")
          [ cbbn "branch1"
          , cbbn "branch11"
          , cbbn "mid12"
          , cbbn "mid111"
          , cbbn "mid112"
          , cbbn "end11"
          , cbbn "end1"
          , cbbn "end"
          ]
          [ Cfg.CfEdge (cbbn "root") (cbbn "branch1") UnconditionalBranch
          , Cfg.CfEdge (cbbn "branch1") (cbbn "branch11") TrueBranch
          , Cfg.CfEdge (cbbn "branch1") (cbbn "mid12") FalseBranch
          , Cfg.CfEdge (cbbn "branch11") (cbbn "mid111") TrueBranch
          , Cfg.CfEdge (cbbn "branch11") (cbbn "mid112") FalseBranch
          , Cfg.CfEdge (cbbn "mid111") (cbbn "end11") UnconditionalBranch
          , Cfg.CfEdge (cbbn "mid112") (cbbn "end11") UnconditionalBranch
          , Cfg.CfEdge (cbbn "end11") (cbbn "end1") UnconditionalBranch
          , Cfg.CfEdge (cbbn "mid12") (cbbn "end1") UnconditionalBranch
          , Cfg.CfEdge (cbbn "end1") (cbbn "end") UnconditionalBranch
          ]
      mid111group = groupbb [""] (gbbn "mid111") (Cfg.mkCfg 0 (gbbn "mid111") [] [])
      mid111groupgroup = groupbb [""] mid111group (Cfg.mkCfg 0 mid111group [] [])
      mid112group = groupbb [""] (gbbn "mid112") (Cfg.mkCfg 0 (gbbn "mid112") [] [])
      branch11group =
        groupbb [""] (gbbn "end11") $
          Cfg.mkCfg 0
            (gbbn "branch11")
            [ mid111groupgroup
            , mid112group
            , gbbn "end11"
            ]
            [ Cfg.CfEdge (gbbn "branch11") mid111groupgroup TrueBranch
            , Cfg.CfEdge (gbbn "branch11") mid112group FalseBranch
            , Cfg.CfEdge mid111groupgroup (gbbn "end11") UnconditionalBranch
            , Cfg.CfEdge mid112group (gbbn "end11") UnconditionalBranch
            ]
      branch1group =
        groupbb [""] (gbbn "end1") $
          Cfg.mkCfg 0
            (gbbn "branch1")
            [ branch11group
            , gbbn "mid12"
            , gbbn "end1"
            ]
            [ Cfg.CfEdge (gbbn "branch1") branch11group TrueBranch
            , Cfg.CfEdge (gbbn "branch1") (gbbn "mid12") FalseBranch
            , Cfg.CfEdge branch11group (gbbn "end1") UnconditionalBranch
            , Cfg.CfEdge (gbbn "mid12") (gbbn "end1") UnconditionalBranch
            ]
      gGrouped =
        Cfg.mkCfg 0
          (gbbn "root")
          [ branch1group
          , gbbn "end"
          ]
          [ Cfg.CfEdge (gbbn "root") branch1group UnconditionalBranch
          , Cfg.CfEdge branch1group (gbbn "end") UnconditionalBranch
          ]
      expectedGrouping =
        [ GroupSpec
            (cbbn "branch1")
            (cbbn "end1")
            [ GroupSpec
                (cbbn "branch11")
                (cbbn "end11")
                [ GroupSpec
                    (cbbn "mid111")
                    (cbbn "mid111")
                    [ GroupSpec
                        (cbbn "mid111")
                        (cbbn "mid111")
                        []
                        [""]
                    ]
                    [""]
                , GroupSpec
                    (cbbn "mid112")
                    (cbbn "mid112")
                    []
                    [""]
                ]
                [""]
            ]
            [""]
        ]

  context "initialNode" $ do
    it "finds the initial node of a Grouping node" $ do
      initialNode branch1group `prettyShouldBe` cbbn "branch1"
      initialNode mid111groupgroup `prettyShouldBe` cbbn "mid111"
    it "returns the original node for a non Grouping node" $ do
      initialNode (gbbn "node") `prettyShouldBe` cbbn "node"

  context "deepTerminalNode" $ do
    it "finds the terminal node of a Grouping node" $ do
      deepTerminalNode branch1group `prettyShouldBe` cbbn "end1"
      deepTerminalNode mid111groupgroup `prettyShouldBe` cbbn "mid111"
    it "returns the original node for a non Grouping node" $ do
      deepTerminalNode (gbbn "node") `prettyShouldBe` cbbn "node"

  let normGroupingTree :: Ord a => GroupingTree a -> GroupingTree a
      normGroupingTree = sort . fmap (over #innerGroups normGroupingTree)

  context "unfoldGroups" $ do
    it "should unfold all groups of a grouped CFG into a flat CFG" $ do
      let (unfolded, groupingTree) = unfoldGroups gGrouped
      unfolded `prettyShouldBe` ungrouped
      normGroupingTree groupingTree `prettyShouldBe` normGroupingTree expectedGrouping

  context "foldGroups" $ do
    it "should fold all groups found in a flat CFG into the original grouped CFG" $ do
      let folded = foldGroups ungrouped expectedGrouping
      folded `prettyShouldBe` gGrouped

  context "findNodesInGroup" $ do
    it "should return empty set if group is just start and end with no middle" $ do
      let cfg = Cfg.mkCfg 0
            (gbbn "root")
            [ gbbn "mid"
            , gbbn "end"
            ]
            [ Cfg.CfEdge
              (gbbn "root")
              (gbbn "mid")
              UnconditionalBranch
            , Cfg.CfEdge
              (gbbn "mid")
              (gbbn "end")
              UnconditionalBranch
            ]
          startNode = gbbn "mid"
          endNode = gbbn "end"
          expected = HashSet.empty
      findNodesInGroup startNode endNode cfg `prettyShouldBe` expected

    it "should return single middle node" $ do
      let startNode = gbbn "start"
          midNode = gbbn "mid"
          endNode = gbbn "end"
          cfg = Cfg.mkCfg 0
            startNode
            [ midNode
            , endNode
            ]
            [ Cfg.CfEdge
              startNode
              midNode
              UnconditionalBranch
            , Cfg.CfEdge
              midNode
              endNode
              UnconditionalBranch
            ]
          expected = HashSet.singleton midNode

      findNodesInGroup startNode endNode cfg `prettyShouldBe` expected

    it "should return two middle nodes" $ do
      let startNode = gbbn "start"
          midNode1 = gbbn "mid1"
          midNode2 = gbbn "mid2"
          endNode = gbbn "end"
          cfg = Cfg.mkCfg 0
            startNode
            [ midNode1
            , midNode2
            , endNode
            ]
            [ Cfg.CfEdge
              startNode
              midNode1
              TrueBranch
            , Cfg.CfEdge
              startNode
              midNode2
              FalseBranch
            , Cfg.CfEdge
              midNode2
              endNode
              UnconditionalBranch
            , Cfg.CfEdge
              midNode1
              endNode
              UnconditionalBranch
            ]
          expected = HashSet.fromList [midNode1, midNode2]

      findNodesInGroup startNode endNode cfg `prettyShouldBe` expected

  context "extractGroupingNode" $ do
    it "should get whole cfg in grouping node if they share start and end" $ do
      let startNode = gbbn "start"
          midNode1 = gbbn "mid1"
          midNode2 = gbbn "mid2"
          endNode = gbbn "end"
          cfg = Cfg.mkCfg 0
            startNode
            [ midNode1
            , midNode2
            , endNode
            ]
            [ Cfg.CfEdge
              startNode
              midNode1
              TrueBranch
            , Cfg.CfEdge
              startNode
              midNode2
              FalseBranch
            , Cfg.CfEdge
              midNode2
              endNode
              UnconditionalBranch
            , Cfg.CfEdge
              midNode1
              endNode
              UnconditionalBranch
            ]
          expected = GroupingNode (getNodeId endNode) (Cfg.getNodeUUID startNode) cfg [""]

          innerGroupNodes = findNodesInGroup startNode endNode cfg
      extractGroupingNode startNode endNode innerGroupNodes cfg [""] `prettyShouldBe` expected

    it "should get the inner group but ignore the outer nodes" $ do
      let rootNode = gbbn "root"
          startNode = gbbn "start"
          midNode1 = gbbn "mid1"
          midNode2 = gbbn "mid2"
          endNode = gbbn "end"
          termNode = gbbn "term"

          outerCfg = Cfg.mkCfg 0
            rootNode
            [ startNode
            , midNode1
            , midNode2
            , endNode
            , termNode
            ]
            [ Cfg.CfEdge
              rootNode
              startNode
              UnconditionalBranch
            , Cfg.CfEdge
              startNode
              midNode1
              TrueBranch
            , Cfg.CfEdge
              startNode
              midNode2
              FalseBranch
            , Cfg.CfEdge
              midNode2
              endNode
              UnconditionalBranch
            , Cfg.CfEdge
              midNode1
              endNode
              UnconditionalBranch
            , Cfg.CfEdge
              endNode
              termNode
              UnconditionalBranch
            ]

          groupCfg = Cfg.mkCfg 0
            startNode
            [ midNode1
            , midNode2
            , endNode
            ]
            [ Cfg.CfEdge
              startNode
              midNode1
              TrueBranch
            , Cfg.CfEdge
              startNode
              midNode2
              FalseBranch
            , Cfg.CfEdge
              midNode2
              endNode
              UnconditionalBranch
            , Cfg.CfEdge
              midNode1
              endNode
              UnconditionalBranch
            ]
          expected = GroupingNode (getNodeId endNode) (Cfg.getNodeUUID startNode) groupCfg [""]

          innerGroupNodes = findNodesInGroup startNode endNode outerCfg
      extractGroupingNode startNode endNode innerGroupNodes outerCfg [""] `prettyShouldBe` expected

  context "makeGrouping" $ do
    it "should replace inner group and connect it to outer nodes" $ do
      let rootNode = gbbn "root"
          startNode = gbbn "start"
          midNode1 = gbbn "mid1"
          midNode2 = gbbn "mid2"
          endNode = gbbn "end"
          termNode = gbbn "term"

          outerCfg = Cfg.mkCfg 0
            rootNode
            [ startNode
            , midNode1
            , midNode2
            , endNode
            , termNode
            ]
            [ Cfg.CfEdge
              rootNode
              startNode
              UnconditionalBranch
            , Cfg.CfEdge
              startNode
              midNode1
              TrueBranch
            , Cfg.CfEdge
              startNode
              midNode2
              FalseBranch
            , Cfg.CfEdge
              midNode2
              endNode
              UnconditionalBranch
            , Cfg.CfEdge
              midNode1
              endNode
              UnconditionalBranch
            , Cfg.CfEdge
              endNode
              termNode
              UnconditionalBranch
            ]

          groupCfg = Cfg.mkCfg 0
            startNode
            [ midNode1
            , midNode2
            , endNode
            ]
            [ Cfg.CfEdge
              startNode
              midNode1
              TrueBranch
            , Cfg.CfEdge
              startNode
              midNode2
              FalseBranch
            , Cfg.CfEdge
              midNode2
              endNode
              UnconditionalBranch
            , Cfg.CfEdge
              midNode1
              endNode
              UnconditionalBranch
            ]

          gnode = Grouping $ GroupingNode (getNodeId endNode) (Cfg.getNodeUUID startNode) groupCfg [""]

          outerCfg' = Cfg.mkCfg 0
            rootNode
            [ gnode
            , termNode
            ]
            [ Cfg.CfEdge
              rootNode
              gnode
              UnconditionalBranch
            , Cfg.CfEdge
              gnode
              termNode
              UnconditionalBranch
            ]

          expected = outerCfg'

      makeGrouping startNode endNode outerCfg [""] `prettyShouldBe` expected

    it "should make group with self looping edge if end node loops to start node" $ do
      let rootNode = gbbn "root"
          startNode = gbbn "start"
          midNode1 = gbbn "mid1"
          midNode2 = gbbn "mid2"
          endNode = gbbn "end"
          termNode = gbbn "term"

          outerCfg = Cfg.mkCfg 0
            rootNode
            [ startNode
            , midNode1
            , midNode2
            , endNode
            , termNode
            ]
            [ Cfg.CfEdge
              rootNode
              startNode
              UnconditionalBranch
            , Cfg.CfEdge
              startNode
              midNode1
              TrueBranch
            , Cfg.CfEdge
              startNode
              midNode2
              FalseBranch
            , Cfg.CfEdge
              midNode2
              endNode
              UnconditionalBranch
            , Cfg.CfEdge
              midNode1
              endNode
              UnconditionalBranch
            , Cfg.CfEdge
              endNode
              termNode
              TrueBranch
            , Cfg.CfEdge
              endNode
              startNode
              FalseBranch
            ]

          groupCfg = Cfg.mkCfg 0
            startNode
            [ midNode1
            , midNode2
            , endNode
            ]
            [ Cfg.CfEdge
              startNode
              midNode1
              TrueBranch
            , Cfg.CfEdge
              startNode
              midNode2
              FalseBranch
            , Cfg.CfEdge
              midNode2
              endNode
              UnconditionalBranch
            , Cfg.CfEdge
              midNode1
              endNode
              UnconditionalBranch
            , Cfg.CfEdge
              endNode
              startNode
              FalseBranch
            ]

          gnode = Grouping $ GroupingNode (getNodeId endNode) (Cfg.getNodeUUID startNode) groupCfg [""]

          outerCfg' = Cfg.mkCfg 0
            rootNode
            [ gnode
            , termNode
            ]
            [ Cfg.CfEdge
              rootNode
              gnode
              UnconditionalBranch
            , Cfg.CfEdge
              gnode
              termNode
              TrueBranch
            ]

          expected = outerCfg'

      makeGrouping startNode endNode outerCfg [""] `prettyShouldBe` expected
