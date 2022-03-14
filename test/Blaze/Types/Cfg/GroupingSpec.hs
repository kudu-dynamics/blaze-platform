{- HLINT ignore "Redundant do" -}

module Blaze.Types.Cfg.GroupingSpec where

import Blaze.Function (Function (Function))
import Blaze.Types.Cfg
    ( BasicBlockNode(BasicBlockNode), BranchType(..) )
import qualified Blaze.Types.Cfg as Cfg
import Blaze.Prelude
import Test.Hspec
import Blaze.Pretty (PrettyShow(PrettyShow))
import Blaze.Types.Pil (Ctx(Ctx), CtxId(CtxId))
import Blaze.Util.Spec (mkUuid1, mkUuid2)
import qualified Blaze.Types.Cfg.Grouping as Grp
import qualified Data.HashSet as HashSet

ctx :: Ctx
ctx = Ctx func . CtxId $ mkUuid1 (0 :: Int)
  where
    func = Function Nothing "foo" 0x00 []

groupbb :: Grp.CfNode a -> Grp.Cfg a -> Grp.CfNode a
groupbb end subcfg =
  Grp.Grouping
    Grp.GroupingNode
      { termNode = end
      , uuid = Grp.getNodeUUID (subcfg ^. #root)
      , grouping = subcfg
      }

gbb :: Address -> Address -> a -> Grp.CfNode a
cbb :: Address -> Address -> a -> Cfg.CfNode a
(gbb, cbb) = (go Grp.BasicBlock, go Cfg.BasicBlock)
  where
    go f startAddr endAddr x = f $ BasicBlockNode
      { ctx = Ctx (Function Nothing "f" 0 []) (CtxId $ mkUuid1 (1 :: Int))
      , start = startAddr
      , end = endAddr
      , uuid = uuid'
      , nodeData = x
      }
      where
        uuid' = mkUuid2 startAddr endAddr

gbbn :: Text -> Grp.CfNode Text
cbbn :: Text -> Cfg.CfNode Text
(gbbn, cbbn) = (go gbb, go cbb)
  where
    go f n = let x = fromIntegral $ hash n in f x x n

spec :: Spec
spec = describe "Blaze.Types.Cfg.Grouping" $ do
  let prettyShouldBe x y = PrettyShow x `shouldBe` PrettyShow y
  let cDoubleDiamond =
        Cfg.mkCfg
          (cbbn "root")
          [ cbbn "mid1"
          , cbbn "mid2"
          , cbbn "end"
          ]
          [ Cfg.CfEdge (cbbn "root") (cbbn "mid1") TrueBranch
          , Cfg.CfEdge (cbbn "root") (cbbn "mid2") FalseBranch
          , Cfg.CfEdge (cbbn "mid1") (cbbn "end") UnconditionalBranch
          , Cfg.CfEdge (cbbn "mid2") (cbbn "end") UnconditionalBranch
          ]
      gDoubleDiamond =
        Grp.mkCfg
          (gbbn "root")
          [ gbbn "mid1"
          , gbbn "mid2"
          , gbbn "end"
          ]
          [ Grp.CfEdge (gbbn "root") (gbbn "mid1") TrueBranch
          , Grp.CfEdge (gbbn "root") (gbbn "mid2") FalseBranch
          , Grp.CfEdge (gbbn "mid1") (gbbn "end") UnconditionalBranch
          , Grp.CfEdge (gbbn "mid2") (gbbn "end") UnconditionalBranch
          ]
      cUngrouped =
        Cfg.mkCfg
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
      gGrouped =
        let mid111group = groupbb (gbbn "mid111") (Grp.mkCfg (gbbn "mid111") [] [])
            mid111groupgroup = groupbb mid111group (Grp.mkCfg mid111group [] [])
            branch11group =
              groupbb (gbbn "end11") $
                Grp.mkCfg
                  (gbbn "branch11")
                  [ mid111groupgroup
                  , gbbn "mid112"
                  , gbbn "end11"
                  ]
                  [ Grp.CfEdge (gbbn "branch11") mid111groupgroup TrueBranch
                  , Grp.CfEdge (gbbn "branch11") (gbbn "mid112") FalseBranch
                  , Grp.CfEdge mid111groupgroup (gbbn "end11") UnconditionalBranch
                  , Grp.CfEdge (gbbn "mid112") (gbbn "end11") UnconditionalBranch
                  ]
            branch1group =
              groupbb (gbbn "end1") $
                Grp.mkCfg
                  (gbbn "branch1")
                  [ branch11group
                  , gbbn "mid12"
                  , gbbn "end1"
                  ]
                  [ Grp.CfEdge (gbbn "branch1") branch11group TrueBranch
                  , Grp.CfEdge (gbbn "branch1") (gbbn "mid12") FalseBranch
                  , Grp.CfEdge branch11group (gbbn "end1") UnconditionalBranch
                  , Grp.CfEdge (gbbn "mid12") (gbbn "end1") UnconditionalBranch
                  ]
        in
          Grp.mkCfg
            (gbbn "root")
            [ branch1group
            , gbbn "end"
            ]
            [ Grp.CfEdge (gbbn "root") branch1group UnconditionalBranch
            , Grp.CfEdge branch1group (gbbn "end") UnconditionalBranch
            ]

  context "fromCfg" $ do
    it "should convert a flat CFG to a grouped CFG" $ do
      Grp.fromCfg cDoubleDiamond `prettyShouldBe` gDoubleDiamond
  context "unfoldGroups" $ do
    it "should unfold all groups of a grouped CFG into a flat CFG" $ do
      Grp.unfoldGroups gGrouped `prettyShouldBe` cUngrouped
  context "findNodesInGroup" $ do
    it "should return empty set if group is just start and end with no middle" $ do
      let cfg = Grp.mkCfg
            (gbbn "root")
            [ gbbn "mid"
            , gbbn "end"
            ]
            [ Grp.CfEdge
              (gbbn "root")
              (gbbn "mid")
              UnconditionalBranch
            , Grp.CfEdge
              (gbbn "mid")
              (gbbn "end")
              UnconditionalBranch
            ]
          startNode = gbbn "mid"
          endNode = gbbn "end"
          expected = HashSet.empty
      Grp.findNodesInGroup startNode endNode cfg `prettyShouldBe` expected

    it "should return single middle node" $ do
      let startNode = gbbn "start"
          midNode = gbbn "mid"
          endNode = gbbn "end"
          cfg = Grp.mkCfg
            startNode
            [ midNode
            , endNode
            ]
            [ Grp.CfEdge
              startNode
              midNode
              UnconditionalBranch
            , Grp.CfEdge
              midNode
              endNode
              UnconditionalBranch
            ]
          expected = HashSet.singleton midNode

      Grp.findNodesInGroup startNode endNode cfg `prettyShouldBe` expected

    it "should return two middle nodes" $ do
      let startNode = gbbn "start"
          midNode1 = gbbn "mid1"
          midNode2 = gbbn "mid2"
          endNode = gbbn "end"
          cfg = Grp.mkCfg
            startNode
            [ midNode1
            , midNode2
            , endNode
            ]
            [ Grp.CfEdge
              startNode
              midNode1
              TrueBranch
            , Grp.CfEdge
              startNode
              midNode2
              FalseBranch
            , Grp.CfEdge
              midNode2
              endNode
              UnconditionalBranch
            , Grp.CfEdge
              midNode1
              endNode
              UnconditionalBranch
            ]
          expected = HashSet.fromList [midNode1, midNode2]

      Grp.findNodesInGroup startNode endNode cfg `prettyShouldBe` expected

  context "Grp.extractGroupingNode" $ do
    it "should get whole cfg in grouping node if they share start and end" $ do
      let startNode = gbbn "start"
          midNode1 = gbbn "mid1"
          midNode2 = gbbn "mid2"
          endNode = gbbn "end"
          cfg = Grp.mkCfg
            startNode
            [ midNode1
            , midNode2
            , endNode
            ]
            [ Grp.CfEdge
              startNode
              midNode1
              TrueBranch
            , Grp.CfEdge
              startNode
              midNode2
              FalseBranch
            , Grp.CfEdge
              midNode2
              endNode
              UnconditionalBranch
            , Grp.CfEdge
              midNode1
              endNode
              UnconditionalBranch
            ]
          expected = Grp.GroupingNode endNode (Grp.getNodeUUID startNode) cfg

          innerGroupNodes = Grp.findNodesInGroup startNode endNode cfg
      Grp.extractGroupingNode startNode endNode innerGroupNodes cfg `prettyShouldBe` expected

    it "should get the inner group but ignore the outer nodes" $ do
      let rootNode = gbbn "root"
          startNode = gbbn "start"
          midNode1 = gbbn "mid1"
          midNode2 = gbbn "mid2"
          endNode = gbbn "end"
          termNode = gbbn "term"

          outerCfg = Grp.mkCfg
            rootNode
            [ startNode
            , midNode1
            , midNode2
            , endNode
            , termNode
            ]
            [ Grp.CfEdge
              rootNode
              startNode
              UnconditionalBranch
            , Grp.CfEdge
              startNode
              midNode1
              TrueBranch
            , Grp.CfEdge
              startNode
              midNode2
              FalseBranch
            , Grp.CfEdge
              midNode2
              endNode
              UnconditionalBranch
            , Grp.CfEdge
              midNode1
              endNode
              UnconditionalBranch
            , Grp.CfEdge
              endNode
              termNode
              UnconditionalBranch
            ]

          groupCfg = Grp.mkCfg
            startNode
            [ midNode1
            , midNode2
            , endNode
            ]
            [ Grp.CfEdge
              startNode
              midNode1
              TrueBranch
            , Grp.CfEdge
              startNode
              midNode2
              FalseBranch
            , Grp.CfEdge
              midNode2
              endNode
              UnconditionalBranch
            , Grp.CfEdge
              midNode1
              endNode
              UnconditionalBranch
            ]
          expected = Grp.GroupingNode endNode (Grp.getNodeUUID startNode) groupCfg

          innerGroupNodes = Grp.findNodesInGroup startNode endNode outerCfg
      Grp.extractGroupingNode startNode endNode innerGroupNodes outerCfg `prettyShouldBe` expected

  context "makeGrouping" $ do
    it "should replace inner group and connect it to outer nodes" $ do
      let rootNode = gbbn "root"
          startNode = gbbn "start"
          midNode1 = gbbn "mid1"
          midNode2 = gbbn "mid2"
          endNode = gbbn "end"
          termNode = gbbn "term"

          outerCfg = Grp.mkCfg
            rootNode
            [ startNode
            , midNode1
            , midNode2
            , endNode
            , termNode
            ]
            [ Grp.CfEdge
              rootNode
              startNode
              UnconditionalBranch
            , Grp.CfEdge
              startNode
              midNode1
              TrueBranch
            , Grp.CfEdge
              startNode
              midNode2
              FalseBranch
            , Grp.CfEdge
              midNode2
              endNode
              UnconditionalBranch
            , Grp.CfEdge
              midNode1
              endNode
              UnconditionalBranch
            , Grp.CfEdge
              endNode
              termNode
              UnconditionalBranch
            ]

          groupCfg = Grp.mkCfg
            startNode
            [ midNode1
            , midNode2
            , endNode
            ]
            [ Grp.CfEdge
              startNode
              midNode1
              TrueBranch
            , Grp.CfEdge
              startNode
              midNode2
              FalseBranch
            , Grp.CfEdge
              midNode2
              endNode
              UnconditionalBranch
            , Grp.CfEdge
              midNode1
              endNode
              UnconditionalBranch
            ]

          gnode = Grp.Grouping $ Grp.GroupingNode endNode (Grp.getNodeUUID startNode) groupCfg

          outerCfg' = Grp.mkCfg
            rootNode
            [ gnode
            , termNode
            ]
            [ Grp.CfEdge
              rootNode
              gnode
              UnconditionalBranch
            , Grp.CfEdge
              gnode
              termNode
              UnconditionalBranch
            ]

          expected = outerCfg'

      Grp.makeGrouping startNode endNode outerCfg `prettyShouldBe` expected
