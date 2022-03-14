{- HLINT ignore "Redundant do" -}

module Blaze.Types.Cfg.GroupingSpec where

import Blaze.Function (Function (Function))
import Blaze.Types.Cfg
    ( BasicBlockNode(BasicBlockNode), BranchType(..) )
import qualified Blaze.Types.Cfg as Cfg
import qualified Blaze.Graph as G
import Blaze.Prelude
import Test.Hspec
import Blaze.Pretty (prettyPrint, pretty, PrettyShow(PrettyShow))
import Blaze.Types.Pil (Ctx(Ctx), CtxId(CtxId))
import Blaze.Util.Spec (mkUuid1, mkUuid2)
import qualified Blaze.Types.Cfg.Grouping as Grp
import qualified Data.HashSet as HashSet

ctx :: Ctx
ctx = Ctx func . CtxId $ mkUuid1 (0 :: Int)
  where
    func = Function Nothing "foo" 0x00 []

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
      Grp.findNodesInGroup startNode endNode cfg `shouldBe` expected

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

      Grp.findNodesInGroup startNode endNode cfg `shouldBe` expected

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

      Grp.findNodesInGroup startNode endNode cfg `shouldBe` expected

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
      Grp.extractGroupingNode startNode endNode innerGroupNodes cfg `shouldBe` expected

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
      Grp.extractGroupingNode startNode endNode innerGroupNodes outerCfg `shouldBe` expected

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

      PrettyShow (Grp.makeGrouping startNode endNode outerCfg) `shouldBe` PrettyShow expected
