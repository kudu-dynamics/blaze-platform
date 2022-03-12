{- HLINT ignore "Redundant do" -}

module Blaze.Types.Cfg.GroupingSpec where

import Blaze.Function (Function (Function))
import qualified Blaze.Types.Cfg as Cfg
import qualified Blaze.Graph as G
import Blaze.Types.Cfg (BranchType(..))
import Blaze.Prelude
import Test.Hspec
import Blaze.Pretty (prettyPrint, pretty, PrettyShow(PrettyShow))
import Blaze.Types.Pil (Ctx(Ctx), CtxId(CtxId))
import Blaze.Util.Spec (mkUuid1, mkUuid2)
import Blaze.Types.Cfg.Grouping
import qualified Data.HashSet as HashSet

ctx :: Ctx
ctx = Ctx func . CtxId $ mkUuid1 (0 :: Int)
  where
    func = Function Nothing "foo" 0x00 []

bb :: Ctx -> Address -> Address -> a -> CfNode a
bb ctx startAddr endAddr x =
  BasicBlock $ Cfg.BasicBlockNode ctx startAddr endAddr uuid x
 where
  uuid = mkUuid2 startAddr endAddr

bbn :: Text -> CfNode Text
bbn name = bb ctx x x name
  where
    x = fromIntegral $ hash name

spec :: Spec
spec = describe "Blaze.Types.Cfg.Grouping" $ do
  context "findNodesInGroup" $ do
    it "should return empty set if group is just start and end with no middle" $ do
      let cfg = mkCfg
            (bbn "root")
            [ bbn "mid"
            , bbn "end"
            ]
            [ CfEdge
              (bbn "root")
              (bbn "mid")
              UnconditionalBranch
            , CfEdge
              (bbn "mid")
              (bbn "end")
              UnconditionalBranch
            ]
          startNode = bbn "mid"
          endNode = bbn "end"
          expected = HashSet.empty
      findNodesInGroup startNode endNode cfg `shouldBe` expected

    it "should return single middle node" $ do
      let startNode = bbn "start"
          midNode = bbn "mid"
          endNode = bbn "end"
          cfg = mkCfg
            startNode
            [ midNode
            , endNode
            ]
            [ CfEdge
              startNode
              midNode
              UnconditionalBranch
            , CfEdge
              midNode
              endNode
              UnconditionalBranch
            ]
          expected = HashSet.singleton midNode

      findNodesInGroup startNode endNode cfg `shouldBe` expected

    it "should return two middle nodes" $ do
      let startNode = bbn "start"
          midNode1 = bbn "mid1"
          midNode2 = bbn "mid2"
          endNode = bbn "end"
          cfg = mkCfg
            startNode
            [ midNode1
            , midNode2
            , endNode
            ]
            [ CfEdge
              startNode
              midNode1
              TrueBranch
            , CfEdge
              startNode
              midNode2
              FalseBranch
            , CfEdge
              midNode2
              endNode
              UnconditionalBranch
            , CfEdge
              midNode1
              endNode
              UnconditionalBranch
            ]
          expected = HashSet.fromList [midNode1, midNode2]

      findNodesInGroup startNode endNode cfg `shouldBe` expected

  context "extractGroupingNode" $ do
    it "should get whole cfg in grouping node if they share start and end" $ do
      let startNode = bbn "start"
          midNode1 = bbn "mid1"
          midNode2 = bbn "mid2"
          endNode = bbn "end"
          cfg = mkCfg
            startNode
            [ midNode1
            , midNode2
            , endNode
            ]
            [ CfEdge
              startNode
              midNode1
              TrueBranch
            , CfEdge
              startNode
              midNode2
              FalseBranch
            , CfEdge
              midNode2
              endNode
              UnconditionalBranch
            , CfEdge
              midNode1
              endNode
              UnconditionalBranch
            ]
          expected = GroupingNode endNode (getNodeUUID startNode) cfg

          innerGroupNodes = findNodesInGroup startNode endNode cfg
      extractGroupingNode startNode endNode innerGroupNodes cfg `shouldBe` expected

    it "should get the inner group but ignore the outer nodes" $ do
      let rootNode = bbn "root"
          startNode = bbn "start"
          midNode1 = bbn "mid1"
          midNode2 = bbn "mid2"
          endNode = bbn "end"
          termNode = bbn "term"
            
          outerCfg = mkCfg
            rootNode
            [ startNode
            , midNode1
            , midNode2
            , endNode
            , termNode
            ]
            [ CfEdge
              rootNode
              startNode
              UnconditionalBranch
            , CfEdge
              startNode
              midNode1
              TrueBranch
            , CfEdge
              startNode
              midNode2
              FalseBranch
            , CfEdge
              midNode2
              endNode
              UnconditionalBranch
            , CfEdge
              midNode1
              endNode
              UnconditionalBranch
            , CfEdge
              endNode
              termNode
              UnconditionalBranch
            ]

          groupCfg = mkCfg
            startNode
            [ midNode1
            , midNode2
            , endNode
            ]
            [ CfEdge
              startNode
              midNode1
              TrueBranch
            , CfEdge
              startNode
              midNode2
              FalseBranch
            , CfEdge
              midNode2
              endNode
              UnconditionalBranch
            , CfEdge
              midNode1
              endNode
              UnconditionalBranch
            ]
          expected = GroupingNode endNode (getNodeUUID startNode) groupCfg

          innerGroupNodes = findNodesInGroup startNode endNode outerCfg
      extractGroupingNode startNode endNode innerGroupNodes outerCfg `shouldBe` expected

  context "makeGrouping" $ do
    it "should replace inner group and connect it to outer nodes" $ do
      let rootNode = bbn "root"
          startNode = bbn "start"
          midNode1 = bbn "mid1"
          midNode2 = bbn "mid2"
          endNode = bbn "end"
          termNode = bbn "term"
            
          outerCfg = mkCfg
            rootNode
            [ startNode
            , midNode1
            , midNode2
            , endNode
            , termNode
            ]
            [ CfEdge
              rootNode
              startNode
              UnconditionalBranch
            , CfEdge
              startNode
              midNode1
              TrueBranch
            , CfEdge
              startNode
              midNode2
              FalseBranch
            , CfEdge
              midNode2
              endNode
              UnconditionalBranch
            , CfEdge
              midNode1
              endNode
              UnconditionalBranch
            , CfEdge
              endNode
              termNode
              UnconditionalBranch
            ]

          groupCfg = mkCfg
            startNode
            [ midNode1
            , midNode2
            , endNode
            ]
            [ CfEdge
              startNode
              midNode1
              TrueBranch
            , CfEdge
              startNode
              midNode2
              FalseBranch
            , CfEdge
              midNode2
              endNode
              UnconditionalBranch
            , CfEdge
              midNode1
              endNode
              UnconditionalBranch
            ]
            
          gnode = Grouping $ GroupingNode endNode (getNodeUUID startNode) groupCfg

          outerCfg' = mkCfg
            rootNode
            [ gnode
            , termNode
            ]
            [ CfEdge
              rootNode
              gnode
              UnconditionalBranch
            , CfEdge
              gnode
              termNode
              UnconditionalBranch
            ]
            
          expected = outerCfg'

      PrettyShow (makeGrouping startNode endNode outerCfg) `shouldBe` PrettyShow expected

