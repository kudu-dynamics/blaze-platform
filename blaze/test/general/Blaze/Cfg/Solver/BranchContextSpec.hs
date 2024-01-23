{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Evaluate" -}

module Blaze.Cfg.Solver.BranchContextSpec where

import Blaze.Prelude hiding (Constraint, Symbol, Type, bitSize, const, sym)

import Blaze.Cfg hiding (
  BasicBlockNode (ctx),
  CallNode (ctx),
  func,
 )
import Blaze.Cfg qualified as Cfg
import Blaze.Cfg.Solver.BranchContext qualified as BC
import Blaze.Function (Function (Function))
import Blaze.Function qualified as Func
import Blaze.Graph qualified as G
import Blaze.Pil.Construct
import Blaze.Pretty (PrettyShow' (PrettyShow'))
import Blaze.Types.Graph.EdgeGraph (EdgeGraph, EdgeGraphNode (NodeNode))
import Blaze.Types.Graph.EdgeGraph qualified as Eg
import Blaze.Types.Pil (Ctx (Ctx), Stmt)
import Blaze.Util.Spec (mkDummyCtx, mkDummyTermNode, mkUuid1)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Test.Hspec


bbp :: Ctx -> Text -> [Stmt] -> CfNode [Stmt]
bbp ctx name stmts = BasicBlock $ BasicBlockNode
  { ctx = ctx
  , start = 0
  , end = 0
  , uuid = uuid'
  , nodeData = stmts
  }
  where
    uuid' = mkUuid1 . hash $ name

bbpn :: Int -> Ctx -> Text -> [Stmt] -> CfNode [Stmt]
bbpn n ctx name stmts = BasicBlock $ BasicBlockNode
  { ctx = ctx
  , start = fromIntegral n
  , end = fromIntegral n
  , uuid = uuid'
  , nodeData = stmts
  }
  where
    uuid' = mkUuid1 . hash $ name

callerFunc :: Function
callerFunc = Function
  { symbol = Nothing
  , name = "caller"
  , address = 0
  , params = []
  }

targetFunc :: Function
targetFunc = Function
  { symbol = Nothing
  , name = "targetFunc"
  , address = 100
  , params = [ Func.FuncParamInfo $ Func.ParamInfo "arg1" Func.In
             , Func.FuncParamInfo $ Func.ParamInfo "arg2" Func.In
             ]
  }

callerCtx :: Ctx
callerCtx = Ctx callerFunc 0

targetCtx :: Ctx
targetCtx = Ctx targetFunc 1

spec :: Spec
spec = describe "Blaze.Cfg.Solver.BranchContext" $ do
  let dummyCtx = mkDummyCtx 0
      dummyTermNode = NodeNode $ mkDummyTermNode dummyCtx []
      dummyTermEdgeType = ()

  context "Edge Dominators" $ do
    it "should create an empty edge Dominators map for graph with single node" $ do
      let rootNode = bbp callerCtx "root"
                     [ branchCond $ cmpE (var "x" 4) (const 0 4) 4 ]

          cfg :: Cfg (CfNode [Stmt])
          cfg = mkCfg 0 rootNode [] []

          edgeGraph :: EdgeGraph PilNode
          edgeGraph = Eg.toEdgeGraph $ cfg ^. #graph

          eRoot = NodeNode $ Cfg.getRootNode cfg
          edgeDoms = BC.filterEdges $ G.getDominators eRoot edgeGraph
          expectedEdgeDom = G.Dominators . HashMap.fromList $ []

      PrettyShow' edgeDoms `shouldBe` PrettyShow' expectedEdgeDom

    it "should create a proper edge Dominators map for linear CFG" $ do
      let rootNode = bbp callerCtx "root"
                     [ branchCond $ cmpE (var "x" 4) (const 0 4) 4 ]
          falseNode1 = bbp callerCtx "falseNode1"
                       [ def "c1" $ const 1 4
                       , branchCond $ cmpE (var "x" 4) (const 0 4) 4
                       ]
          trueNode2 = bbp callerCtx "trueNode2"
                      [ def "c3" $ const 3 4 ]

          endNode = bbp callerCtx "endNode"
                    [ defPhi 4 "c" ["c1", "c2", "c3"]
                    ]

          cfg = mkCfg 0 rootNode [ falseNode1
                                 , trueNode2
                                 , endNode
                                 ]
                [ CfEdge rootNode falseNode1 Cfg.FalseBranch
                , CfEdge falseNode1 trueNode2 Cfg.TrueBranch
                , CfEdge trueNode2 endNode Cfg.UnconditionalBranch
                ]

          edgeGraph = Eg.toEdgeGraph $ cfg ^. #graph :: EdgeGraph PilNode
          eRoot = NodeNode $ Cfg.getRootNode cfg
          edgeDoms = BC.filterEdges $ G.getDominators eRoot edgeGraph

          edge1 = CfEdge rootNode falseNode1 Cfg.FalseBranch
          edge2 = CfEdge falseNode1 trueNode2 Cfg.TrueBranch
          edge3 = CfEdge trueNode2 endNode Cfg.UnconditionalBranch

          expectedEdgeDom = G.Dominators . HashMap.fromList $
            [ (edge2, HashSet.fromList [edge1])
            , (edge3, HashSet.fromList [edge1, edge2])
            ]

      PrettyShow' edgeDoms `shouldBe` PrettyShow' expectedEdgeDom

    it "should create a proper edge Dominators map for complex CFG" $ do
      let rootNode = bbp callerCtx "root"
                     [ branchCond $ cmpE (var "x" 4) (const 0 4) 4 ]
          falseNode1 = bbp callerCtx "falseNode1"
                       [ def "c1" $ const 1 4
                       , branchCond $ cmpE (var "x" 4) (const 0 4) 4
                       ]
          falseNode2 = bbp callerCtx "falseNode2"
                       [ def "c2" $ const 2 4 ]
          trueNode2 = bbp callerCtx "trueNode2"
                      [ def "c3" $ const 3 4 ]

          endNode = bbp callerCtx "endNode"
                    [ defPhi 4 "c" ["c1", "c2", "c3"]
                    ]

          cfg = mkCfg 0 rootNode [ falseNode1
                                 , trueNode2
                                 , falseNode2
                                 , endNode
                                 ]
                [ CfEdge rootNode falseNode1 Cfg.FalseBranch
                , CfEdge falseNode1 falseNode2 Cfg.FalseBranch
                , CfEdge falseNode1 trueNode2 Cfg.TrueBranch
                , CfEdge falseNode2 endNode Cfg.UnconditionalBranch
                , CfEdge trueNode2 endNode Cfg.UnconditionalBranch
                , CfEdge rootNode endNode Cfg.TrueBranch
                ]

          edgeGraph = Eg.toEdgeGraph $ cfg ^. #graph :: EdgeGraph PilNode
          eRoot = NodeNode $ Cfg.getRootNode cfg
          edgeDoms = BC.filterEdges $ G.getDominators eRoot edgeGraph

          rootToFalse1 = CfEdge rootNode falseNode1 Cfg.FalseBranch
          false1ToFalse2 = CfEdge falseNode1 falseNode2 Cfg.FalseBranch
          false1ToTrue2 = CfEdge falseNode1 trueNode2 Cfg.TrueBranch
          false2ToEnd = CfEdge falseNode2 endNode Cfg.UnconditionalBranch
          true2ToEnd = CfEdge trueNode2 endNode Cfg.UnconditionalBranch

          expectedEdgeDom = G.Dominators . HashMap.fromList $
            [ (false1ToFalse2, HashSet.fromList [rootToFalse1])
            , (false1ToTrue2, HashSet.fromList [rootToFalse1])
            , (false2ToEnd, HashSet.fromList [rootToFalse1, false1ToFalse2])
            , (true2ToEnd, HashSet.fromList [rootToFalse1, false1ToTrue2])
            ]

      PrettyShow' edgeDoms `shouldBe` PrettyShow' expectedEdgeDom

  context "Edge PostDominators" $ do
    it "should create an empty edge PostDominators map for graph with single node" $ do
      let rootNode = bbp callerCtx "root"
                     [ branchCond $ cmpE (var "x" 4) (const 0 4) 4 ]

          cfg = mkCfg 0 rootNode [] []

          edgeGraph :: EdgeGraph PilNode
          edgeGraph = Eg.toEdgeGraph $ cfg ^. #graph
          edgeDoms = BC.filterEdges $ G.getPostDominators dummyTermNode dummyTermEdgeType edgeGraph

          expectedEdgeDom = G.PostDominators . HashMap.fromList $ []

      PrettyShow' edgeDoms `shouldBe` PrettyShow' expectedEdgeDom

    it "should create a proper edge PostDominators map for linear CFG" $ do
      let rootNode = bbp callerCtx "root"
                     [ branchCond $ cmpE (var "x" 4) (const 0 4) 4 ]
          falseNode1 = bbp callerCtx "falseNode1"
                       [ def "c1" $ const 1 4
                       , branchCond $ cmpE (var "x" 4) (const 0 4) 4
                       ]
          trueNode2 = bbp callerCtx "trueNode2"
                      [ def "c3" $ const 3 4 ]

          endNode = bbp callerCtx "endNode"
                    [ defPhi 4 "c" ["c1", "c2", "c3"]
                    ]

          cfg = mkCfg 0 rootNode [ falseNode1
                                 , trueNode2
                                 , endNode
                                 ]
                [ CfEdge rootNode falseNode1 Cfg.FalseBranch
                , CfEdge falseNode1 trueNode2 Cfg.TrueBranch
                , CfEdge trueNode2 endNode Cfg.UnconditionalBranch
                ]

          edgeGraph = Eg.toEdgeGraph $ cfg ^. #graph :: EdgeGraph PilNode

          edgeDoms = BC.filterEdges $ G.getPostDominators dummyTermNode dummyTermEdgeType edgeGraph

          edge1 = CfEdge rootNode falseNode1 Cfg.FalseBranch
          edge2 = CfEdge falseNode1 trueNode2 Cfg.TrueBranch
          edge3 = CfEdge trueNode2 endNode Cfg.UnconditionalBranch

          expectedEdgeDom = G.PostDominators . HashMap.fromList $
            [ (edge1, HashSet.fromList [edge2, edge3])
            , (edge2, HashSet.fromList [edge3])
            ]

      PrettyShow' edgeDoms `shouldBe` PrettyShow' expectedEdgeDom


    it "should create a proper edge Dominators map for complex CFG" $ do
      let rootNode = bbpn 0 callerCtx "root"
                     [ branchCond $ cmpE (var "x" 4) (const 0 4) 4 ]
          trueNode1 = bbpn 1 callerCtx "trueNode1"
                       [ def "c1" $ const 1 4 ]

          falseNode1 = bbpn 2 callerCtx "falseNode1" []

          ifNode2 = bbpn 3 callerCtx "ifNode2"
                    [ branchCond $ cmpNE (var "x" 4) (const 0 4) 4 ]

          falseNode2 = bbpn 4 callerCtx "falseNode2"
                       [ def "c2" $ const 2 4 ]
          trueNode2 = bbpn 5 callerCtx "trueNode2"
                      [ def "c3" $ const 3 4 ]

          joinNode2 = bbpn 6 callerCtx "joinNode2"
                      [ defPhi 4 "c4" ["c2", "c3"] ]

          endNode = bbpn 7 callerCtx "endNode"
                    [ defPhi 4 "c" ["c1", "c4"]
                    ]

          cfg = mkCfg 0 rootNode [ falseNode1
                                 , trueNode1
                                 , ifNode2
                                 , trueNode2
                                 , falseNode2
                                 , joinNode2
                                 , endNode
                                 ]
                [ CfEdge rootNode falseNode1 Cfg.FalseBranch
                , CfEdge rootNode trueNode1 Cfg.TrueBranch
                , CfEdge falseNode1 ifNode2 Cfg.UnconditionalBranch
                , CfEdge ifNode2 falseNode2 Cfg.FalseBranch
                , CfEdge ifNode2 trueNode2 Cfg.TrueBranch
                , CfEdge falseNode2 joinNode2 Cfg.UnconditionalBranch
                , CfEdge trueNode2 joinNode2 Cfg.UnconditionalBranch
                , CfEdge joinNode2 endNode Cfg.UnconditionalBranch
                , CfEdge trueNode1 endNode Cfg.UnconditionalBranch
                ]

          edgeGraph = Eg.toEdgeGraph $ cfg ^. #graph :: EdgeGraph PilNode
          edgeDoms = BC.filterEdges $ G.getPostDominators dummyTermNode dummyTermEdgeType edgeGraph

          rootToFalse1 = CfEdge rootNode falseNode1 Cfg.FalseBranch
          rootToTrue1 = CfEdge rootNode trueNode1 Cfg.TrueBranch
          false1ToIf2 = CfEdge falseNode1 ifNode2 Cfg.UnconditionalBranch
          if2ToFalse2 = CfEdge ifNode2 falseNode2 Cfg.FalseBranch
          if2ToTrue2 = CfEdge ifNode2 trueNode2 Cfg.TrueBranch
          false2ToJoin2 = CfEdge falseNode2 joinNode2 Cfg.UnconditionalBranch
          true2ToJoin2 = CfEdge trueNode2 joinNode2 Cfg.UnconditionalBranch
          join2ToEnd = CfEdge joinNode2 endNode Cfg.UnconditionalBranch
          true1ToEnd = CfEdge trueNode1 endNode Cfg.UnconditionalBranch

          expectedEdgeDom = G.PostDominators . HashMap.fromList $
            [ (rootToFalse1, HashSet.fromList [false1ToIf2, join2ToEnd])
            , (false1ToIf2, HashSet.fromList [join2ToEnd])
            , (if2ToFalse2, HashSet.fromList [false2ToJoin2, join2ToEnd])
            , (if2ToTrue2, HashSet.fromList [true2ToJoin2, join2ToEnd])
            , (true2ToJoin2, HashSet.fromList [join2ToEnd])
            , (false2ToJoin2, HashSet.fromList [join2ToEnd])
            , (rootToTrue1, HashSet.fromList [true1ToEnd])
            ]

          getPostDomCounts (G.PostDominators m) = HashSet.size <$> m

      PrettyShow' (getPostDomCounts edgeDoms) `shouldBe` PrettyShow' (getPostDomCounts expectedEdgeDom)
      PrettyShow' edgeDoms `shouldBe` PrettyShow' expectedEdgeDom

  context "getUnsatBranches" $ do
    it "should remove inconsistent child constraint of pruned parent" $ do
      let rootNode = bbpn 0 callerCtx "root"
                     [ branchCond $ cmpE (var "x" 4) (const 0 4) 4 ]

          ifNode = bbpn 1 callerCtx "ifNode"
                    [ branchCond $ cmpE (var "x" 4) (const 0 4) 4 ]

          falseNode = bbpn 2 callerCtx "falseNode" [ nop ]

          trueNode = bbpn 3 callerCtx "trueNode" [ nop ]

          endNode = bbpn 4 callerCtx "endNode" [ nop ]

          cfg = mkCfg 0 rootNode [ ifNode
                                 , falseNode
                                 , trueNode
                                 , endNode
                                 ]
                [ CfEdge rootNode ifNode Cfg.FalseBranch
                , CfEdge ifNode trueNode Cfg.TrueBranch
                , CfEdge ifNode falseNode Cfg.FalseBranch
                , CfEdge trueNode endNode Cfg.UnconditionalBranch
                , CfEdge falseNode endNode Cfg.UnconditionalBranch
                ]

          expectedRemoved = [CfEdge ifNode trueNode Cfg.TrueBranch]

      r <- BC.getUnsatBranches cfg

      (PrettyShow' . snd <$> r) `shouldBe` (PrettyShow' <$> Right expectedRemoved)

    it "should work around a type error" $ do
      let rootNode = bbpn 0 callerCtx "root"
                     [ def "x" $ const 0 4
                     , def "y" $ const 0 4
                     , def "z" $ var "y" 8
                     , branchCond $ cmpNE (var "x" 4) (const 0 4) 4
                     ]

          falseNode = bbpn 2 callerCtx "falseNode" [ nop ]

          trueNode = bbpn 3 callerCtx "trueNode" [ nop ]

          endNode = bbpn 4 callerCtx "endNode" [ nop ]

          cfg = mkCfg 0 rootNode [ falseNode
                                 , trueNode
                                 , endNode
                                 ]
                [ CfEdge rootNode trueNode Cfg.TrueBranch
                , CfEdge rootNode falseNode Cfg.FalseBranch
                , CfEdge trueNode endNode Cfg.UnconditionalBranch
                , CfEdge falseNode endNode Cfg.UnconditionalBranch
                ]

          expectedRemoved = [CfEdge rootNode trueNode Cfg.TrueBranch]

      r <- BC.getUnsatBranches cfg

      (PrettyShow' . snd <$> r) `shouldBe` (PrettyShow' <$> Right  expectedRemoved)
