{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

module Blaze.Cfg.Solver.BranchContextSpec where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint, const, Symbol)
import qualified Prelude as P

import Blaze.Cfg hiding
  ( BasicBlockNode (ctx)
  , CallNode (ctx)
  , func
  )
import qualified Blaze.Cfg as Cfg
import Blaze.Cfg (asIdNode)
import Blaze.Function (Function (Function))
import qualified Blaze.Function as Func
import qualified Blaze.Pil.Construct as C
import Blaze.Types.Pil (Ctx (Ctx), CtxId (CtxId), Symbol)
import qualified Blaze.Types.Pil as Pil
import Blaze.Util.Spec (mkUuid1)
import qualified Data.HashMap.Strict as HashMap
import Blaze.Cfg.Checker
import Blaze.Pretty (PrettyShow(PrettyShow))
import Blaze.Pil.Construct
import qualified Data.SBV.Trans as SBV
import Blaze.Cfg.Solver.General (generalCfgFormula, getUnsatBranches, simplify)
import Data.SBV.Dynamic as D hiding (Solver, name)
import Blaze.Types.Pil.Solver (checkSatWith, SolverCtx(SolverCtx))
import qualified Blaze.Types.Pil.Solver as PilSolver
import qualified Blaze.Pil.Solver as PilSolver
import qualified Blaze.Cfg.Analysis as CfgA
import qualified Blaze.Cfg.Solver.BranchContext as BC
import qualified Blaze.Graph as G
import Blaze.Types.Graph.Alga (AlgaGraph)
import qualified Data.HashSet as HashSet
import Test.Hspec


bbp :: Ctx -> Text -> [Pil.Stmt] -> CfNode [Pil.Stmt]
bbp ctx name stmts = BasicBlock $ BasicBlockNode
  { ctx = ctx
  , start = 0
  , end = 0
  , uuid = uuid'
  , nodeData = stmts
  }
  where
    uuid' = mkUuid1 . hash $ name

pilCall :: Symbol -> Function -> [Pil.Expression] -> Pil.Stmt
pilCall varSym func args =
  C.defCall varSym (Pil.CallFunc func) args 8

mkCallNode :: Ctx -> Text -> Symbol -> Function -> [Pil.Expression] -> (Cfg.CallNode [Pil.Stmt], Pil.Stmt)
mkCallNode ctx name retVarSym targetFunc' args =
  ( CallNode
    { ctx = ctx
    , start = 0
    , callDest = Pil.CallFunc targetFunc'
    , uuid = uuid'
    , nodeData = [callStmt']
    }
  , callStmt'
  )
  where
    callStmt' = pilCall retVarSym targetFunc' args
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
callerCtx = Ctx callerFunc . CtxId $ mkUuid1 (1 :: Int)

targetCtx :: Ctx
targetCtx = Ctx targetFunc . CtxId $ mkUuid1 (2 :: Int)

spec :: Spec
spec = describe "Blaze.Cfg.Solver.BranchContext" $ do
  -- context "unsatBranches" $ do
  --   let solve :: Cfg [Pil.Stmt] -> IO [CfEdge ()]
  --       solve cfg = getUnsatBranches cfg >>= either (P.error . show) return

  --   it "should find single unsat branch" $ do
  --     let rootNode = bbp callerCtx "root"
  --                    [ branchCond $ cmpE (var "a" 4) (const 800 4) 4 ]
  --         falseNode = bbp callerCtx "falseNode"
  --                    [ def "b1" $ const 200 4 ]
  --         trueNode = bbp callerCtx "trueNode"
  --                    [ def "b2" $ const 400 4 ]
  --         endNode = bbp callerCtx "endNode"
  --                   [ defPhi "b" ["b1", "b2"]
  --                   , constraint $ cmpE (var "a" 4) (const 900 4) 4
  --                   ]

  --         cfg = mkCfg rootNode [ trueNode, falseNode, endNode ]
  --               [ CfEdge rootNode falseNode Cfg.FalseBranch
  --               , CfEdge rootNode trueNode Cfg.TrueBranch
  --               , CfEdge falseNode endNode Cfg.UnconditionalBranch
  --               , CfEdge trueNode endNode Cfg.UnconditionalBranch
  --               ]
  --         r = Cfg.asIdEdge <$> [CfEdge rootNode trueNode Cfg.TrueBranch]

  --     r' <- solve cfg
  --     r' `shouldBe` r

  --   it "should find unsat branch after sat branch" $ do
  --     let rootNode = bbp callerCtx "root"
  --                    [ branchCond $ cmpSlt (var "b" 4) (const 300 4) 4 ]
  --         falseNode1 = bbp callerCtx "falseNode1" []
  --         trueNode1 = bbp callerCtx "trueNode1" []
  --         joinNode1 = bbp callerCtx "joinNode1"
  --                     [ branchCond $ cmpE (var "a" 4) (const 200 4) 4 ]
  --         trueNode2 = bbp callerCtx "trueNode2" []
  --         falseNode2 = bbp callerCtx "falseNode2" []
  --         endNode = bbp callerCtx "endNode"
  --                   [ constraint $ cmpE (var "a" 4) (const 400 4) 4 ]

  --         cfg = mkCfg rootNode [ trueNode1
  --                              , falseNode1
  --                              , trueNode1
  --                              , joinNode1
  --                              , falseNode2
  --                              , trueNode2
  --                              , endNode
  --                              ]
  --               [ CfEdge rootNode falseNode1 Cfg.FalseBranch
  --               , CfEdge rootNode trueNode1 Cfg.TrueBranch
  --               , CfEdge falseNode1 joinNode1 Cfg.UnconditionalBranch
  --               , CfEdge trueNode1 joinNode1 Cfg.UnconditionalBranch
  --               , CfEdge joinNode1 falseNode2 Cfg.FalseBranch
  --               , CfEdge falseNode2 endNode Cfg.UnconditionalBranch
  --               , CfEdge joinNode1 trueNode2 Cfg.TrueBranch
  --               , CfEdge trueNode2 endNode Cfg.UnconditionalBranch
  --               ]
  --         r = Cfg.asIdEdge <$> [ CfEdge joinNode1 trueNode2 Cfg.TrueBranch ]

  --     r' <- solve cfg
  --     r' `shouldBe` r

  --   it "should find two unsat branch using same counter-constraint" $ do
  --     let rootNode = bbp callerCtx "root"
  --                    [ branchCond $ cmpSlt (var "a" 4) (const 300 4) 4 ]
  --         falseNode1 = bbp callerCtx "falseNode1" []
  --         trueNode1 = bbp callerCtx "trueNode1" []
  --         joinNode1 = bbp callerCtx "joinNode1"
  --                     [ branchCond $ cmpE (var "a" 4) (const 200 4) 4 ]
  --         trueNode2 = bbp callerCtx "trueNode2" []
  --         falseNode2 = bbp callerCtx "falseNode2" []
  --         endNode = bbp callerCtx "endNode"
  --                   [ constraint $ cmpE (var "a" 4) (const 400 4) 4 ]

  --         cfg = mkCfg rootNode [ trueNode1
  --                              , falseNode1
  --                              , trueNode1
  --                              , joinNode1
  --                              , falseNode2
  --                              , trueNode2
  --                              , endNode
  --                              ]
  --               [ CfEdge rootNode falseNode1 Cfg.FalseBranch
  --               , CfEdge rootNode trueNode1 Cfg.TrueBranch
  --               , CfEdge falseNode1 joinNode1 Cfg.UnconditionalBranch
  --               , CfEdge trueNode1 joinNode1 Cfg.UnconditionalBranch
  --               , CfEdge joinNode1 falseNode2 Cfg.FalseBranch
  --               , CfEdge falseNode2 endNode Cfg.UnconditionalBranch
  --               , CfEdge joinNode1 trueNode2 Cfg.TrueBranch
  --               , CfEdge trueNode2 endNode Cfg.UnconditionalBranch
  --               ]
  --         r = Cfg.asIdEdge <$>
  --             [ CfEdge rootNode trueNode1 Cfg.TrueBranch
  --             , CfEdge joinNode1 trueNode2 Cfg.TrueBranch
  --             ]

  --     r' <- solve cfg
  --     sort r' `shouldBe` sort r

  --   it "should find unsat branch using constraint from pruned branch" $ do
  --     let rootNode = bbp callerCtx "root"
  --                    [ branchCond $ cmpSlt (var "a" 4) (const 300 4) 4 ]
  --         falseNode1 = bbp callerCtx "falseNode1" []
  --         trueNode1 = bbp callerCtx "trueNode1" []
  --         joinNode1 = bbp callerCtx "joinNode1"
  --                     [ branchCond $ cmpE (var "a" 4) (const 200 4) 4 ]
  --         trueNode2 = bbp callerCtx "trueNode2" []
  --         endNode = bbp callerCtx "endNode" []

  --         cfg = mkCfg rootNode [ trueNode1
  --                              , falseNode1
  --                              , trueNode1
  --                              , joinNode1
  --                              , trueNode2
  --                              , endNode
  --                              ]
  --               [ CfEdge rootNode falseNode1 Cfg.FalseBranch
  --               , CfEdge rootNode trueNode1 Cfg.TrueBranch
  --               , CfEdge falseNode1 joinNode1 Cfg.UnconditionalBranch
  --               , CfEdge trueNode1 joinNode1 Cfg.UnconditionalBranch
  --               , CfEdge joinNode1 trueNode2 Cfg.TrueBranch
  --               , CfEdge trueNode2 endNode Cfg.UnconditionalBranch
  --               ]
  --         r = Cfg.asIdEdge <$> [ CfEdge rootNode falseNode1 Cfg.FalseBranch ]

  --     r' <- solve cfg
  --     r' `shouldBe` r

  -- context "simplify" $ do
  --   let solve :: Cfg [Pil.Stmt] -> IO (Cfg [Pil.Stmt])
  --       solve cfg = simplify cfg >>= either (P.error . show) return

  --   it "should find single unsat branch and reduce phi" $ do
  --     let rootNode = bbp callerCtx "root"
  --                    [ branchCond $ cmpE (var "a" 4) (const 800 4) 4 ]
  --         falseNode = bbp callerCtx "falseNode"
  --                    [ def "b1" $ const 200 4 ]
  --         trueNode = bbp callerCtx "trueNode"
  --                    [ def "b2" $ const 400 4 ]
  --         endNode = bbp callerCtx "endNode"
  --                   [ defPhi "b" ["b1", "b2"]
  --                   , constraint $ cmpE (var "a" 4) (const 900 4) 4
  --                   ]

  --         cfg = mkCfg rootNode [ trueNode, falseNode, endNode ]
  --               [ CfEdge rootNode falseNode Cfg.FalseBranch
  --               , CfEdge rootNode trueNode Cfg.TrueBranch
  --               , CfEdge falseNode endNode Cfg.UnconditionalBranch
  --               , CfEdge trueNode endNode Cfg.UnconditionalBranch
  --               ]

  --         endNode' = bbp callerCtx "endNode"
  --                   [ constraint $ cmpE (var "a" 4) (const 900 4) 4 ]

  --         rcfg = mkCfg rootNode [ falseNode, endNode' ]
  --                [ CfEdge rootNode falseNode Cfg.FalseBranch
  --                , CfEdge falseNode endNode' Cfg.UnconditionalBranch
  --                ]

  --     r' <- solve cfg
  --     PrettyShow r' `shouldBe` PrettyShow rcfg

  --   it "should find single unsat branch and reduce phi" $ do
  --     let rootNode = bbp callerCtx "root"
  --                    [ branchCond $ cmpE (var "a" 4) (const 800 4) 4 ]
  --         falseNode = bbp callerCtx "falseNode"
  --                    [ def "b1" $ const 200 4 ]
  --         trueNode = bbp callerCtx "trueNode"
  --                    [ def "b2" $ const 400 4 ]
  --         endNode = bbp callerCtx "endNode"
  --                   [ defPhi "b" ["b1", "b2"]
  --                   , constraint $ cmpE (var "a" 4) (const 900 4) 4
  --                   ]

  --         cfg = mkCfg rootNode [ trueNode, falseNode, endNode ]
  --               [ CfEdge rootNode falseNode Cfg.FalseBranch
  --               , CfEdge rootNode trueNode Cfg.TrueBranch
  --               , CfEdge falseNode endNode Cfg.UnconditionalBranch
  --               , CfEdge trueNode endNode Cfg.UnconditionalBranch
  --               ]

  --         endNode' = bbp callerCtx "endNode"
  --                   [ constraint $ cmpE (var "a" 4) (const 900 4) 4 ]

  --         rcfg = mkCfg rootNode [ falseNode, endNode' ]
  --                [ CfEdge rootNode falseNode Cfg.FalseBranch
  --                , CfEdge falseNode endNode' Cfg.UnconditionalBranch
  --                ]

  --     r' <- solve cfg
  --     PrettyShow r' `shouldBe` PrettyShow rcfg


  --   it "should find two cascading unsat branch" $ do
  --     let rootNode = bbp callerCtx "root"
  --                    [ branchCond $ cmpSlt (var "a" 4) (const 300 4) 4 ]
  --         falseNode1 = bbp callerCtx "falseNode1"
  --                    [ def "b1" $ const 200 4 ]
  --         trueNode1 = bbp callerCtx "trueNode1"
  --                    [ def "b2" $ const 400 4 ]
  --         joinNode1 = bbp callerCtx "joinNode1"
  --                     [ defPhi "b" ["b1", "b2"]
  --                     , branchCond $ cmpNE (var "b" 4) (const 200 4) 4
  --                     ]
  --         falseNode2 = bbp callerCtx "falseNode2"
  --                    [ def "c1" $ const 77 4 ]
  --         trueNode2 = bbp callerCtx "trueNode2"
  --                    [ def "c2" $ const 66 4 ]

  --         endNode = bbp callerCtx "endNode"
  --                   [ defPhi "c" ["c1", "c2"]
  --                   , constraint $ cmpE (var "a" 4) (const 500 4) 4
  --                   ]

  --         cfg = mkCfg rootNode [ trueNode1
  --                              , falseNode1
  --                              , joinNode1
  --                              , trueNode2
  --                              , falseNode2
  --                              , endNode
  --                              ]
  --               [ CfEdge rootNode falseNode1 Cfg.FalseBranch
  --               , CfEdge rootNode trueNode1 Cfg.TrueBranch
  --               , CfEdge falseNode1 joinNode1 Cfg.UnconditionalBranch
  --               , CfEdge trueNode1 joinNode1 Cfg.UnconditionalBranch
  --               , CfEdge joinNode1 falseNode2 Cfg.FalseBranch
  --               , CfEdge joinNode1 trueNode2 Cfg.TrueBranch
  --               , CfEdge falseNode2 endNode Cfg.UnconditionalBranch
  --               , CfEdge trueNode2 endNode Cfg.UnconditionalBranch
                
  --               ]

  --         falseNode1' = bbp callerCtx "falseNode1"
  --                    [ def "b1" $ const 200 4 ]
  --         joinNode1' = bbp callerCtx "joinNode1"
  --                     [ -- branchCond $ cmpNE (var "b1" 4) (const 200 4) 4
  --                       -- TODO: do we still want const prop?
  --                        branchCond $ cmpNE (const 200 4) (const 200 4) 4
  --                     ]
  --         falseNode2' = bbp callerCtx "falseNode2"
  --                    [ def "c1" $ const 77 4 ]

  --         endNode' = bbp callerCtx "endNode"
  --                   [ constraint $ cmpE (var "a" 4) (const 500 4) 4 ]

  --         rcfg = mkCfg rootNode [ falseNode1'
  --                               , joinNode1'
  --                               , falseNode2'
  --                               , endNode'
  --                               ]
  --               [ CfEdge rootNode falseNode1' Cfg.FalseBranch
  --               , CfEdge falseNode1' joinNode1' Cfg.UnconditionalBranch
  --               , CfEdge joinNode1' falseNode2' Cfg.FalseBranch
  --               , CfEdge falseNode2' endNode' Cfg.UnconditionalBranch
  --               ]


  --     r <- solve cfg
  --     PrettyShow r `shouldBe` PrettyShow rcfg


  --   -- it "should cascade prune for NE" $ do
  --   --   let rootNode = bbp callerCtx "root"
  --   --                  [ branchCond $ cmpE (var "a" 4) (const 0 4) 4 ]
  --   --       falseNode1 = bbp callerCtx "falseNode1"
  --   --                  [ def "b1" $ const 200 4 ]
  --   --       -- trueNode1 = bbp callerCtx "trueNode1"
  --   --       --            [ def "b2" $ const 400 4 ]
  --   --       joinNode1 = bbp callerCtx "joinNode1"
  --   --                   [ defPhi "b" ["b1", "b2"]
  --   --                   , branchCond $ cmpNE (var "a" 4) (const 0 4) 4
  --   --                   ]
  --   --       falseNode2 = bbp callerCtx "falseNode2"
  --   --                  [ def "c1" $ const 77 4 ]
  --   --       trueNode2 = bbp callerCtx "trueNode2"
  --   --                  [ def "c2" $ const 66 4 ]

  --   --       endNode = bbp callerCtx "endNode"
  --   --                 [ defPhi "c" ["c1", "c2"]
  --   --                 ]

  --   --       cfg = mkCfg rootNode [ -- trueNode1
  --   --                              falseNode1
  --   --                            , joinNode1
  --   --                            , trueNode2
  --   --                            , falseNode2
  --   --                            , endNode
  --   --                            ]
  --   --             [ CfEdge rootNode falseNode1 Cfg.FalseBranch
  --   --             -- , CfEdge rootNode trueNode1 Cfg.TrueBranch
  --   --             , CfEdge falseNode1 joinNode1 Cfg.UnconditionalBranch
  --   --             -- , CfEdge trueNode1 joinNode1 Cfg.UnconditionalBranch
  --   --             , CfEdge joinNode1 falseNode2 Cfg.FalseBranch
  --   --             , CfEdge joinNode1 trueNode2 Cfg.TrueBranch
  --   --             , CfEdge falseNode2 endNode Cfg.UnconditionalBranch
  --   --             , CfEdge trueNode2 endNode Cfg.UnconditionalBranch
                
  --   --             ]

  --   --       falseNode1' = bbp callerCtx "falseNode1"
  --   --                  [ def "b1" $ const 200 4 ]
  --   --       joinNode1' = bbp callerCtx "joinNode1"
  --   --                   [ -- branchCond $ cmpNE (var "b1" 4) (const 200 4) 4
  --   --                     -- TODO: do we still want const prop?
  --   --                      branchCond $ cmpNE (const 200 4) (const 200 4) 4
  --   --                   ]
  --   --       falseNode2' = bbp callerCtx "falseNode2"
  --   --                  [ def "c1" $ const 77 4 ]

  --   --       endNode' = bbp callerCtx "endNode"
  --   --                 [ constraint $ cmpE (var "a" 4) (const 500 4) 4 ]

  --   --       rcfg = mkCfg rootNode [ falseNode1'
  --   --                             , joinNode1'
  --   --                             , falseNode2'
  --   --                             , endNode'
  --   --                             ]
  --   --             [ CfEdge rootNode falseNode1' Cfg.FalseBranch
  --   --             , CfEdge falseNode1' joinNode1' Cfg.UnconditionalBranch
  --   --             , CfEdge joinNode1' falseNode2' Cfg.FalseBranch
  --   --             , CfEdge falseNode2' endNode' Cfg.UnconditionalBranch
  --   --             ]


  --   --   r <- solve cfg
  --   --   PrettyShow r `shouldBe` PrettyShow rcfg


  context "Edge Dominators" $ do
    it "Should create an empty edge Dominators map for graph with single node" $ do
      let rootNode = bbp callerCtx "root"
                     [ branchCond $ cmpE (var "x" 4) (const 0 4) 4 ]

          cfg = mkCfg rootNode [] []

          edgeGraph = G.toEdgeGraph $ cfg ^. #graph :: AlgaGraph () () (G.EdgeGraphNode BranchType (CfNode ()))
          eRoot = G.NodeNode . Cfg.asIdNode $ cfg ^. #root
          edgeDoms = BC.filterEdges $ G.getDominators eRoot edgeGraph

          rootNode' = asIdNode rootNode
                
          expectedEdgeDom = G.Dominators . HashMap.fromList $ []

      PrettyShow edgeDoms `shouldBe` PrettyShow expectedEdgeDom

    it "Should create a proper edge Dominators map for linear CFG" $ do
      let rootNode = bbp callerCtx "root"
                     [ branchCond $ cmpE (var "x" 4) (const 0 4) 4 ]
          falseNode1 = bbp callerCtx "falseNode1"
                       [ def "c1" $ const 1 4
                       , branchCond $ cmpE (var "x" 4) (const 0 4) 4
                       ]
          trueNode2 = bbp callerCtx "trueNode2"
                      [ def "c3" $ const 3 4 ]

          endNode = bbp callerCtx "endNode"
                    [ defPhi "c" ["c1", "c2", "c3"]
                    ]

          cfg = mkCfg rootNode [ falseNode1
                               , trueNode2
                               , endNode
                               ]
                [ CfEdge rootNode falseNode1 Cfg.FalseBranch
                , CfEdge falseNode1 trueNode2 Cfg.TrueBranch
                , CfEdge trueNode2 endNode Cfg.UnconditionalBranch
                ]

          edgeGraph = G.toEdgeGraph $ cfg ^. #graph :: AlgaGraph () () (G.EdgeGraphNode BranchType (CfNode ()))
          eRoot = G.NodeNode . Cfg.asIdNode $ cfg ^. #root
          edgeDoms = BC.filterEdges $ G.getDominators eRoot edgeGraph

          rootNode' = asIdNode rootNode
          falseNode1' = asIdNode falseNode1
          trueNode2' = asIdNode trueNode2
          endNode' = asIdNode endNode

          edge1 = CfEdge rootNode' falseNode1' Cfg.FalseBranch
          edge2 = CfEdge falseNode1' trueNode2' Cfg.TrueBranch
          edge3 = CfEdge trueNode2' endNode' Cfg.UnconditionalBranch

          expectedEdgeDom = G.Dominators . HashMap.fromList $
            [ (edge2, HashSet.fromList [edge1])
            , (edge3, HashSet.fromList [edge1, edge2])
            ]
      
      PrettyShow edgeDoms `shouldBe` PrettyShow expectedEdgeDom



    it "Should create a proper edge Dominators map for complex CFG" $ do
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
                    [ defPhi "c" ["c1", "c2", "c3"]
                    ]

          cfg = mkCfg rootNode [ falseNode1
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

          edgeGraph = G.toEdgeGraph $ cfg ^. #graph :: AlgaGraph () () (G.EdgeGraphNode BranchType (CfNode ()))
          eRoot = G.NodeNode . Cfg.asIdNode $ cfg ^. #root
          edgeDoms = BC.filterEdges $ G.getDominators eRoot edgeGraph

          rootNode' = asIdNode rootNode
          falseNode1' = asIdNode falseNode1
          falseNode2' = asIdNode falseNode2
          trueNode2' = asIdNode trueNode2
          endNode' = asIdNode endNode

          rootToFalse1 = CfEdge rootNode' falseNode1' Cfg.FalseBranch
          false1ToFalse2 = CfEdge falseNode1' falseNode2' Cfg.FalseBranch
          false1ToTrue2 = CfEdge falseNode1' trueNode2' Cfg.TrueBranch
          false2ToEnd = CfEdge falseNode2' endNode' Cfg.UnconditionalBranch
          true2ToEnd = CfEdge trueNode2' endNode' Cfg.UnconditionalBranch
          rootToEnd = CfEdge rootNode' endNode' Cfg.TrueBranch                
                
          expectedEdgeDom = G.Dominators . HashMap.fromList $
            [ (false1ToFalse2, HashSet.fromList [rootToFalse1])
            , (false1ToTrue2, HashSet.fromList [rootToFalse1])
            , (false2ToEnd, HashSet.fromList [rootToFalse1, false1ToFalse2])
            , (true2ToEnd, HashSet.fromList [rootToFalse1, false1ToTrue2])
            ]
      
      PrettyShow edgeDoms `shouldBe` PrettyShow expectedEdgeDom
  

  context "Edge PostDominators" $ do
    it "Should create an empty edge PostDominators map for graph with single node" $ do
      let rootNode = bbp callerCtx "root"
                     [ branchCond $ cmpE (var "x" 4) (const 0 4) 4 ]

          cfg = mkCfg rootNode [] []

          edgeGraph = G.toEdgeGraph $ cfg ^. #graph :: AlgaGraph () () (G.EdgeGraphNode BranchType (CfNode ()))
          eRoot = G.NodeNode . Cfg.asIdNode $ cfg ^. #root
          edgeDoms = BC.filterEdges $ G.getPostDominators eRoot edgeGraph

          rootNode' = asIdNode rootNode
                
          expectedEdgeDom = G.PostDominators . HashMap.fromList $ []

      PrettyShow edgeDoms `shouldBe` PrettyShow expectedEdgeDom

    it "Should create a proper edge PostDominators map for linear CFG" $ do
      let rootNode = bbp callerCtx "root"
                     [ branchCond $ cmpE (var "x" 4) (const 0 4) 4 ]
          falseNode1 = bbp callerCtx "falseNode1"
                       [ def "c1" $ const 1 4
                       , branchCond $ cmpE (var "x" 4) (const 0 4) 4
                       ]
          trueNode2 = bbp callerCtx "trueNode2"
                      [ def "c3" $ const 3 4 ]

          endNode = bbp callerCtx "endNode"
                    [ defPhi "c" ["c1", "c2", "c3"]
                    ]

          cfg = mkCfg rootNode [ falseNode1
                               , trueNode2
                               , endNode
                               ]
                [ CfEdge rootNode falseNode1 Cfg.FalseBranch
                , CfEdge falseNode1 trueNode2 Cfg.TrueBranch
                , CfEdge trueNode2 endNode Cfg.UnconditionalBranch
                ]

          edgeGraph = G.toEdgeGraph $ cfg ^. #graph :: AlgaGraph () () (G.EdgeGraphNode BranchType (CfNode ()))
          -- eRoot = G.NodeNode . Cfg.asIdNode $ cfg ^. #root
          -- TODO: Why do we pass in the end node?
          eRoot = G.NodeNode . Cfg.asIdNode $ endNode
          edgeDoms = BC.filterEdges $ G.getPostDominators eRoot edgeGraph

          rootNode' = asIdNode rootNode
          falseNode1' = asIdNode falseNode1
          trueNode2' = asIdNode trueNode2
          endNode' = asIdNode endNode

          edge1 = CfEdge rootNode' falseNode1' Cfg.FalseBranch
          edge2 = CfEdge falseNode1' trueNode2' Cfg.TrueBranch
          edge3 = CfEdge trueNode2' endNode' Cfg.UnconditionalBranch

          expectedEdgeDom = G.PostDominators . HashMap.fromList $
            [ (edge1, HashSet.fromList [edge2, edge3])
            , (edge2, HashSet.fromList [edge3])
            ]
          -- expectedEdgeDom = G.PostDominators . HashMap.fromList $ []
      
      PrettyShow edgeDoms `shouldBe` PrettyShow expectedEdgeDom


    -- it "Should create a proper edge Dominators map for complex CFG" $ do
    --   let rootNode = bbp callerCtx "root"
    --                  [ branchCond $ cmpE (var "x" 4) (const 0 4) 4 ]
    --       falseNode1 = bbp callerCtx "falseNode1"
    --                    [ def "c1" $ const 1 4
    --                    , branchCond $ cmpE (var "x" 4) (const 0 4) 4
    --                    ]
    --       falseNode2 = bbp callerCtx "falseNode2"
    --                    [ def "c2" $ const 2 4 ]
    --       trueNode2 = bbp callerCtx "trueNode2"
    --                   [ def "c3" $ const 3 4 ]

    --       endNode = bbp callerCtx "endNode"
    --                 [ defPhi "c" ["c1", "c2", "c3"]
    --                 ]

    --       cfg = mkCfg rootNode [ falseNode1
    --                            , trueNode2
    --                            , falseNode2
    --                            , endNode
    --                            ]
    --             [ CfEdge rootNode falseNode1 Cfg.FalseBranch
    --             , CfEdge falseNode1 falseNode2 Cfg.FalseBranch
    --             , CfEdge falseNode1 trueNode2 Cfg.TrueBranch
    --             , CfEdge falseNode2 endNode Cfg.UnconditionalBranch
    --             , CfEdge trueNode2 endNode Cfg.UnconditionalBranch
    --             , CfEdge rootNode endNode Cfg.TrueBranch                
    --             ]

    --       edgeGraph = G.toEdgeGraph $ cfg ^. #graph :: AlgaGraph () () (G.EdgeGraphNode BranchType (CfNode ()))
    --       eRoot = G.NodeNode . Cfg.asIdNode $ cfg ^. #root
    --       edgeDoms = BC.filterEdges $ G.getDominators eRoot edgeGraph

    --       rootNode' = asIdNode rootNode
    --       falseNode1' = asIdNode falseNode1
    --       falseNode2' = asIdNode falseNode2
    --       trueNode2' = asIdNode trueNode2
    --       endNode' = asIdNode endNode

    --       rootToFalse1 = CfEdge rootNode' falseNode1' Cfg.FalseBranch
    --       false1ToFalse2 = CfEdge falseNode1' falseNode2' Cfg.FalseBranch
    --       false1ToTrue2 = CfEdge falseNode1' trueNode2' Cfg.TrueBranch
    --       false2ToEnd = CfEdge falseNode2' endNode' Cfg.UnconditionalBranch
    --       true2ToEnd = CfEdge trueNode2' endNode' Cfg.UnconditionalBranch
    --       rootToEnd = CfEdge rootNode' endNode' Cfg.TrueBranch                
                
    --       expectedEdgeDom = G.Dominators . HashMap.fromList $
    --         [ (false1ToFalse2, HashSet.fromList [rootToFalse1])
    --         , (false1ToTrue2, HashSet.fromList [rootToFalse1])
    --         , (false2ToEnd, HashSet.fromList [rootToFalse1, false1ToFalse2])
    --         , (true2ToEnd, HashSet.fromList [rootToFalse1, false1ToTrue2])
    --         ]
      
    --   PrettyShow edgeDoms `shouldBe` PrettyShow expectedEdgeDom
