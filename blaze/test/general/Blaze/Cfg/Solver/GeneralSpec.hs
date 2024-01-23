{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Evaluate" -}

module Blaze.Cfg.Solver.GeneralSpec where

import Blaze.Prelude hiding (Constraint, Symbol, Type, bitSize, const, sym)
import Prelude qualified as P

import Blaze.Cfg hiding (
  BasicBlockNode (ctx),
  CallNode (ctx),
  func,
 )
import Blaze.Cfg qualified as Cfg
import Blaze.Cfg.Analysis qualified as CfgA
import Blaze.Cfg.Checker
import Blaze.Cfg.Solver.General (generalCfgFormula, getUnsatBranches, simplify)
import Blaze.Function (Function (Function))
import Blaze.Function qualified as Func
import Blaze.Pil.Construct
import Blaze.Pil.Solver qualified as PilSolver
import Blaze.Pretty (PrettyShow' (PrettyShow'))
import Blaze.Types.Pil (Ctx (Ctx), Stmt)
import Blaze.Types.Pil.Solver (SolverCtx (SolverCtx), SolverLeniency (IgnoreErrors), checkSatWith)
import Blaze.Util.Spec (mkUuid1)
import Data.HashMap.Strict qualified as HashMap
import Data.SBV.Dynamic as D hiding (Solver, name)
import Data.SBV.Trans qualified as SBV
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
spec = describe "Blaze.Cfg.Solver.General" $ do
  context "generalCfgFormula" $ do
    let generalSolve :: PilCfg -> IO PilSolver.SolverResult
        generalSolve cfg = case checkCfg cfg of
          Left err -> P.error $ show err
          Right (_, cfg', tr) -> do
            let ddg = CfgA.getDataDependenceGraph cfg
            er <- flip (checkSatWith SBV.z3)
                  ( PilSolver.emptyState
                  , SolverCtx (tr ^. #varSymTypeMap) HashMap.empty False IgnoreErrors
                  )
                  $ PilSolver.declarePilVars >> generalCfgFormula ddg cfg'
            either (P.error . show) (return . view _1) er

    it "should solve a single basic block" $ do
      let rootNode = bbp callerCtx "root"
                     [ def "x" $ sx (const 900 4) 4
                     , def "a" $ sub (var "x" 4) (const 800 4) 4 ]
          cfg = mkCfg 0 rootNode [] []
          rvars = HashMap.fromList [ ("x", CV (KBounded True 32) (CInteger 900))
                                   , ("a", CV (KBounded True 32) (CInteger 100))
                                   ]
      r <- generalSolve cfg
      r `shouldBe` PilSolver.Sat rvars

    it "should should two nodes" $ do
      let rootNode = bbp callerCtx "root"
                     [ def "a" $ const 800 4 ]
          endNode = bbp callerCtx "endNode"
                     [ def "b" $ const 200 4 ]
          cfg = mkCfg 0 rootNode [ endNode ]
                [ CfEdge rootNode endNode Cfg.UnconditionalBranch ]
          rvars = HashMap.fromList
            [ ("a", CV (KBounded False 32) (CInteger 800))
            , ("b", CV (KBounded False 32) (CInteger 200))
            ]
      r <- generalSolve cfg
      r `shouldBe` PilSolver.Sat rvars

    it "should solve a pruned True branch cond" $ do
      let rootNode = bbp callerCtx "root"
                     [ branchCond $ cmpE (var "a" 4) (const 800 4) 4]
          trueNode = bbp callerCtx "trueNode"
                     [ def "b" $ const 200 4 ]
          cfg = mkCfg 0 rootNode [ trueNode ]
                [ CfEdge rootNode trueNode Cfg.TrueBranch ]
          rvars = HashMap.fromList
            [ ("a", CV (KBounded False 32) (CInteger 800))
            , ("b", CV (KBounded False 32) (CInteger 200))
            ]
      r <- generalSolve cfg
      r `shouldBe` PilSolver.Sat rvars

    it "should solve a pruned False branch cond" $ do
      let rootNode = bbp callerCtx "root"
                     [ branchCond $ cmpNE (var "a" 4) (const 800 4) 4]
          falseNode = bbp callerCtx "falseNode"
                     [ def "b" $ const 200 4 ]
          cfg = mkCfg 0 rootNode [ falseNode ]
                [ CfEdge rootNode falseNode Cfg.FalseBranch ]
          rvars = HashMap.fromList
            [ ("a", CV (KBounded False 32) (CInteger 800))
            , ("b", CV (KBounded False 32) (CInteger 200))
            ]
      r <- generalSolve cfg
      r `shouldBe` PilSolver.Sat rvars

    it "should ignore an unpruned if branch constraint" $ do
      let rootNode = bbp callerCtx "root"
                     [ branchCond $ cmpNE (var "a" 4) (const 800 4) 4 ]
          trueNode = bbp callerCtx "trueNode" []
          falseNode = bbp callerCtx "falseNode" []
          endNode = bbp callerCtx "endNode"
                    [ constraint $ cmpE (var "a" 4) (const 800 4) 4 ]
          cfg = mkCfg 0 rootNode [ trueNode, falseNode, endNode ]
                [ CfEdge rootNode falseNode Cfg.FalseBranch
                , CfEdge rootNode trueNode Cfg.TrueBranch
                , CfEdge trueNode endNode Cfg.UnconditionalBranch
                , CfEdge falseNode endNode Cfg.UnconditionalBranch
                ]
          rvars = HashMap.fromList
            [ ("a", CV (KBounded False 32) (CInteger 800)) ]

      r <- generalSolve cfg
      r `shouldBe` PilSolver.Sat rvars


    it "should 'or' together non-loop phi vars" $ do
      let rootNode = bbp callerCtx "root"
                     [ branchCond $ cmpE (const 800 4) (const 800 4) 4]
          falseNode = bbp callerCtx "falseNode"
                     [ def "b1" $ const 200 4 ]
          trueNode = bbp callerCtx "trueNode"
                     [ def "b2" $ const 400 4 ]
          endNode c = bbp callerCtx "endNode"
                      [ defPhi 4 "b" ["b1", "b2"]
                      , constraint c
                      ]

          cfg end' = mkCfg 0 rootNode [ trueNode, falseNode, end' ]
                   [ CfEdge rootNode falseNode Cfg.FalseBranch
                   , CfEdge rootNode trueNode Cfg.TrueBranch
                   , CfEdge falseNode end' Cfg.UnconditionalBranch
                   , CfEdge trueNode end' Cfg.UnconditionalBranch
                   ]
          rvars b = HashMap.fromList
            [ ("b1", CV (KBounded False 32) (CInteger 200))
            , ("b2", CV (KBounded False 32) (CInteger 400))
            , ("b", CV (KBounded False 32) (CInteger b))
            ]

      r1 <- generalSolve . cfg . endNode $ cmpE (var "b" 4) (const 200 4) 4
      r1 `shouldBe` PilSolver.Sat (rvars 200)

      r2 <- generalSolve . cfg . endNode $ cmpE (var "b" 4) (const 400 4) 4
      r2 `shouldBe` PilSolver.Sat (rvars 400)

      r3 <- generalSolve . cfg . endNode $ cmpE (var "b" 4) (const 300 4) 4
      r3 `shouldBe` PilSolver.Unsat Nothing

    it "should ignore loop phi var assignment" $ do
      let rootNode = bbp callerCtx "root"
                     [ def "b0" $ const 0 4 ]
          loopStart = bbp callerCtx "loopStart"
                      [ defPhi 4 "b" ["b0", "b1"]
                      , branchCond $ cmpUle (var "b" 4) (const 800 4) 4
                      ]
          falseNode = bbp callerCtx "falseNode"
                     [ def "b1" $ add (var "b" 4) (const 1 4) 4 ]
          trueNode = bbp callerCtx "trueNode" []
          endNode = bbp callerCtx "endNode"
                    [ constraint $ cmpE (var "b" 4) (const 900 4) 4
                    , constraint $ cmpE (var "b1" 4) (const 901 4) 4
                    ]

          cfg = mkCfg 0 rootNode [ trueNode, falseNode, endNode ]
                   [ CfEdge rootNode loopStart Cfg.UnconditionalBranch
                   , CfEdge loopStart falseNode Cfg.FalseBranch
                   , CfEdge loopStart trueNode Cfg.TrueBranch
                   , CfEdge falseNode loopStart Cfg.UnconditionalBranch
                   , CfEdge trueNode endNode Cfg.UnconditionalBranch
                   ]
          rvars = HashMap.fromList
            [ ("b0", CV (KBounded False 32) (CInteger 0))
            , ("b1", CV (KBounded False 32) (CInteger 901))
            , ("b", CV (KBounded False 32) (CInteger 900))
            ]

      r <- generalSolve cfg
      r `shouldBe` PilSolver.Sat rvars

  context "unsatBranches" $ do
    let solve :: PilCfg -> IO [CfEdge PilNode]
        solve cfg = getUnsatBranches cfg >>= either (P.error . show) return

    it "should find single unsat branch" $ do
      let rootNode = bbp callerCtx "root"
                     [ branchCond $ cmpE (var "a" 4) (const 800 4) 4 ]
          falseNode = bbp callerCtx "falseNode"
                     [ def "b1" $ const 200 4 ]
          trueNode = bbp callerCtx "trueNode"
                     [ def "b2" $ const 400 4 ]
          endNode = bbp callerCtx "endNode"
                    [ defPhi 4 "b" ["b1", "b2"]
                    , constraint $ cmpE (var "a" 4) (const 900 4) 4
                    ]

          cfg = mkCfg 0 rootNode [ trueNode, falseNode, endNode ]
                [ CfEdge rootNode falseNode Cfg.FalseBranch
                , CfEdge rootNode trueNode Cfg.TrueBranch
                , CfEdge falseNode endNode Cfg.UnconditionalBranch
                , CfEdge trueNode endNode Cfg.UnconditionalBranch
                ]
          r = [CfEdge rootNode trueNode Cfg.TrueBranch]

      r' <- solve cfg
      r' `shouldBe` r

    it "should find unsat branch after sat branch" $ do
      let rootNode = bbp callerCtx "root"
                     [ branchCond $ cmpSlt (var "b" 4) (const 300 4) 4 ]
          falseNode1 = bbp callerCtx "falseNode1" []
          trueNode1 = bbp callerCtx "trueNode1" []
          joinNode1 = bbp callerCtx "joinNode1"
                      [ branchCond $ cmpE (var "a" 4) (const 200 4) 4 ]
          trueNode2 = bbp callerCtx "trueNode2" []
          falseNode2 = bbp callerCtx "falseNode2" []
          endNode = bbp callerCtx "endNode"
                    [ constraint $ cmpE (var "a" 4) (const 400 4) 4 ]

          cfg = mkCfg 0 rootNode [ trueNode1
                               , falseNode1
                               , trueNode1
                               , joinNode1
                               , falseNode2
                               , trueNode2
                               , endNode
                               ]
                [ CfEdge rootNode falseNode1 Cfg.FalseBranch
                , CfEdge rootNode trueNode1 Cfg.TrueBranch
                , CfEdge falseNode1 joinNode1 Cfg.UnconditionalBranch
                , CfEdge trueNode1 joinNode1 Cfg.UnconditionalBranch
                , CfEdge joinNode1 falseNode2 Cfg.FalseBranch
                , CfEdge falseNode2 endNode Cfg.UnconditionalBranch
                , CfEdge joinNode1 trueNode2 Cfg.TrueBranch
                , CfEdge trueNode2 endNode Cfg.UnconditionalBranch
                ]
          r = [ CfEdge joinNode1 trueNode2 Cfg.TrueBranch ]

      r' <- solve cfg
      r' `shouldBe` r

    it "should find two unsat branch using same counter-constraint" $ do
      let rootNode = bbp callerCtx "root"
                     [ branchCond $ cmpSlt (var "a" 4) (const 300 4) 4 ]
          falseNode1 = bbp callerCtx "falseNode1" []
          trueNode1 = bbp callerCtx "trueNode1" []
          joinNode1 = bbp callerCtx "joinNode1"
                      [ branchCond $ cmpE (var "a" 4) (const 200 4) 4 ]
          trueNode2 = bbp callerCtx "trueNode2" []
          falseNode2 = bbp callerCtx "falseNode2" []
          endNode = bbp callerCtx "endNode"
                    [ constraint $ cmpE (var "a" 4) (const 400 4) 4 ]

          cfg = mkCfg 0 rootNode [ trueNode1
                               , falseNode1
                               , trueNode1
                               , joinNode1
                               , falseNode2
                               , trueNode2
                               , endNode
                               ]
                [ CfEdge rootNode falseNode1 Cfg.FalseBranch
                , CfEdge rootNode trueNode1 Cfg.TrueBranch
                , CfEdge falseNode1 joinNode1 Cfg.UnconditionalBranch
                , CfEdge trueNode1 joinNode1 Cfg.UnconditionalBranch
                , CfEdge joinNode1 falseNode2 Cfg.FalseBranch
                , CfEdge falseNode2 endNode Cfg.UnconditionalBranch
                , CfEdge joinNode1 trueNode2 Cfg.TrueBranch
                , CfEdge trueNode2 endNode Cfg.UnconditionalBranch
                ]
          r = [ CfEdge rootNode trueNode1 Cfg.TrueBranch
              , CfEdge joinNode1 trueNode2 Cfg.TrueBranch
              ]

      r' <- solve cfg
      sort r' `shouldBe` sort r

    it "should find unsat branch using constraint from pruned branch" $ do
      let rootNode = bbp callerCtx "root"
                     [ branchCond $ cmpSlt (var "a" 4) (const 300 4) 4 ]
          falseNode1 = bbp callerCtx "falseNode1" []
          trueNode1 = bbp callerCtx "trueNode1" []
          joinNode1 = bbp callerCtx "joinNode1"
                      [ branchCond $ cmpE (var "a" 4) (const 200 4) 4 ]
          trueNode2 = bbp callerCtx "trueNode2" []
          endNode = bbp callerCtx "endNode" []

          cfg = mkCfg 0 rootNode [ trueNode1
                               , falseNode1
                               , trueNode1
                               , joinNode1
                               , trueNode2
                               , endNode
                               ]
                [ CfEdge rootNode falseNode1 Cfg.FalseBranch
                , CfEdge rootNode trueNode1 Cfg.TrueBranch
                , CfEdge falseNode1 joinNode1 Cfg.UnconditionalBranch
                , CfEdge trueNode1 joinNode1 Cfg.UnconditionalBranch
                , CfEdge joinNode1 trueNode2 Cfg.TrueBranch
                , CfEdge trueNode2 endNode Cfg.UnconditionalBranch
                ]
          r = [ CfEdge rootNode falseNode1 Cfg.FalseBranch ]

      r' <- solve cfg
      r' `shouldBe` r

  context "simplify" $ do
    let solve :: PilCfg -> IO PilCfg
        solve cfg = simplify cfg >>= either (P.error . show) return

    it "should find single unsat branch and reduce phi" $ do
      let rootNode = bbp callerCtx "root"
                     [ branchCond $ cmpE (var "a" 4) (const 800 4) 4 ]
          falseNode = bbp callerCtx "falseNode"
                     [ def "b1" $ const 200 4 ]
          trueNode = bbp callerCtx "trueNode"
                     [ def "b2" $ const 400 4 ]
          endNode = bbp callerCtx "endNode"
                    [ defPhi 4 "b" ["b1", "b2"]
                    , constraint $ cmpE (var "a" 4) (const 900 4) 4
                    ]

          cfg = mkCfg 0 rootNode [ trueNode, falseNode, endNode ]
                [ CfEdge rootNode falseNode Cfg.FalseBranch
                , CfEdge rootNode trueNode Cfg.TrueBranch
                , CfEdge falseNode endNode Cfg.UnconditionalBranch
                , CfEdge trueNode endNode Cfg.UnconditionalBranch
                ]

          endNode' = bbp callerCtx "endNode"
                    [ constraint $ cmpE (var "a" 4) (const 900 4) 4 ]

          rcfg = mkCfg 0 rootNode [ falseNode, endNode' ]
                 [ CfEdge rootNode falseNode Cfg.FalseBranch
                 , CfEdge falseNode endNode' Cfg.UnconditionalBranch
                 ]

      r' <- solve cfg
      PrettyShow' r' `shouldBe` PrettyShow' rcfg

    it "should find single unsat branch and reduce phi" $ do
      let rootNode = bbp callerCtx "root"
                     [ branchCond $ cmpE (var "a" 4) (const 800 4) 4 ]
          falseNode = bbp callerCtx "falseNode"
                     [ def "b1" $ const 200 4 ]
          trueNode = bbp callerCtx "trueNode"
                     [ def "b2" $ const 400 4 ]
          endNode = bbp callerCtx "endNode"
                    [ defPhi 4 "b" ["b1", "b2"]
                    , constraint $ cmpE (var "a" 4) (const 900 4) 4
                    ]

          cfg = mkCfg 0 rootNode [ trueNode, falseNode, endNode ]
                [ CfEdge rootNode falseNode Cfg.FalseBranch
                , CfEdge rootNode trueNode Cfg.TrueBranch
                , CfEdge falseNode endNode Cfg.UnconditionalBranch
                , CfEdge trueNode endNode Cfg.UnconditionalBranch
                ]

          endNode' = bbp callerCtx "endNode"
                    [ constraint $ cmpE (var "a" 4) (const 900 4) 4 ]

          rcfg = mkCfg 0 rootNode [ falseNode, endNode' ]
                 [ CfEdge rootNode falseNode Cfg.FalseBranch
                 , CfEdge falseNode endNode' Cfg.UnconditionalBranch
                 ]

      r' <- solve cfg
      PrettyShow' r' `shouldBe` PrettyShow' rcfg


    it "should find two cascading unsat branch" $ do
      let rootNode = bbp callerCtx "root"
                     [ branchCond $ cmpSlt (var "a" 4) (const 300 4) 4 ]
          falseNode1 = bbp callerCtx "falseNode1"
                     [ def "b1" $ const 200 4 ]
          trueNode1 = bbp callerCtx "trueNode1"
                     [ def "b2" $ const 400 4 ]
          joinNode1 = bbp callerCtx "joinNode1"
                      [ defPhi 4 "b" ["b1", "b2"]
                      , branchCond $ cmpNE (var "b" 4) (const 200 4) 4
                      ]
          falseNode2 = bbp callerCtx "falseNode2"
                     [ def "c1" $ const 77 4 ]
          trueNode2 = bbp callerCtx "trueNode2"
                     [ def "c2" $ const 66 4 ]

          endNode = bbp callerCtx "endNode"
                    [ defPhi 4 "c" ["c1", "c2"]
                    , constraint $ cmpE (var "a" 4) (const 500 4) 4
                    ]

          cfg = mkCfg 0 rootNode [ trueNode1
                               , falseNode1
                               , joinNode1
                               , trueNode2
                               , falseNode2
                               , endNode
                               ]
                [ CfEdge rootNode falseNode1 Cfg.FalseBranch
                , CfEdge rootNode trueNode1 Cfg.TrueBranch
                , CfEdge falseNode1 joinNode1 Cfg.UnconditionalBranch
                , CfEdge trueNode1 joinNode1 Cfg.UnconditionalBranch
                , CfEdge joinNode1 falseNode2 Cfg.FalseBranch
                , CfEdge joinNode1 trueNode2 Cfg.TrueBranch
                , CfEdge falseNode2 endNode Cfg.UnconditionalBranch
                , CfEdge trueNode2 endNode Cfg.UnconditionalBranch

                ]

          falseNode1' = bbp callerCtx "falseNode1"
                     [ def "b1" $ const 200 4 ]
          joinNode1' = bbp callerCtx "joinNode1"
                      [ -- branchCond $ cmpNE (var "b1" 4) (const 200 4) 4
                        -- TODO: do we still want const prop?
                         branchCond $ cmpNE (const 200 4) (const 200 4) 4
                      ]
          falseNode2' = bbp callerCtx "falseNode2"
                     [ def "c1" $ const 77 4 ]

          endNode' = bbp callerCtx "endNode"
                    [ constraint $ cmpE (var "a" 4) (const 500 4) 4 ]

          rcfg = mkCfg 0 rootNode [ falseNode1'
                                , joinNode1'
                                , falseNode2'
                                , endNode'
                                ]
                [ CfEdge rootNode falseNode1' Cfg.FalseBranch
                , CfEdge falseNode1' joinNode1' Cfg.UnconditionalBranch
                , CfEdge joinNode1' falseNode2' Cfg.FalseBranch
                , CfEdge falseNode2' endNode' Cfg.UnconditionalBranch
                ]


      r <- solve cfg
      PrettyShow' r `shouldBe` PrettyShow' rcfg


    -- it "should cascade prune for NE" $ do
    --   let rootNode = bbp callerCtx "root"
    --                  [ branchCond $ cmpE (var "a" 4) (const 0 4) 4 ]
    --       falseNode1 = bbp callerCtx "falseNode1"
    --                  [ def "b1" $ const 200 4 ]
    --       -- trueNode1 = bbp callerCtx "trueNode1"
    --       --            [ def "b2" $ const 400 4 ]
    --       joinNode1 = bbp callerCtx "joinNode1"
    --                   [ defPhi 4 "b" ["b1", "b2"]
    --                   , branchCond $ cmpNE (var "a" 4) (const 0 4) 4
    --                   ]
    --       falseNode2 = bbp callerCtx "falseNode2"
    --                  [ def "c1" $ const 77 4 ]
    --       trueNode2 = bbp callerCtx "trueNode2"
    --                  [ def "c2" $ const 66 4 ]

    --       endNode = bbp callerCtx "endNode"
    --                 [ defPhi 4 "c" ["c1", "c2"]
    --                 ]

    --       cfg = mkCfg 0 rootNode [ -- trueNode1
    --                              falseNode1
    --                            , joinNode1
    --                            , trueNode2
    --                            , falseNode2
    --                            , endNode
    --                            ]
    --             [ CfEdge rootNode falseNode1 Cfg.FalseBranch
    --             -- , CfEdge rootNode trueNode1 Cfg.TrueBranch
    --             , CfEdge falseNode1 joinNode1 Cfg.UnconditionalBranch
    --             -- , CfEdge trueNode1 joinNode1 Cfg.UnconditionalBranch
    --             , CfEdge joinNode1 falseNode2 Cfg.FalseBranch
    --             , CfEdge joinNode1 trueNode2 Cfg.TrueBranch
    --             , CfEdge falseNode2 endNode Cfg.UnconditionalBranch
    --             , CfEdge trueNode2 endNode Cfg.UnconditionalBranch

    --             ]

    --       falseNode1' = bbp callerCtx "falseNode1"
    --                  [ def "b1" $ const 200 4 ]
    --       joinNode1' = bbp callerCtx "joinNode1"
    --                   [ -- branchCond $ cmpNE (var "b1" 4) (const 200 4) 4
    --                     -- TODO: do we still want const prop?
    --                      branchCond $ cmpNE (const 200 4) (const 200 4) 4
    --                   ]
    --       falseNode2' = bbp callerCtx "falseNode2"
    --                  [ def "c1" $ const 77 4 ]

    --       endNode' = bbp callerCtx "endNode"
    --                 [ constraint $ cmpE (var "a" 4) (const 500 4) 4 ]

    --       rcfg = mkCfg 0 rootNode [ falseNode1'
    --                             , joinNode1'
    --                             , falseNode2'
    --                             , endNode'
    --                             ]
    --             [ CfEdge rootNode falseNode1' Cfg.FalseBranch
    --             , CfEdge falseNode1' joinNode1' Cfg.UnconditionalBranch
    --             , CfEdge joinNode1' falseNode2' Cfg.FalseBranch
    --             , CfEdge falseNode2' endNode' Cfg.UnconditionalBranch
    --             ]


    --   r <- solve cfg
    --   PrettyShow' r `shouldBe` PrettyShow' rcfg
