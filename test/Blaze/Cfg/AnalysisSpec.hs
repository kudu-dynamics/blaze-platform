{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Evaluate" -}

module Blaze.Cfg.AnalysisSpec where

import Blaze.Cfg hiding (BasicBlockNode (ctx), CallNode (ctx), func, root, end)
import Blaze.Cfg.Analysis (removeEmptyBasicBlockNodes, getStmts, removeUnusedPhi, focus)
import qualified Blaze.Cfg.Analysis as CfgA
import Blaze.Function (Function (Function))
import Blaze.Prelude hiding (const)
import Blaze.Pretty (PrettyShow (PrettyShow))
import Blaze.Types.Pil (Ctx (Ctx), CtxId (CtxId))
import Blaze.Util.Spec (bb, mkUuid1)
import Test.Hspec hiding (focus)
import Blaze.Pil.Construct
import qualified Data.HashSet as HashSet
import Blaze.Cfg.Interprocedural (InterCfg(InterCfg))


fooCtx :: Ctx
fooCtx = Ctx func . CtxId $ mkUuid1 (0 :: Int)
  where
    func = Function Nothing "foo" 0x00 []

spec :: Spec
spec = describe "Blaze.Cfg.Analysis" $ do
  context "removeEmptyBasicBlockNodes" $ do
    let units :: Int -> [()]
        units = flip replicate ()

        bbnn :: Text -> Int -> CfNode [()]
        bbnn name unitsLength = bb fooCtx x x $ units unitsLength
          where
            x = fromIntegral $ hash name

    it "should remove single empty node" $ do
      let input = mkCfg
            (bbnn "root" 4)
            [ bbnn "a" 0
            , bbnn "terminal" 3
            ]
            [ CfEdge
              (bbnn "root" 4)
              (bbnn "a" 0)
              TrueBranch
            , CfEdge
              (bbnn "a" 0)
              (bbnn "terminal" 3)
              UnconditionalBranch
            ]
          expected = mkCfg
            (bbnn "root" 4)
            [ bbnn "terminal" 3
            ]
            [ CfEdge
              (bbnn "root" 4)
              (bbnn "terminal" 3)
              TrueBranch
            ]

      PrettyShow (removeEmptyBasicBlockNodes input)
        `shouldBe`
        PrettyShow expected


    it "should remove two empty nodes in series" $ do
      let input = mkCfg
            (bbnn "root" 4)
            [ bbnn "a" 0
            , bbnn "b" 0
            , bbnn "terminal" 3
            ]
            [ CfEdge
              (bbnn "root" 4)
              (bbnn "a" 0)
              TrueBranch
            , CfEdge
              (bbnn "a" 0)
              (bbnn "b" 0)
              UnconditionalBranch
            , CfEdge
              (bbnn "b" 0)
              (bbnn "terminal" 3)
              UnconditionalBranch
            ]

          expected = mkCfg
            (bbnn "root" 4)
            [ bbnn "terminal" 3
            ]
            [ CfEdge
              (bbnn "root" 4)
              (bbnn "terminal" 3)
              TrueBranch
            ]

      PrettyShow (removeEmptyBasicBlockNodes input)
        `shouldBe`
        PrettyShow expected


    it "should remove two empty nodes in parallel" $ do
      let input = mkCfg
            (bbnn "root" 4)
            [ bbnn "a" 0
            , bbnn "b" 0
            , bbnn "terminal" 3
            ]
            [ CfEdge
              (bbnn "root" 4)
              (bbnn "a" 0)
              TrueBranch
            , CfEdge
              (bbnn "root" 4)
              (bbnn "b" 0)
              FalseBranch
            , CfEdge
              (bbnn "a" 0)
              (bbnn "terminal" 3)
              UnconditionalBranch
            , CfEdge
              (bbnn "b" 0)
              (bbnn "terminal" 3)
              UnconditionalBranch
            ]

          expected = mkCfg
            (bbnn "root" 4)
            [ bbnn "terminal" 3
            ]
            [ CfEdge
              (bbnn "root" 4)
              (bbnn "terminal" 3)
              UnconditionalBranch
            ]
      
      PrettyShow (removeEmptyBasicBlockNodes input)
        `shouldBe`
        PrettyShow expected

  context "removeUnusedPhi" $ do
    it "should remove DefPhi statements where defined variable is unused" $ do
      let root = bb fooCtx 0 1 [def "x" (const 42 4)]
          mid =
            bb
              fooCtx
              1
              2
              [ def "y" (add (var "x" 4) (const 10 4) 4)
              , defPhi "unusedVar" ["a", "b", "c"]
              ]
          end =
            bb
              fooCtx
              2
              3
              [ def "z" (add (var "x" 4) (var "y" 4) 4)
              , def "zz" (const 6 4)
              ]
          input =
            InterCfg $
              mkCfg
                root
                [mid, end]
                [ CfEdge root mid UnconditionalBranch
                , CfEdge mid end UnconditionalBranch
                ]
      HashSet.fromList
        [ def "x" (const 42 4)
        , def "y" (add (var "x" 4) (const 10 4) 4)
        , def "z" (add (var "x" 4) (var "y" 4) 4)
        , def "zz" (const 6 4)
        ]
        `shouldBe` (HashSet.fromList . concat . getStmts . removeUnusedPhi $ input)


  context "focus" $ do
    it "should removed unused variable from phi stmts" $ do
      let root =
            bb
              fooCtx
              0
              1
              [ def "x" (var "w" 4)
              , branchCond $ cmpNE (var "x" 4) (const 0 4) 4
              ]
          midLeft =
            bb
              fooCtx
              1
              2
              [ def "y#1" (add (var "x" 4) (const 10 4) 4)
              ]
          midRight =
            bb
              fooCtx
              3
              4
              [ def "y#2" (add (var "x" 4) (const 5000 4) 4)
              ]
          end =
            bb
              fooCtx
              5
              6
              [ defPhi "r" ["y#1", "y#2"]
              , ret $ var "r" 4
              ]
          root' =
            bb
              fooCtx
              0
              1
              [ branchCond $ cmpNE (var "w" 4) (const 0 4) 4
              ]
          midLeft' =
            bb
              fooCtx
              1
              2
              [ def "y#1" (add (var "w" 4) (const 10 4) 4)
              ]
          end' =
            bb
              fooCtx
              5
              6
              [ret $ var "y#1" 4]
          input =
            InterCfg $
              mkCfg
                root
                [midLeft, midRight, end]
                [ CfEdge root midLeft TrueBranch
                , CfEdge root midRight FalseBranch
                , CfEdge midLeft end UnconditionalBranch
                , CfEdge midRight end UnconditionalBranch
                ]
          output =
            InterCfg $
              mkCfg
                root'
                [midLeft', end']
                [ CfEdge root' midLeft' TrueBranch
                , CfEdge midLeft' end' UnconditionalBranch
                ]

      PrettyShow (focus midLeft input) `shouldBe` PrettyShow output

    it "should removed edges that skip focal node" $ do
      let root =
            bb
              fooCtx
              0
              1
              [ branchCond $ cmpNE (var "x" 4) (const 0 4) 4 ]
          midLeft =
            bb
              fooCtx
              1
              2
              [ def "y" (add (var "x" 4) (const 10 4) 4)
              ]
          end =
            bb
              fooCtx
              5
              6
              [ ret $ var "y" 4 ]
          input =
            InterCfg $
              mkCfg
                root
                [midLeft, end]
                [ CfEdge root midLeft TrueBranch
                , CfEdge root end FalseBranch
                , CfEdge midLeft end UnconditionalBranch
                ]
          output =
            InterCfg $
              mkCfg
                root
                [midLeft, end]
                [ CfEdge root midLeft TrueBranch
                , CfEdge midLeft end UnconditionalBranch
                ]

      PrettyShow (focus midLeft input) `shouldBe` PrettyShow output

  context "_simplify" $ do
    it "should leave an unsimplifiable ICFG untouched" $ do
      let root =
            bb
              fooCtx
              0
              1
              [ branchCond $ cmpNE (var "x" 4) (const 0 4) 4
              ]
          midLeft =
            bb
              fooCtx
              1
              2
              [ def "y#1" (add (var "x" 4) (const 10 4) 4)
              ]
          midRight =
            bb
              fooCtx
              3
              4
              [ def "y#2" (add (var "x" 4) (const 5000 4) 4)
              ]
          end =
            bb
              fooCtx
              5
              6
              [ defPhi "r" ["y#1", "y#2"]
              , ret $ var "r" 4
              ]
          input =
            InterCfg $
              mkCfg
                root
                [midLeft, midRight, end]
                [ CfEdge root midLeft TrueBranch
                , CfEdge root midRight FalseBranch
                , CfEdge midLeft end UnconditionalBranch
                , CfEdge midRight end UnconditionalBranch
                ]
          output = input

      PrettyShow (CfgA._simplify 1 input) `shouldBe` PrettyShow output

    it "should copy prop" $ do
      let root =
            bb
              fooCtx
              0
              1
              [ def "x" (var "w" 4)
              , branchCond $ cmpNE (var "x" 4) (const 0 4) 4
              ]
          midLeft =
            bb
              fooCtx
              1
              2
              [ def "y#1" (add (var "x" 4) (const 10 4) 4)
              ]
          midRight =
            bb
              fooCtx
              3
              4
              [ def "y#2" (add (var "x" 4) (const 5000 4) 4)
              ]
          end =
            bb
              fooCtx
              5
              6
              [ defPhi "r" ["y#1", "y#2"]
              , ret $ var "r" 4
              ]
          root' =
            bb
              fooCtx
              0
              1
              [ branchCond $ cmpNE (var "w" 4) (const 0 4) 4
              ]
          midLeft' =
            bb
              fooCtx
              1
              2
              [ def "y#1" (add (var "w" 4) (const 10 4) 4)
              ]
          midRight' =
            bb
              fooCtx
              3
              4
              [ def "y#2" (add (var "w" 4) (const 5000 4) 4)
              ]
          end' =
            bb
              fooCtx
              5
              6
              [ defPhi "r" ["y#1", "y#2"]
              , ret $ var "r" 4
              ]
          input =
            InterCfg $
              mkCfg
                root
                [midLeft, midRight, end]
                [ CfEdge root midLeft TrueBranch
                , CfEdge root midRight FalseBranch
                , CfEdge midLeft end UnconditionalBranch
                , CfEdge midRight end UnconditionalBranch
                ]
          output =
            InterCfg $
              mkCfg
                root'
                [midLeft', midRight', end']
                [ CfEdge root' midLeft' TrueBranch
                , CfEdge root' midRight' FalseBranch
                , CfEdge midLeft' end' UnconditionalBranch
                , CfEdge midRight' end' UnconditionalBranch
                ]
      PrettyShow (CfgA._simplify 1 input) `shouldBe` PrettyShow output


    it "should not constant prop" $ do
      let root =
            bb
              fooCtx
              0
              1
              [ def "x" (const 22 4)
              ]
          end =
            bb
              fooCtx
              5
              6
              [ ret $ add (const 88 4) (var "x" 4) 4
              ]
          input =
            InterCfg $
              mkCfg
                root
                [end]
                [ CfEdge root end UnconditionalBranch
                ]
          output = input
      PrettyShow (CfgA._simplify 1 input) `shouldBe` PrettyShow output

    it "should use constant prop to globally prune branch" $ do
      let root =
            bb
              fooCtx
              0
              1
              [ def "x" (const 0 4)
              , branchCond $ cmpE (var "x" 4) (const 0 4) 4
              ]
          midLeft =
            bb
              fooCtx
              1
              2
              [ def "y#1" (const 10 4)
              ]
          midRight =
            bb
              fooCtx
              3
              4
              [ def "y#2" (const 5000 4)
              ]
          end =
            bb
              fooCtx
              5
              6
              [ ret $ var "x" 4
              ]
          input =
            InterCfg $
              mkCfg
                root
                [midLeft, midRight, end]
                [ CfEdge root midLeft TrueBranch
                , CfEdge root midRight FalseBranch
                , CfEdge midLeft end UnconditionalBranch
                , CfEdge midRight end UnconditionalBranch
                ]
          output =
            InterCfg $
              mkCfg
                root
                [midLeft, midRight, end]
                [ CfEdge root midLeft TrueBranch
                , CfEdge midLeft end UnconditionalBranch
                ]

      PrettyShow (CfgA._simplify 1 input) `shouldBe` PrettyShow output


    it "should use constant from pruned branch to prune" $ do
      let root =
            bb
              fooCtx
              0
              1
              [ branchCond $ cmpE (var "x" 4) (const 0 4) 4
              ]
          midStart =
            bb
              fooCtx
              100
              101
              [ branchCond $ cmpE (var "x" 4) (const 0 4) 4
              ]
          midFalse =
            bb
              fooCtx
              1
              2
              [ def "y#1" (const 10 4)
              ]
          midTrue =
            bb
              fooCtx
              3
              4
              [ def "y#2" (const 5000 4)
              ]
          end =
            bb
              fooCtx
              5
              6
              [ ret $ var "x" 4
              ]
          input =
            InterCfg $
              mkCfg
                root
                [midStart, midTrue, midFalse, end]
                [ CfEdge root midStart TrueBranch
                , CfEdge midStart midTrue TrueBranch
                , CfEdge midStart midFalse FalseBranch
                , CfEdge midTrue end UnconditionalBranch
                , CfEdge midFalse end UnconditionalBranch
                ]
          output =
            InterCfg $
              mkCfg
                root
                [midStart, midTrue, end]
                [ CfEdge root midStart TrueBranch
                , CfEdge midStart midTrue TrueBranch
                , CfEdge midTrue end UnconditionalBranch
                ]

      PrettyShow (CfgA._simplify 1 input) `shouldBe` PrettyShow output
