module Blaze.Cfg.Path.SolverSpec where

import Blaze.Prelude

import Blaze.Cfg (BranchType(TrueBranch, FalseBranch))
import Test.Hspec
import Blaze.Types.Pil (Ctx(Ctx))
import qualified Blaze.Pil.Construct as C
import qualified Blaze.Cfg.Path as Path
import Blaze.Types.Path ((-|), (|-))
import Blaze.Cfg.Path.Solver
import Blaze.Cfg.PathSpec (bbp, func1)


spec :: Spec
spec = describe "Blaze.Cfg.Path.Solver" $ do
  context "solvePathWith_ z3" $ do
    let solvePath = solvePathWith_ z3 AbortOnError

    it "should solve path with a single empty basic block" $ do
      let ctx = Ctx func1 0
          bb0 = bbp ctx "bb0" []
          path = Path.build 1 $ Path.start bb0
      (is #_Sat <$> solvePath path) `shouldReturn` True

    it "should find path branch constraint depending on branch type" $ do
      let ctx = Ctx func1 0
          bb0 = bbp ctx "bb0"
            [ C.def "x" $ C.const 54 0x8
            , C.branchCond $ C.cmpE (C.var "x" 8) (C.const 0 8) 8
            ]
          bb1 = bbp ctx "bb1"
            [ C.ret $ C.var "x" 8 ]
          pathTrue = Path.build 1 $ Path.start bb0 -| TrueBranch |- bb1
          pathFalse = Path.build 1 $ Path.start bb0 -| FalseBranch |- bb1
      (is #_Unsat <$> solvePath pathTrue) `shouldReturn` True
      (is #_Sat <$> solvePath pathFalse) `shouldReturn` True

    it "should handle path with single store and load" $ do
      let ctx = Ctx func1 0
          bb0 = bbp ctx "bb0"
            [ C.store (C.var "x" 8) $ C.const 54 0x8
            , C.branchCond $ C.cmpE (C.load (C.var "x" 8) 8) (C.const 54 8) 8
            ]
          bb1 = bbp ctx "bb1"
            [ C.ret $ C.var "x" 8 ]
          pathFalse = Path.build 1 $ Path.start bb0 -| FalseBranch |- bb1
          pathTrue = Path.build 1 $ Path.start bb0 -| TrueBranch |- bb1
      (is #_Sat <$> solvePath pathTrue) `shouldReturn` True
      (is #_Unsat <$> solvePath pathFalse) `shouldReturn` True

    it "should handle path with sequential stores to same var" $ do
      let ctx = Ctx func1 0
          bb0 = bbp ctx "bb0"
            [ C.store (C.var "x" 8) $ C.const 54 0x8
            , C.branchCond $ C.cmpE (C.load (C.var "x" 8) 8) (C.const 54 8) 8
            ]
          bb1 = bbp ctx "bb1"
            [ C.store (C.var "x" 8) $ C.const 888 0x8
            , C.branchCond $ C.cmpE (C.load (C.var "x" 8) 8) (C.const 888 8) 8
            ]
          bb2 = bbp ctx "bb2"
            [ C.ret $ C.var "x" 8 ]
          path = Path.build 1 $ Path.start bb0 -| TrueBranch |- bb1 -| TrueBranch |- bb2
      (is #_Sat <$> solvePath path) `shouldReturn` True

    it "should handle path with sequential stores to multiple vars" $ do
      let ctx = Ctx func1 0
          a = C.var "a" 8
          x = C.var "x" 8
          y = C.var "y" 8
          loadx = C.load x 8
          loady = C.load y 8
          bb0 = bbp ctx "bb0"
            [ C.store x a
            , C.store y $ C.add loadx (C.const 1 8) 8
            , C.branchCond $ C.cmpE loady loadx 8
            ]
          bb1 = bbp ctx "bb1"
            [ C.def "xval" loadx
            , C.store x $ C.sub loady (C.const 1 8) 8
            , C.branchCond $ C.cmpE (C.var "xval" 8) loadx 8
            ]
          bb2 = bbp ctx "bb2"
            [ C.ret $ C.var "a" 8 ]
          path = Path.build 1 $ Path.start bb0 -| FalseBranch |- bb1 -| TrueBranch |- bb2
      (is #_Sat <$> solvePath path) `shouldReturn` True

