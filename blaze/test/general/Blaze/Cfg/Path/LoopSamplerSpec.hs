{- HLINT ignore "Redundant do" -}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Blaze.Cfg.Path.LoopSamplerSpec where

import Blaze.Prelude hiding (const, Symbol)

import Test.Hspec

import Blaze.Cfg (BranchType(UnconditionalBranch, TrueBranch, FalseBranch), PilNode, mkCfg)
import qualified Blaze.Cfg as Cfg
import Blaze.Cfg.Path.LoopSampler
import Blaze.Types.Pil (Ctx(Ctx), PilVar, Stmt, Expression)
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Pil.Construct as C
import Blaze.Util.Spec (bb)
import Blaze.Function (Function(Function))
import qualified Blaze.Path as Path
import Blaze.Path (emptyVisitCounts)
import qualified Blaze.Types.Graph as G

import qualified Blaze.Pil.Analysis as PA
import qualified Data.HashSet as HashSet


-----------------------------------------------------------------------
-- Test helpers
-----------------------------------------------------------------------

ctx0 :: Ctx
ctx0 = Ctx (Function Nothing "testFunc" (intToAddr 0) []) 0 False

sz :: Pil.Size Expression
sz = 8

pvSz :: Pil.Size PilVar
pvSz = 8

mkPilVar :: Text -> Maybe Int -> PilVar
mkPilVar name mVer = C.pilVar__ pvSz (Just ctx0) mVer name False Pil.UnknownLocation

-- | Make a BasicBlock PilNode from an address and statements.
mkBB :: Int64 -> [Stmt] -> PilNode
mkBB addr = bb ctx0 (intToAddr addr) (intToAddr $ addr + 1)

-- | Build a simple loop CFG:
--
--   [entry] --Unconditional--> [header] --True--> [body] --Unconditional--> [header] (backedge)
--                                |
--                                +--False--> [exit]
--
--   entry:  x#0 = 0
--   header: x#1 = phi(x#0, x#2)
--           ?: x#1 < 100
--   body:   [y + x#1] = 0
--           x#2 = x#1 + 1
--   exit:   ret 0
--
mkSimpleLoopCfg :: (PilNode, PilNode, PilNode, PilNode, Cfg.Cfg PilNode)
mkSimpleLoopCfg = (entryNode, headerNode, bodyNode, exitNode, cfg)
  where
    x0 = mkPilVar "x" (Just 0)
    x1 = mkPilVar "x" (Just 1)
    x2 = mkPilVar "x" (Just 2)
    y  = mkPilVar "y" Nothing

    entryNode = mkBB 0x1000
      [ C.def' x0 (C.const 0 sz)
      ]

    headerNode = mkBB 0x2000
      [ C.defPhi' x1 [x0, x2]
      , C.branchCond (C.cmpSlt (C.var' x1 sz) (C.const 100 sz) sz)
      ]

    bodyNode = mkBB 0x3000
      [ C.store (C.add (C.var' y sz) (C.var' x1 sz) sz) (C.const 0 sz)
      , C.def' x2 (C.add (C.var' x1 sz) (C.const 1 sz) sz)
      ]

    exitNode = mkBB 0x4000
      [ C.ret (C.const 0 sz)
      ]

    cfg = mkCfg 0 entryNode
      [headerNode, bodyNode, exitNode]
      [ Cfg.CfEdge entryNode headerNode UnconditionalBranch
      , Cfg.CfEdge headerNode bodyNode TrueBranch
      , Cfg.CfEdge headerNode exitNode FalseBranch
      , Cfg.CfEdge bodyNode headerNode UnconditionalBranch  -- backedge
      ]


-----------------------------------------------------------------------
-- Tests
-----------------------------------------------------------------------

spec :: Spec
spec = describe "Blaze.Cfg.Path.LoopSampler" $ do

  let (entryNode, headerNode, bodyNode, exitNode, cfg) = mkSimpleLoopCfg
      x0 = mkPilVar "x" (Just 0)
      x1 = mkPilVar "x" (Just 1)
      x2 = mkPilVar "x" (Just 2)

  describe "detectInductionVar" $ do
    let preLoopVars = HashSet.fromList [x0]
        loopBody = [(TrueBranch, headerNode), (UnconditionalBranch, bodyNode)]

    it "should detect x#1 as an induction variable with stride +1" $ do
      let result = detectInductionVar headerNode loopBody preLoopVars
      result `shouldSatisfy` isJust
      let Just iv = result
      iv ^. #phiDest `shouldBe` x1
      iv ^. #initVal `shouldBe` x0
      iv ^. #updateVal `shouldBe` x2
      iv ^. #stride `shouldBe` 1
      iv ^. #loopVar . #symbol `shouldBe` "x_looping"

  describe "extractBound" $ do
    it "should extract bound 100 with SLT from the header" $ do
      let result = extractBound x1 headerNode
      result `shouldSatisfy` isJust
      let Just bi = result
      bi ^. #boundOp `shouldBe` BoundSLT

  describe "buildLoopSummary" $ do
    let preLoopVars = HashSet.fromList [x0]
        loopBody = [(TrueBranch, headerNode), (UnconditionalBranch, bodyNode)]
        Just indVar = detectInductionVar headerNode loopBody preLoopVars
        bound = extractBound (indVar ^. #phiDest) headerNode
        summary = buildLoopSummary indVar bound (intToAddr 0x2000) ctx0 1 loopBody

    it "should not contain any DefPhi statements" $ do
      let hasPhi (Pil.Stmt _ (Pil.DefPhi _)) = True
          hasPhi _ = False
      any hasPhi summary `shouldBe` False

    it "should not contain BranchCond statements" $ do
      let hasBranch (Pil.Stmt _ (Pil.BranchCond _)) = True
          hasBranch _ = False
      any hasBranch summary `shouldBe` False

    it "should contain Constraint statements for entry and exit bounds" $ do
      let isConstraint (Pil.Stmt _ (Pil.Constraint _)) = True
          isConstraint _ = False
          constraints = filter isConstraint summary
      -- Should have: v_loop >= x#0, v_loop < 100, x_exit >= 100
      length constraints `shouldBe` 3

    it "should contain a Store statement (the rewritten loop body)" $ do
      let isStore (Pil.Stmt _ (Pil.Store _)) = True
          isStore _ = False
      any isStore summary `shouldBe` True

    it "should not contain the induction variable update definition" $ do
      let isUpdateDef (Pil.Stmt _ (Pil.Def (Pil.DefOp dest _))) = dest == x2
          isUpdateDef _ = False
      any isUpdateDef summary `shouldBe` False

  describe "sampleWithLoopSummarization" $ do
    it "should produce a path through a simple loop CFG" $ do
      let dmap = G.calcStrictDescendantsMap cfg
      result <- sampleWithLoopSummarizationIO
        dmap [exitNode] emptyVisitCounts
        entryNode cfg
      result `shouldSatisfy` isRight
      let Right (path, _vc) = result
          pathNodes = HashSet.toList $ Path.nodes path
      -- Path should have nodes (entry, summary, exit — at minimum)
      length pathNodes `shouldSatisfy` (>= 2)

    it "should not contain _loopN variable names in the path" $ do
      let dmap = G.calcStrictDescendantsMap cfg
      result <- sampleWithLoopSummarizationIO
        dmap [exitNode] emptyVisitCounts
        entryNode cfg
      let Right (path, _) = result
          allStmts = concatMap Cfg.getNodeData . HashSet.toList $ Path.nodes path
          allVars = PA.getDefinedVars allStmts
          hasLoopN pv = "_loop" `isPrefixOf` (drop 1 . dropWhile (/= '_') . show $ pv ^. #symbol)
                     && "_loop" /= (pv ^. #symbol)
      -- No variable should have _loopN suffix (old unrolling artifact)
      any hasLoopN (HashSet.toList allVars) `shouldBe` False
