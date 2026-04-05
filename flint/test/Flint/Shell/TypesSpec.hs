{- HLINT ignore "Evaluate" -}

module Flint.Shell.TypesSpec where

import Flint.Prelude hiding (const, sym)

import Flint.Shell.Types

import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Function (Function(Function), FuncParamInfo(FuncParamInfo), ParamInfo(ParamInfo))
import qualified Blaze.Types.Function as Func

import qualified Data.HashSet as HashSet

import Test.Hspec


spec :: Spec
spec = describe "Flint.Shell.Types" $ do
  describe "parseSuffix" $ do
    it "parses empty string as ViewReduced" $
      parseSuffix "" `shouldBe` ViewReduced

    it "parses ! as ViewRaw" $
      parseSuffix "!" `shouldBe` ViewRaw

    it "parses !! as ViewContextStripped" $
      parseSuffix "!!" `shouldBe` ViewContextStripped

    it "parses unknown suffix as ViewReduced" $
      parseSuffix "x" `shouldBe` ViewReduced

  describe "parseRefPart" $ do
    it "parses plain number" $
      parseRefPart "3" `shouldBe` [PathRef 3 ViewReduced]

    it "parses number with ! suffix" $
      parseRefPart "3!" `shouldBe` [PathRef 3 ViewRaw]

    it "parses number with !! suffix" $
      parseRefPart "3!!" `shouldBe` [PathRef 3 ViewContextStripped]

    it "parses dotdot range" $
      parseRefPart "1..3" `shouldBe`
        [PathRef 1 ViewReduced, PathRef 2 ViewReduced, PathRef 3 ViewReduced]

    it "parses dash range" $
      parseRefPart "2-4" `shouldBe`
        [PathRef 2 ViewReduced, PathRef 3 ViewReduced, PathRef 4 ViewReduced]

    it "returns empty for non-numeric" $
      parseRefPart "abc" `shouldBe` []

  describe "parsePathRefs" $ do
    it "parses multiple space-separated refs" $
      parsePathRefs ["0", "1!", "2!!"] `shouldBe`
        [ PathRef 0 ViewReduced
        , PathRef 1 ViewRaw
        , PathRef 2 ViewContextStripped
        ]

    it "parses bracket-delimited list" $
      parsePathRefs ["[0,", "1!,", "2!!]"] `shouldBe`
        [ PathRef 0 ViewReduced
        , PathRef 1 ViewRaw
        , PathRef 2 ViewContextStripped
        ]

    it "parses range within brackets" $
      parsePathRefs ["[0..2]"] `shouldBe`
        [ PathRef 0 ViewReduced
        , PathRef 1 ViewReduced
        , PathRef 2 ViewReduced
        ]

  describe "resolveStmts" $ do
    let nopStmt addr = Pil.Stmt (intToAddr addr) Pil.Nop

        dummyFunc :: Function
        dummyFunc = Function Nothing "target" (intToAddr 0x1000)
          [ FuncParamInfo $ ParamInfo "arg0" Nothing Func.In ]

        -- Simulate a path with caller context: caller stmts at 0x2000,
        -- target stmts at 0x1000-0x1010
        callerStmt = nopStmt 0x2000
        targetStmt1 = nopStmt 0x1000
        targetStmt2 = nopStmt 0x1008
        targetStmt3 = nopStmt 0x1010

        allStmts = [callerStmt, targetStmt1, targetStmt2, targetStmt3]
        targetAddrs = HashSet.fromList
          [intToAddr 0x1000, intToAddr 0x1008, intToAddr 0x1010]

        ctxInfo = CallerContext
          { innerStmtAddrs = targetAddrs
          , resolvedParams = [("arg0", Pil.Expression 8 (Pil.CONST (Pil.ConstOp 42)))]
          }

        cp = CachedPath
          { pilPath = allStmts
          , fullPath = error "fullPath not used in this test"
          , sourceFunc = dummyFunc
          , pathPrep = Nothing  -- No prep means reduced = raw
          , pathPrepTaintVersion = 0
          , callerContext = Just ctxInfo
          }

        cpNoContext = cp { callerContext = Nothing }

    it "ViewRaw returns raw pilPath" $
      resolveStmts cp ViewRaw `shouldBe` allStmts

    it "ViewReduced without pathPrep falls back to raw" $
      resolveStmts cp ViewReduced `shouldBe` allStmts

    it "ViewContextStripped filters to target function addresses" $
      resolveStmts cp ViewContextStripped `shouldBe`
        [targetStmt1, targetStmt2, targetStmt3]

    it "ViewContextStripped without caller context falls back to reduced" $
      resolveStmts cpNoContext ViewContextStripped `shouldBe` allStmts
