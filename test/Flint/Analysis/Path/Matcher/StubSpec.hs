{- HLINT ignore "Evaluate" -}

module Flint.Analysis.Path.Matcher.StubSpec where

import Flint.Prelude hiding (sym, const)

import Flint.Analysis.Path.Matcher
import Flint.Analysis.Path.Matcher.Stub

import Blaze.Pil.Construct
import Blaze.Types.Function (Function(Function))
import qualified Blaze.Types.Pil as Pil

import Test.Hspec


foo :: Function
foo = Function Nothing "foo" 0x999 []

bar :: Function
bar = Function Nothing "bar" 0x888 []

alloc :: Function
alloc = Function Nothing "alloc" 0x111 []

spec :: Spec
spec = describe "Flint.Analysis.Path.Matcher.Stub" $ do
  context "stubStmt" $ do
    it "should not stub a statement that doesn't match" $ do
      let nextVarId = StubMatcherState 0
          sspec = StubSpec
            { stmtToStub = Call Nothing (CallIndirect Wild) []
            , removeOriginalStmt = True
            , stubs = []
            }
          stmt = def "x" (const 50 8)
          expected = Nothing
      stubStmt nextVarId sspec stmt  `shouldBe` expected

    it "should stub a statement that matches" $ do
      let nextVarId = StubMatcherState 0
          sspec = StubSpec
            { stmtToStub = Call Nothing (CallFunc $ FuncName "foo") [Bind "x" Wild, Wild]
            , removeOriginalStmt = True
            , stubs = [store (bound "x") (const 0 (SizeOf "x"))]
            }
          stmt = defCall "r" (Pil.CallFunc foo) [var "a" 4, load (var "arg4" 4) 4] 8
          expected = Just (nextVarId, [store (var "a" 4) (const 0 4)])

      stubStmt nextVarId sspec stmt  `shouldBe` expected

  context "stubStmts" $ do
    let specs =
          [ StubSpec
            { stmtToStub = Call Nothing (CallFunc $ FuncName "foo") [Bind "x" Wild, Wild]
            , removeOriginalStmt = True
            , stubs = [store (bound "x") (const 0 (SizeOf "x"))]
            }
          , StubSpec
            { stmtToStub = Call Nothing (CallFunc $ FuncName "bar") [Bind "x" Wild, Bind "y" Wild]
            , removeOriginalStmt = False
            , stubs = [constraint $ cmpUlt (bound "x") (bound "y") (ConstSize 8)]
            }
          , StubSpec
            { stmtToStub = Call Nothing (CallFunc $ FuncName "alloc") [Bind "x" Wild, Wild]
            , removeOriginalStmt = True
            , stubs = [ store (bound "x") (newVar "free" (SizeOf "x"))
                      , constraint $ cmpNE (bound "free") (const 0 (SizeOf "free"))
                        (ConstSize 8)
                      ]
            }
          ]

    it "should not stub empty list" $ do
      let stmts = []
          expected = []
      stubStmts specs stmts `shouldBe` expected

    it "should not stub non-matching stmts" $ do
      let stmts = [ def "x" (const 50 8)
                  , ret (var "x" 8)
                  ]
          expected = stmts
      stubStmts specs stmts `shouldBe` expected

    it "should stub matching function call stmts" $ do
      let stmts =
            [ def "var_20" (const 50 8)
            , defCall "r" (Pil.CallFunc foo) [var "var_20" 8, load (var "arg4" 8) 8] 8
            , ret (var "r" 8)
            ]
          expected =
            [ def "var_20" (const 50 8)
            , store (var "var_20" 8) (const 0 8)
            , ret (var "r" 8)
            ]
      stubStmts specs stmts `shouldBe` expected

    it "should optionally leave in matching statement with stub" $ do
      let stmts =
            [ def "var_20" (const 50 8)
            , defCall "r" (Pil.CallFunc bar) [var "var_20" 8, var "arg4" 8] 8
            , ret (var "r" 8)
            ]
          expected =
            [ def "var_20" (const 50 8)
            , defCall "r" (Pil.CallFunc bar) [var "var_20" 8, var "arg4" 8] 8
            , constraint $ cmpUlt (var "var_20" 8) (var "arg4" 8) 8
            , ret (var "r" 8)
            ]
      stubStmts specs stmts `shouldBe` expected

    it "should stub matching stmt twice" $ do
      let stmts =
            [ def "var_20" (const 50 8)
            , defCall "r" (Pil.CallFunc foo) [var "var_20" 8, load (var "arg4" 8) 8] 8
            , def "y" (add (var "r" 8) (const 88 8) 8)
            , defCall "r2" (Pil.CallFunc foo) [var "y" 8, load (var "arg4" 8) 8] 8
            , ret (var "r" 8)
            ]
          expected =
            [ def "var_20" (const 50 8)
            , store (var "var_20" 8) (const 0 8)
            , def "y" (add (var "r" 8) (const 88 8) 8)
            , store (var "y" 8) (const 0 8)
            , ret (var "r" 8)
            ]
      stubStmts specs stmts `shouldBe` expected

    it "should create new var for stub" $ do
      let stmts =
            [ def "var_20" (const 50 8)
            , defCall "r" (Pil.CallFunc alloc) [var "var_20" 8, load (var "arg4" 8) 8] 8
            , ret (var "r" 8)
            ]
          expected =
            [ def "var_20" (const 50 8)
            , store (var "var_20" 8) (var "free_0" 8)
            , constraint $ cmpNE (var "free_0" 8) (const 0 8) 8
            , ret (var "r" 8)
            ]
      stubStmts specs stmts `shouldBe` expected

    it "should create new var for stub twice without conflict" $ do
      let stmts =
            [ def "var_20" (const 50 8)
            , defCall "r" (Pil.CallFunc alloc) [var "var_20" 8, load (var "arg4" 8) 8] 8
            , def "y" (add (var "r" 8) (const 88 8) 8)
            , defCall "r2" (Pil.CallFunc alloc) [var "y" 8, load (var "arg4" 8) 8] 8
            , ret (var "r2" 8)
            ]
          expected =
            [ def "var_20" (const 50 8)
            , store (var "var_20" 8) (var "free_0" 8)
            , constraint $ cmpNE (var "free_0" 8) (const 0 8) 8
            , def "y" (add (var "r" 8) (const 88 8) 8)
            , store (var "y" 8) (var "free_1" 8)
            , constraint $ cmpNE (var "free_1" 8) (const 0 8) 8
            , ret (var "r2" 8)
            ]
      stubStmts specs stmts `shouldBe` expected
