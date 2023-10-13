module Flint.Analysis.Path.MatcherSpec
  ( module Flint.Analysis.Path.MatcherSpec
  ) where

import Flint.Prelude hiding (Symbol, sym, const)

import Flint.Analysis.Path.Matcher

import Blaze.Pil.Construct
import Blaze.Types.Function (Function(Function))
import qualified Blaze.Types.Pil as Pil

import Test.Hspec

path0 :: [Pil.Stmt]
path0 = []

path1 :: [Pil.Stmt]
path1 =
  [ def "b" (add (var "arg1" 4) (const 1 4) 4)
  , ret (const 0 4)
  ]

path2 :: [Pil.Stmt]
path2 =
  [ def "b" (load (var "arg4" 4) 4)
  , branchCond (cmpE (var "b" 4) (const 0 4) 4)
  , ret (const 0 4)
  ]

func0 :: Function
func0 = Function Nothing "func0" 0x888 []



spec :: Spec
spec = describe "Flint.Analysis.Path.Matcher" $ do
  context "matchStmts" $ do
    it "should match empty list of stmts when provided no patterns" $ do
      matchStmts' [] [] `shouldBe` MatchNoAssertions []

    it "should match full list of stmts when provided no patterns" $ do
      let stmts = path1
          pats = []
          expected = MatchNoAssertions path1
      matchStmts' pats stmts `shouldBe` expected

    it "should fail to match stmt pattern when there are no statements" $ do
      let stmts = []
          pats = [Stmt $ Def Wild Wild]
          expected = NoMatch
      matchStmts' pats stmts `shouldBe` expected

    it "should match on a def statement" $ do
      let stmts = [def "b" (load (var "arg4" 4) 4)]
          pats = [Stmt $ Def Wild Wild]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should match on an immediate" $ do
      let stmts = [def "b" (const 33 4)]
          pats = [Stmt $ Def Wild Immediate]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should match .== for integral CmpE" $ do
      let stmts = [branchCond $ cmpE (const 33 4) (const 33 4) 4]
          pats = [Stmt . BranchCond $ Wild .== Wild]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should match .== for float FcmpE" $ do
      let stmts = [branchCond $ fcmpE (fconst 33.0 4) (fconst 33.0 4) 4]
          pats = [Stmt . BranchCond $ Wild .== Wild]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should match .== for integral (Not (Not (CmpE ...)))" $ do
      let stmts = [branchCond $ cmpE (const 33 4) (const 33 4) 4]
          pats = [Stmt . BranchCond $ Wild .== Wild]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should match .== for integral (Not (CmpNe ...))" $ do
      let stmts = [branchCond $ cmpE (const 33 4) (const 33 4) 4]
          pats = [Stmt . BranchCond $ Wild .== Wild]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should match .== for float (Not (Not (fcmpE ...)))" $ do
      let stmts = [branchCond $ fcmpE (fconst 33.0 4) (fconst 33.0 4) 4]
          pats = [Stmt . BranchCond $ Wild .== Wild]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should match .== for float (Not (fcmpNe ...))" $ do
      let stmts = [branchCond $ fcmpE (fconst 33.0 4) (fconst 33.0 4) 4]
          pats = [Stmt . BranchCond $ Wild .== Wild]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should match on a var" $ do
      let stmts = [def "b" (load (var "arg4" 4) 4)]
          pats = [Stmt $ Def (Var "b") Wild]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should match on a ConstFuncPtr with Var" $ do
      let funcPtr = Pil.Expression 4
            . Pil.ConstFuncPtr
            . Pil.ConstFuncPtrOp 0x888
            $ Just "funcTable"
          stmts = [def "b" funcPtr]
          pats = [Stmt $ Def (Var "b") (Var "funcTable")]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should fail to match match a var if prefix of name is different" $ do
      let stmts = [def "b" (load (var "arg4" 4) 4)]
          pats = [Stmt $ Def (Var "a") Wild]
          expected = NoMatch
      matchStmts' pats stmts `shouldBe` expected

    it "should match an expression that Contains a variable" $ do
      let stmts = [def "b" (load (var "arg4" 4) 4)]
          pats = [Stmt $ Def Wild (Contains (Var "arg4"))]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should match a more complex expression that Contains a variable" $ do
      let stmts = [def "b" (load (add (var "arg4" 4) (const 44 4) 4) 4)]
          pats = [Stmt $ Def Wild (Contains (Var "arg4"))]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should match on two statements in a row" $ do
      let stmts = [ def "b" (load (var "arg4" 4) 4)
                  , def "c" (load (var "arg4" 4) 4)
                  ]
          pats = [ Stmt $ Def (Var "b") Wild
                 , Stmt $ Def (Var "c") Wild
                 ]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should match on two statements, skipping non-matching ones in the middle" $ do
      let stmts = [ def "b" (load (var "arg4" 4) 4)
                  , def "x" (const 0 4)
                  , def "y" (const 1 4)
                  , def "c" (load (var "arg4" 4) 4)
                  ]
          pats = [ Stmt $ Def (Var "b") Wild
                 , Stmt $ Def (Var "c") Wild
                 ]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should match an expression has been bound to sym" $ do
      let stmts = [ def "b" (load (var "arg4" 4) 4)
                  , def "c" (load (var "arg4" 4) 4)
                  ]
          pats = [ Stmt $ Def (Var "b") (Bind "x" Wild)
                 , Stmt $ Def (Var "c") (Bind "x" Wild)
                 ]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should fail to match an expression that has been bound to a different sym" $ do
      let stmts = [ def "b" (load (var "arg4" 4) 4)
                  , def "c" (load (var "arg5" 4) 4)
                  ]
          pats = [ Stmt $ Def (Var "b") (Bind "x" Wild)
                 , Stmt $ Def (Var "c") (Bind "x" Wild)
                 ]
          expected = NoMatch
      matchStmts' pats stmts `shouldBe` expected

    it "should skip over statement with expression that has been bound to different sym, but then match a later statement" $ do
      let stmts = [ def "b" (load (var "arg4" 4) 4)
                  , def "z" (const 0 4)
                  , def "c" (load (var "arg4" 4) 4)
                  ]
          pats = [ Stmt $ Def (Var "b") (Bind "x" Wild)
                 , Stmt $ Def Wild (Bind "x" Wild)
                 ]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should match on a call to a named function" $ do
      let cdest = Pil.CallFunc func0
          stmts = [ defCall "r" cdest [(var "a" 4), (load (var "arg4" 4) 4)] 8
                  ]
          pats = [ Stmt $ Call (Just Wild) (CallFunc (FuncName "func0")) [Wild, Wild]
                 ]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should not match on a call to a named function if args do not parse" $ do
      let cdest = Pil.CallFunc func0
          stmts = [ defCall "r" cdest [(var "a" 4), (load (var "arg4" 4) 4)] 8
                  ]
          pats = [ Stmt $ Call (Just Wild) (CallFunc (FuncName "func0"))
                   [Var "nope", Wild]
                 ]
          expected = NoMatch
      matchStmts' pats stmts `shouldBe` expected

    it "should match on a call to a named function with a return variable even if the pattern for the return variable is Nothing" $ do
      let cdest = Pil.CallFunc func0
          stmts = [ defCall "r" cdest [(var "a" 4), (load (var "arg4" 4) 4)] 8
                  ]
          pats = [ Stmt $ Call Nothing (CallFunc (FuncName "func0")) [Wild, Wild]
                 ]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected


    it "should match on an indirect call" $ do
      let cdest = Pil.CallExpr $ var "x" 4
          stmts = [ defCall "r" cdest [(var "a" 4), (load (var "arg4" 4) 4)] 8
                  ]
          pats = [ Stmt $ Call Nothing (CallIndirect $ Var "x") [Wild, Wild]
                 ]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should match on an indirect call using Contains" $ do
      let cdest = Pil.CallExpr $ load (add (var "x" 4) (const 1 4) 4) 4
          stmts = [ defCall "r" cdest [(var "a" 4), (load (var "arg4" 4) 4)] 8
                  ]
          pats = [ Stmt $ Call Nothing (CallIndirect . Contains $ Var "x") [Wild, Wild]
                 ]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should match on an expr in an indirect call to a const func ptr" $ do
      let funcPtr = Pil.Expression 4
            . Pil.ConstFuncPtr
            . Pil.ConstFuncPtrOp 0x888
            $ Just "funcTable"
          cdest = Pil.CallExpr $ load (add funcPtr (const 0x4e 4) 4) 4
          stmts = [ defCall "r" cdest [(var "a" 4), (load (var "arg4" 4) 4)] 8
                  ]
          pats = [ Stmt (Call Nothing (CallIndirect . Contains $ Var "funcTable") [Wild, Wild])
                 ]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should match on AnyOne" $ do
      let stmts = [ def "b" (const 0 4)
                  ]
          pats = [ AnyOne [ Stmt $ Def (Var "a") Wild
                          , Stmt $ Def (Var "b") Wild
                          ]
                 ]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should match Ordered statements" $ do
      let stmts = [ def "b" (const 0 4)
                  , def "skip" (const 1 4)
                  , def "c" (const 1 4)
                  , def "d" (const 1 4)
                  ]
          pats = [ Ordered [ Stmt $ Def (Var "b") Wild
                           , Stmt $ Def (Var "c") Wild
                           ]
                 , Stmt $ Def (Var "d") Wild
                 ]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should match on unordered statements" $ do
      let stmts = [ def "b" (const 0 4)
                  , def "c" (const 1 4)
                  ]
          pats = [ Unordered [ Stmt $ Def (Var "c") Wild
                             , Stmt $ Def (Var "b") Wild
                             ]
                 ]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should avoid single statement" $ do
      let stmts = [ def "b" (const 0 4)
                  ]
          pats = [ AvoidUntil $ AvoidSpec (Stmt $ Def (Var "b") Wild) Nothing
                 ]
          expected = NoMatch
      matchStmts' pats stmts `shouldBe` expected

    it "should avoid later statement" $ do
      let stmts = [ def "a" (const 0 4)
                  , def "b" (const 0 4)
                  ]
          pats = [ AvoidUntil $ AvoidSpec (Stmt $ Def (Var "b") Wild) Nothing
                 ]
          expected = NoMatch
      matchStmts' pats stmts `shouldBe` expected

    it "should avoid until" $ do
      let stmts = [ def "a" (const 0 4)
                  , def "wiff" (const 1 4)
                  , def "b" (const 0 4)
                  ]
          pats = [ AvoidUntil $ AvoidSpec
                   (Stmt $ Def (Var "b") Wild)
                   (Just . Stmt $ Def (Var "wiff") Wild)
                 ]
          expected = MatchNoAssertions stmts
      matchStmts' pats stmts `shouldBe` expected

    it "should make concrete assertion with empty stmt list" $ do
      let stmts = []
          mkConst = BoundExpr (ConstSize 4) . Pil.CONST . Pil.ConstOp
          pats = [ Assert
                   . BoundExpr (ConstSize 4)
                   . Pil.CMP_E
                   $ Pil.CmpEOp (mkConst 0) (mkConst 0)
                 ]
          stmts' = [constraint (cmpE (const 0 4) (const 0 4) 4)]
          expected = MatchWithAssertions stmts'
      matchStmts' pats stmts `shouldBe` expected

    it "should make assertion using bound vars" $ do
      let stmts = [ def "a" (const 0 4)
                  , def "b" (const 777 4)
                  ]
          pats = [ Stmt $ Def (Var "a") (Bind "x" Wild)
                 , Stmt $ Def (Var "b") (Bind "y" Wild)
                 , Assert
                   . BoundExpr (ConstSize 4)
                   . Pil.CMP_E
                   $ Pil.CmpEOp (Bound "x") (Bound "y")
                 ]
          stmts' = stmts <> [constraint (cmpE (const 0 4) (const 777 4) 4)]
          expected = MatchWithAssertions stmts'
      matchStmts' pats stmts `shouldBe` expected

      
