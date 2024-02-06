{- HLINT ignore "Evaluate" -}

module Flint.Analysis.Path.MatcherSpec
  ( module Flint.Analysis.Path.MatcherSpec
  ) where

import Flint.Prelude hiding (sym, const)

import Flint.Types.Analysis (TaintPropagator(..), Parameter (Parameter, ReturnParameter))
import Flint.Analysis.Path.Matcher

import Blaze.Pil.Construct
import Blaze.Pil.Solver (solveStmtsWithZ3)
import qualified Blaze.Pil.Solver as Solver
import Blaze.Types.Function (Function(Function))
import qualified Blaze.Types.Pil as Pil

import qualified Data.HashMap.Strict as HashMap

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
      pureMatchStmts' [] [] [] `shouldBe` Match []

    it "should match full list of stmts when provided no patterns" $ do
      let stmts = path1
          pats = []
          expected = Match path1
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should fail to match stmt pattern when there are no statements" $ do
      let stmts = []
          pats = [Stmt $ Def Wild Wild]
          expected = NoMatch
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match on a def statement" $ do
      let stmts = [def "b" (load (var "arg4" 4) 4)]
          pats = [Stmt $ Def Wild Wild]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match on an immediate" $ do
      let stmts = [def "b" (const 33 4)]
          pats = [Stmt $ Def Wild Immediate]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match .== for integral CmpE" $ do
      let stmts = [branchCond $ cmpE (const 33 4) (const 33 4) 4]
          pats = [Stmt . BranchCond $ Wild .== Wild]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match .== for float FcmpE" $ do
      let stmts = [branchCond $ fcmpE (fconst 33.0 4) (fconst 33.0 4) 4]
          pats = [Stmt . BranchCond $ Wild .== Wild]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match .== for integral (Not (Not (CmpE ...)))" $ do
      let stmts = [branchCond $ cmpE (const 33 4) (const 33 4) 4]
          pats = [Stmt . BranchCond $ Wild .== Wild]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match .== for integral (Not (CmpNe ...))" $ do
      let stmts = [branchCond $ cmpE (const 33 4) (const 33 4) 4]
          pats = [Stmt . BranchCond $ Wild .== Wild]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match .== for float (Not (Not (fcmpE ...)))" $ do
      let stmts = [branchCond $ fcmpE (fconst 33.0 4) (fconst 33.0 4) 4]
          pats = [Stmt . BranchCond $ Wild .== Wild]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match .== for float (Not (fcmpNe ...))" $ do
      let stmts = [branchCond $ fcmpE (fconst 33.0 4) (fconst 33.0 4) 4]
          pats = [Stmt . BranchCond $ Wild .== Wild]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match on a var" $ do
      let stmts = [def "b" (load (var "arg4" 4) 4)]
          pats = [Stmt $ Def (Var "b") Wild]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match on a ConstFuncPtr with Var" $ do
      let funcPtr = Pil.Expression 4
            . Pil.ConstFuncPtr
            . Pil.ConstFuncPtrOp 0x888
            $ Just "funcTable"
          stmts = [def "b" funcPtr]
          pats = [Stmt $ Def (Var "b") (Var "funcTable")]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should fail to match match a var if prefix of name is different" $ do
      let stmts = [def "b" (load (var "arg4" 4) 4)]
          pats = [Stmt $ Def (Var "a") Wild]
          expected = NoMatch
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match an expression that Contains a variable" $ do
      let stmts = [def "b" (load (var "arg4" 4) 4)]
          pats = [Stmt $ Def Wild (Contains (Var "arg4"))]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match first match in OrPattern" $ do
      let stmts = [def "b" (load (var "arg4" 4) 4)]
          pats = [Stmt $ Def (Var "b" .|| Var "a") Wild]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match second match in OrPattern" $ do
      let stmts = [def "b" (load (var "arg4" 4) 4)]
          pats = [Stmt $ Def (Var "a" .|| Var "b") Wild]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match a more complex expression that Contains a variable" $ do
      let stmts = [def "b" (load (add (var "arg4" 4) (const 44 4) 4) 4)]
          pats = [Stmt $ Def Wild (Contains (Var "arg4"))]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match on two statements in a row" $ do
      let stmts = [ def "b" (load (var "arg4" 4) 4)
                  , def "c" (load (var "arg4" 4) 4)
                  ]
          pats = [ Stmt $ Def (Var "b") Wild
                 , Stmt $ Def (Var "c") Wild
                 ]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match on two statements, skipping non-matching ones in the middle" $ do
      let stmts = [ def "b" (load (var "arg4" 4) 4)
                  , def "x" (const 0 4)
                  , def "y" (const 1 4)
                  , def "c" (load (var "arg4" 4) 4)
                  ]
          pats = [ Stmt $ Def (Var "b") Wild
                 , Stmt $ Def (Var "c") Wild
                 ]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match an expression has been bound to sym" $ do
      let stmts = [ def "b" (load (var "arg4" 4) 4)
                  , def "c" (load (var "arg4" 4) 4)
                  ]
          pats = [ Stmt $ Def (Var "b") (Bind "x" Wild)
                 , Stmt $ Def (Var "c") (Bind "x" Wild)
                 ]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should fail to match an expression that has been bound to a different sym" $ do
      let stmts = [ def "b" (load (var "arg4" 4) 4)
                  , def "c" (load (var "arg5" 4) 4)
                  ]
          pats = [ Stmt $ Def (Var "b") (Bind "x" Wild)
                 , Stmt $ Def (Var "c") (Bind "x" Wild)
                 ]
          expected = NoMatch
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should skip over statement with expression that has been bound to different sym, but then match a later statement" $ do
      let stmts = [ def "b" (load (var "arg4" 4) 4)
                  , def "z" (const 0 4)
                  , def "c" (load (var "arg4" 4) 4)
                  ]
          pats = [ Stmt $ Def (Var "b") (Bind "x" Wild)
                 , Stmt $ Def Wild (Bind "x" Wild)
                 ]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match on a call to a named function" $ do
      let cdest = Pil.CallFunc func0
          stmts = [ defCall "r" cdest [var "a" 4, load (var "arg4" 4) 4] 8
                  ]
          pats = [ Stmt $ Call (Just Wild) (CallFunc (FuncName "func0")) [Wild, Wild]
                 ]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should not match on a call to a named function if args do not parse" $ do
      let cdest = Pil.CallFunc func0
          stmts = [ defCall "r" cdest [var "a" 4, load (var "arg4" 4) 4] 8
                  ]
          pats = [ Stmt $ Call (Just Wild) (CallFunc (FuncName "func0"))
                   [Var "nope", Wild]
                 ]
          expected = NoMatch
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should not match on a call that has fewer args than expected" $ do
      let cdest = Pil.CallFunc func0
          stmts = [ defCall "r" cdest [var "a" 4] 8
                  ]
          pats = [ Stmt $ Call (Just Wild) (CallFunc (FuncName "func0")) [Wild, Wild]
                 ]
          expected = NoMatch
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match on a call to a named function with a return variable even if the pattern for the return variable is Nothing" $ do
      let cdest = Pil.CallFunc func0
          stmts = [ defCall "r" cdest [var "a" 4, load (var "arg4" 4) 4] 8
                  ]
          pats = [ Stmt $ Call Nothing (CallFunc (FuncName "func0")) [Wild, Wild]
                 ]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match on an indirect call" $ do
      let cdest = Pil.CallExpr $ var "x" 4
          stmts = [ defCall "r" cdest [var "a" 4, load (var "arg4" 4) 4] 8
                  ]
          pats = [ Stmt $ Call Nothing (CallIndirect $ Var "x") [Wild, Wild]
                 ]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match on an indirect call using Contains" $ do
      let cdest = Pil.CallExpr $ load (add (var "x" 4) (const 1 4) 4) 4
          stmts = [ defCall "r" cdest [var "a" 4, load (var "arg4" 4) 4] 8
                  ]
          pats = [ Stmt $ Call Nothing (CallIndirect . Contains $ Var "x") [Wild, Wild]
                 ]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match on an expr in an indirect call to a const func ptr" $ do
      let funcPtr = Pil.Expression 4
            . Pil.ConstFuncPtr
            . Pil.ConstFuncPtrOp 0x888
            $ Just "funcTable"
          cdest = Pil.CallExpr $ load (add funcPtr (const 0x4e 4) 4) 4
          stmts = [ defCall "r" cdest [var "a" 4, load (var "arg4" 4) 4] 8
                  ]
          pats = [ Stmt (Call Nothing (CallIndirect . Contains $ Var "funcTable") [Wild, Wild])
                 ]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should match on AnyOne" $ do
      let stmts = [ def "b" (const 0 4)
                  ]
          pats = [ AnyOne [ Stmt $ Def (Var "a") Wild
                          , Stmt $ Def (Var "b") Wild
                          ]
                 ]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

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
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should backtrack on Ordered statements until it finds a match" $ do
      let stmts = [ def "a" (const 1 8)
                  , def "b" (const 2 8)
                  , def "c" (const 3 8)
                  , def "d" (const 2 8)
                  ]
          pats = [ Ordered [ Stmt $ Def (Bind "dest1" Wild) (Bind "x" Wild)
                           , Stmt $ Def (Bind "dest2" Wild) (Bind "x" Wild)
                           ]
                 ]
          expected = Match stmts
          (ms, mr) = pureMatchStmts [] pats stmts
      mr `shouldBe` expected
      HashMap.lookup "x" (ms ^. #boundSyms) `shouldBe` Just (const 2 8)
      HashMap.lookup "dest1" (ms ^. #boundSyms) `shouldBe` Just (var "b" 8)
      HashMap.lookup "dest2" (ms ^. #boundSyms) `shouldBe` Just (var "d" 8)

    it "should backtrack on top-level patterns until it finds a match" $ do
      let stmts = [ def "a" (const 1 8)
                  , def "b" (const 2 8)
                  , def "c" (const 3 8)
                  , def "d" (const 2 8)
                  ]
          pats = [ Stmt $ Def (Bind "dest1" Wild) (Bind "x" Wild)
                 , Stmt $ Def (Bind "dest2" Wild) (Bind "x" Wild)      
                 ]
          expected = Match stmts
          (ms, mr) = pureMatchStmts [] pats stmts
      mr `shouldBe` expected
      HashMap.lookup "x" (ms ^. #boundSyms) `shouldBe` Just (const 2 8)
      HashMap.lookup "dest1" (ms ^. #boundSyms) `shouldBe` Just (var "b" 8)
      HashMap.lookup "dest2" (ms ^. #boundSyms) `shouldBe` Just (var "d" 8)

    it "should match on unordered statements" $ do
      let stmts = [ def "b" (const 0 4)
                  , def "c" (const 1 4)
                  ]
          pats = [ Unordered [ Stmt $ Def (Var "c") Wild
                             , Stmt $ Def (Var "b") Wild
                             ]
                 ]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should avoid until" $ do
      let stmts = [ def "a" (const 0 4)
                  , def "wiff" (const 1 4)
                  , def "b" (const 0 4)
                  ]
          pats = [ AvoidUntil $ AvoidSpec
                   (Stmt $ Def (Var "b") Wild)
                   (Stmt $ Def (Var "wiff") Wild)
                 ]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should fail if Until not reached" $ do
      let stmts = [ def "a" (const 0 4)
                  , def "wiff" (const 1 4)
                  , def "b" (const 0 4)
                  ]
          pats = [ AvoidUntil $ AvoidSpec
                   { avoid = Stmt $ Def (Var "b") Wild
                   , until = Stmt $ Def (Var "c") Wild
                   }
                 ]
          expected = NoMatch
      pureMatchStmts' [] pats stmts `shouldBe` expected

    it "should fail if avoid is reached before until" $ do
      -- The problem here currently is that if the avoid matches the first stmt
      -- it moves on and tries from the second, forgetting that the first failed big time
      let stmts = [ def "a" (const 0 4)
                  , def "wiff" (const 1 4)
                  , def "b" (const 0 4)
                  ]
          pats = [ AvoidUntil $ AvoidSpec
                   { avoid = Stmt $ Def (Var "a") Wild
                   , until = Stmt $ Def (Var "b") Wild
                   }
                 ]
          expected = NoMatch
      pureMatchStmts' [] pats stmts `shouldBe` expected


    it "should find the 'until' first, then backtrack and check the 'avoid' after" $ do
      -- If AvoidUntil is implemented without backtracking, it will bind "ptr" to the var named "x"
      -- Then it will fail because (Bind "ptr" Wild) in the Until part doesn't bind to "x"
      let stmts = [ constraint $ cmpSlt (load (var "x" 8) 8) (const 888 8) 8
                  , store (var "a" 8) $ add (load (var "a" 8) 8) (const 1 8) 8
                  ]
          pats = [ AvoidUntil $ AvoidSpec
                   { avoid = Stmt . Constraint $ load (Bind "ptr" Wild) () .< Wild
                   , until = Stmt $ Store (Bind "ptr" Wild)
                             $ add (load (Bind "ptr" Wild) ()) Wild ()
                   }
                 ]
          expected = Match stmts
      pureMatchStmts' [] pats stmts `shouldBe` expected

    context "assertions" $ do
      let matchStmtsIO = matchStmts' (solveStmtsWithZ3 Solver.AbortOnError)

      it "should make assertion using bound vars" $ do
        let stmts = [ def "a" (const 0 4)
                    , def "b" (const 777 4)
                    ]
            pats = [ Stmt $ Def (Var "a") (Bind "x" Wild)
                   , Stmt (Def (Var "b") (Bind "y" Wild))
                     `Where`
                     [ cmpNE (Bound "x") (Bound "y") (ConstSize 4) ]
                   ]
            stmts' = stmts <> [constraint (cmpNE (const 0 4) (const 777 4) 4)]
            expected = Match stmts'
        matchStmtsIO [] pats stmts `shouldReturn` expected

      it "should fail if assertion fails" $ do
        let stmts = [ def "a" (const 0 4)
                    , def "b" (const 777 4)
                    ]
            pats = [ Stmt $ Def (Var "a") (Bind "x" Wild)
                   , Stmt (Def (Var "b") (Bind "y" Wild))
                     `Where`
                     [ cmpE (Bound "x") (Bound "y") (ConstSize 4) ]
                   ]
            expected = NoMatch
        matchStmtsIO [] pats stmts `shouldReturn` expected

      it "should try the next statement if assertion fails" $ do
        let stmts = [ def "a" (const 100 4)
                    , def "b" (const 777 4)
                    , def "c" (const 0 4)
                    ]
            pats = [ Stmt $ Def (Bind "x" Wild) Immediate
                   , Stmt (Def (Bind "y" Wild) Immediate)
                     `Where`
                     [ cmpUgt (Bound "x") (Bound "y") (ConstSize 4) ]
                   ]
            stmts' = stmts <> [constraint (cmpUgt (var "a" 4) (var "c" 4) 4)]
            expected = Match stmts'
        matchStmtsIO [] pats stmts `shouldReturn` expected

    context "solving" $ do
      let matchStmtsIO = matchStmts' (solveStmtsWithZ3 Solver.AbortOnError)

      it "should always run solver at end of match" $ do
        let stmts = [ def "a" (const 0 4)
                    , def "b" (const 777 4)
                    , constraint $ cmpE (var "a" 4) (var "b" 4) 4
                    ]
            pats = [ Stmt $ Def (Var "a") Wild
                   , Stmt $ Def (Var "b") Wild
                   ]
            expected = NoMatch
        matchStmtsIO [] pats stmts `shouldReturn` expected


    context "taint propagators" $ do
      let f = Function Nothing "myfunc" 0x888 []
          tps =
            [ FunctionCallPropagator "myfunc" (Parameter 0) ReturnParameter
            ]
          cdest = Pil.CallFunc f
          v = pilVar_ 4 Nothing
          -- TODO we should use Construct.var here once it's fixed
          vexp sym = var' (v sym) 4
          stmts =
            [ def' (v "b") (vexp "a"),
              def' (v "c") (add (load (vexp "b") 4) (const 0 4) 4),
              store (vexp "d") (vexp "c"),
              defCall' (v "r") cdest [vexp "d", vexp "e"] 8,
              def' (v "x") (vexp "r")
            ]

      it "should propagate taint through pure expressions" $ do
        let pats =
              [ Stmt $
                  Store
                    (Bind "out" Wild)
                    ( Bind "in" Wild
                        `TaintedBy` (mkExpr (ConstSize 4) . Pil.VAR . Pil.VarOp $ Pil.PilVar 4 Nothing "a")
                    )
              ]
            (ms, mr) = pureMatchStmts tps pats stmts
        mr `shouldBe` Match stmts
        HashMap.lookup "in" (ms ^. #boundSyms) `shouldBe` Just (vexp "c")
        HashMap.lookup "out" (ms ^. #boundSyms) `shouldBe` Just (vexp "d")

      it "should propagate taint through custom taint propagators" $ do
        let pats =
              [ Stmt $
                  Def
                    (Bind "out" Wild)
                    ( Bind "in" Wild
                      `TaintedBy` var' (pilVar 4 "d") (ConstSize 4)
                    )
              ]
            (ms, mr) = pureMatchStmts tps pats stmts
        mr `shouldBe` Match stmts
        HashMap.lookup "in" (ms ^. #boundSyms) `shouldBe` Just (vexp "r")
        HashMap.lookup "out" (ms ^. #boundSyms) `shouldBe` Just (vexp "x")
