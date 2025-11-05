{- HLINT ignore "Evaluate" -}

module Flint.Analysis.Path.MatcherSpec
  ( module Flint.Analysis.Path.MatcherSpec
  ) where

import Flint.Prelude hiding (and, const, not, or, until, sym, Location)

import Helper.Primitives

import Flint.Analysis.Path.Matcher
-- import Flint.Types.Analysis (TaintPropagator(..), Parameter (Parameter, ReturnParameter))
import Flint.Types.Analysis.Path.Matcher.Func
import Flint.Types.Analysis.Path.Matcher.PathPrep (mkPathPrep, PathPrep(PathPrep))
import Flint.Analysis.Path.Matcher.Logic.Combinators (good, bad)
import Flint.Analysis.Path.Matcher.Primitives (getInitialWMIs)
import Flint.Types.Symbol (Symbol)

import Blaze.Pil.Construct
import qualified Blaze.Pil.Construct as C
import Blaze.Pil.Solver (solveStmtsWithZ3)
import qualified Blaze.Pil.Solver as Solver
import Blaze.Types.Pil.Summary (CodeSummary(CodeSummary))
import Blaze.Pretty (PrettyShow'(PrettyShow'))
import Blaze.Types.Function (Function(Function))
import qualified Blaze.Types.Function as Func
import qualified Blaze.Types.Pil as Pil

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

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
func0 = Function Nothing "func0" (intToAddr 0x888) []

func1 :: Function
func1 = Function Nothing "func1" (intToAddr 0x999) []

func2 :: Function
func2 = Function Nothing "CGC_free" (intToAddr 0xAAA) []

-- matchStmtsIO :: [TaintPropagator] -> [M.StmtPattern] -> [Pil.Stmt] -> IO MatcherResult
-- matchStmtsIO tps pats = match' (solveStmtsWithZ3 Solver.AbortOnError) pats . mkPathPrep tps
type MatcherInt a = MatcherT () Int Identity a

spec :: Spec
spec = describe "Flint.Analysis.Path.Matcher" $ do
  let dummyCodeSummary = CodeSummary HashSet.empty HashSet.empty HashSet.empty [] HashSet.empty
      mkDummyPathPrep stmts = PathPrep stmts stmts HashSet.empty dummyCodeSummary
  context "parsing" $ do
    let defaultMatcherState = emptyMatcherState :: MatcherState () Int
        defaultMatcherCtx = MatcherCtx dummySolver
        maxResults = 20 -- should always be less for these tests
        observeAll :: MatcherState () Int -> MatcherInt a -> [(a, MatcherState () Int)]
        observeAll st = runIdentity . observeManyMatcherT defaultMatcherCtx st maxResults
        observeAll' :: [Int] -> MatcherInt a -> [(a, [Int])]
        observeAll' stmts = fmap (over _2 (view #remaining)) . observeAll st
          where
            st = defaultMatcherState & #remaining .~ stmts
  
        isEven :: Int -> Bool
        isEven = even

        getEven :: Int -> Maybe Int
        getEven n = if even n then Just n else Nothing

    context "parseUntil" $ do
      it "should return no results if there is no match" $ do
        let stmts :: [Int]
            stmts = [1]
            pat = bool bad good . (== 0)
            expected = []
        observeAll' stmts (parseUntil pat) `shouldBe` expected 

      it "should return single result when single thing in list matches" $ do
        let stmts :: [Int]
            stmts = [1]
            pat = bool bad good . (== 1)
            expected = [((), [])]
        observeAll' stmts (parseUntil pat) `shouldBe` expected 

      it "should return single matching result and remaining list" $ do
        let stmts :: [Int]
            stmts = [1, 2, 3, 4]
            pat n = bool bad (return n) $ n == 2
            expected = [(2, [3, 4])]
        observeAll' stmts (parseUntil pat) `shouldBe` expected 

      it "should return multiple matching results with appropriate remaining lists" $ do
        let stmts :: [Int]
            stmts = [1, 2, 3, 4, 5]
            pat n = bool bad (return n) $ isEven n
            expected = [ (2, [3, 4, 5])
                       , (4, [5])
                       ]
        sort (observeAll' stmts $ parseUntil pat) `shouldBe` sort expected 

    context "parseNext" $ do
      it "should return no results if there is no match" $ do
        let stmts :: [Int]
            stmts = [1]
            pat = bool bad good . (== 0)
            expected = []
        observeAll' stmts (parseNext pat) `shouldBe` expected 

      it "should return single result when single thing in list matches" $ do
        let stmts :: [Int]
            stmts = [1]
            pat = bool bad good . (== 1)
            expected = [((), [])]
        observeAll' stmts (parseNext pat) `shouldBe` expected 

      it "should not return non-next result, even if it matches" $ do
        let stmts :: [Int]
            stmts = [1, 2, 3, 4]
            pat n = bool bad (return n) $ n == 2
            expected = []
        observeAll' stmts (parseNext pat) `shouldBe` expected 

      it "should not return multiple matching results" $ do
        let stmts :: [Int]
            stmts = [2, 3, 4, 5]
            pat n = bool bad (return n) $ isEven n
            expected = [ (2, [3, 4, 5])
                       ]
        sort (observeAll' stmts $ parseNext pat) `shouldBe` sort expected 

    context "avoidUntil" $ do
      it "should return no results if `until` is not matched" $ do
        let stmts :: [Int]
            stmts = [1, 2, 3, 4, 5]
            avoid _ = bad
            until :: MatcherInt ((), [Int])
            until = bad
            expected = []
        sort (observeAll' stmts $ avoidUntil avoid until) `shouldBe` sort expected 

      it "should return no results if `until` is matched but `avoid` also matches" $ do
        let stmts :: [Int]
            stmts = [1, 2, 3, 4, 5]
            avoid _ = good
            until :: MatcherInt Text
            until = parseUntil (bool bad (return "Got it") . (== 3))
            expected = []
        sort (observeAll' stmts $ avoidUntil avoid until) `shouldBe` sort expected 

      it "should return no results if `until` is matched but `avoid` matches once" $ do
        let stmts :: [Int]
            stmts = [1, 2, 3, 4, 5]
            avoid _ = void $ parseUntil (bool bad good . (== 2))
            until :: MatcherInt Text
            until = do
              parseUntil (bool bad (return "Got it") . (== 3))
            expected = []
        sort (observeAll' stmts $ avoidUntil avoid until) `shouldBe` sort expected

      it "should return result if `until` matches and `avoid` doesn't match" $ do
        let stmts :: [Int]
            stmts = [1, 2, 3, 4, 5]
            avoid _ = bad
            until :: MatcherInt Text
            until = parseUntil (bool bad (return "Got it") . (== 3))
            expected = [("Got it", [4, 5])]
        sort (observeAll' stmts $ avoidUntil avoid until) `shouldBe` sort expected 

      it "should return result if `until` matches and `avoid` doesn't match until after `until`" $ do
        let stmts :: [Int]
            stmts = [1, 2, 3, 4, 5]
            avoid _ = void $ parseUntil (bool bad good . (== 4))
            until :: MatcherInt Text
            until = do
              parseUntil (bool bad (return "Got it") . (== 3))
            expected = [("Got it", [4, 5])]
        sort (observeAll' stmts $ avoidUntil avoid until) `shouldBe` sort expected 

      it "should return results for two matching `until`s if there are no avoids" $ do
        let stmts :: [Int]
            stmts = [1, 3, 4, 5, 6, 7, 9]
            avoid _ = void $ parseUntil (bool bad good . (== 14))
            until :: MatcherInt Int
            until = parseUntil (maybe bad return . getEven)
            expected = [ (4, [5, 6, 7, 9])
                       , (6, [7, 9])
                       ]
        sort (observeAll' stmts $ avoidUntil avoid until) `shouldBe` sort expected 

      it "should prune off one matching `until` because of an `avoid` while letting other succeed because the `avoid` comes after" $ do
        let stmts :: [Int]
            stmts = [1, 3, 4, 5, 6, 7, 9]
            avoid _ = void $ parseUntil (bool bad good . (== 5))
            until :: MatcherInt Int
            until = parseUntil (maybe bad return . getEven)
            expected = [ (4, [5, 6, 7, 9])
                       ]
        sort (observeAll' stmts $ avoidUntil avoid until) `shouldBe` sort expected

  context "match" $ do
    let pureMatch :: [StmtPattern] -> [Pil.Stmt] -> [(MatcherState Pil.Expression Pil.Stmt, [Pil.Stmt])]
        pureMatch pats stmts = runIdentity . match 20 dummySolver pats $ mkDummyPathPrep (stmts :: [Pil.Stmt])
        pureMatch_ pats stmts = view _2 <$> pureMatch pats stmts
        pureMatchWithBinds
          :: [StmtPattern]
          -> [Pil.Stmt]
          -> [ ( HashMap ( Symbol Pil.Expression) Pil.Expression
               , [Pil.Stmt]
               )
             ]
        pureMatchWithBinds pats stmts = f <$> pureMatch pats stmts
          where f (ms, stmts') = (ms ^. #boundSyms, stmts')
        _pureMatchStmt
          :: Statement ExprPattern
          -> Pil.Stmt
          -> [MatcherState Pil.Expression Pil.Stmt]
        _pureMatchStmt pat stmt = runIdentity
          . fmap (fmap snd)
          . observeManyMatcherT mctx mstate 20
          $ matchStmt pat stmt
          where
            mctx :: MatcherCtx Pil.Stmt Identity
            mstate :: MatcherState Pil.Expression Pil.Stmt
            (mctx, mstate) = mkMatcherState dummySolver $ mkDummyPathPrep ([] :: [Pil.Stmt])

        -- puts stmts in state and matches pattern
        pureMatchPattern
          :: [Pil.Stmt]
          -> StmtPattern
          -> [MatcherState Pil.Expression Pil.Stmt]
        pureMatchPattern stmts pat = runIdentity
          . fmap (fmap snd)
          . observeManyMatcherT mctx mstate 20
          $ matchPattern pat
          where
            mctx :: MatcherCtx Pil.Stmt Identity
            mstate :: MatcherState Pil.Expression Pil.Stmt
            (mctx, mstate) = mkMatcherState dummySolver $ mkDummyPathPrep stmts

        -- Match that runs the solver in IO
        solveMatch :: [StmtPattern] -> [Pil.Stmt] -> IO [(MatcherState Pil.Expression Pil.Stmt, [Pil.Stmt])]
        solveMatch pats stmts = match 20 (solveStmtsWithZ3 Solver.AbortOnError) pats $ mkDummyPathPrep (stmts :: [Pil.Stmt])
        solveMatch_ pats stmts = view _2 <<$>> solveMatch pats stmts

    it "should match empty list of stmts when provided no patterns" $ do
      pureMatch_ [] [] `shouldBe` [[]]

    it "should match when provided no patterns" $ do
      let stmts = path1
          pats = []
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should fail to match stmt pattern when there are no statements" $ do
      let stmts = []
          pats = [Stmt $ Def Wild Wild]
          expected = []
      pureMatch_ pats stmts `shouldBe` expected

    it "should match on a def statement" $ do
      -- let stmt = def "b" (load (var "arg4" 4) 4)
      --     pat = Def Wild Wild
      -- length (pureMatchStmt pat stmt) `shouldBe` 1

      let stmts = [def "b" (load (var "arg4" 4) 4)]
          pat = Stmt $ Def Wild Wild
          expected = [([], stmts)]
      ((\ms -> (ms ^. #remaining, ms ^. #parsedStmtsWithAssertions)) <$> pureMatchPattern stmts pat) `shouldBe` expected

      -- let stmts = [def "b" (load (var "arg4" 4) 4)]
      --     pats = [Stmt $ Def Wild Wild]
      --     expected = [stmts]
      -- pureMatch_ pats stmts `shouldBe` expected

    it "should match on an immediate" $ do
      let stmts = [def "b" (const 33 4)]
          pats = [Stmt $ Def Wild Immediate]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match on an immediate that must be evaluated" $ do
      let stmts = [def "b" (add (const 0 4) (const 33 4) 4)]
          pats = [Stmt $ Def Wild Immediate]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match on a const ptr immediate" $ do
      let loadPtrExpr = Pil.Expression
                { size = 8
                , op = Pil.CONST_PTR $ Pil.ConstPtrOp 1052800
                }
          stmts = [def "b" loadPtrExpr]
          pats = [Stmt $ Def Wild Immediate]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match on a const ptr immediate contained in a load" $ do
      let loadPtrExpr = Pil.Expression
                { size = 8
                , op = Pil.LOAD
                       . Pil.LoadOp
                       $ Pil.Expression
                         { size = 8
                         , op = Pil.CONST_PTR $ Pil.ConstPtrOp 1052800
                         }
                }
          stmts = [def "b" loadPtrExpr]
          pats = [Stmt $ Def Wild (Contains Immediate)]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match .== for integral CmpE" $ do
      let stmts :: [Pil.Stmt]
          stmts = [branchCond $ cmpE (const 33 4) (const 33 4) 4]
          pats = [Stmt . BranchCond $ Wild .== Wild]
          expected = [stmts]
      PrettyShow' (pureMatch_ pats stmts) `shouldBe` PrettyShow' expected

    it "should match .== with bind on either side" $ do
      let stmts :: [Pil.Stmt]
          stmts = [ branchCond $ cmpE (const 33 4) (var "arg4" 4) 4
                  , ret (var "arg4" 4)
                  ]
          pats1 = [ Stmt . BranchCond $ Bind "x" (Var "arg4") .== Wild
                  , Stmt . Ret $ Bind "x" Wild
                  ]
          expected = [ (HashMap.fromList
                        [ ("x", var "arg4" 4)
                        ]
                       , stmts
                       )
                     ]
          pats2 = [ Stmt . BranchCond $ Wild .== Bind "x" (Var "arg4")
                  , Stmt . Ret $ Bind "x" Wild
                  ]
      PrettyShow' (pureMatchWithBinds pats1 stmts) `shouldBe` PrettyShow' expected
      PrettyShow' (pureMatchWithBinds pats2 stmts) `shouldBe` PrettyShow' expected

    it "should match .== for float FcmpE" $ do
      let stmts = [branchCond $ fcmpE (fconst 33.0 4) (fconst 33.0 4) 4]
          pats = [Stmt . BranchCond $ Wild .== Wild]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match .== for integral (Not (Not (CmpE ...)))" $ do
      let stmts = [branchCond $ cmpE (const 33 4) (const 33 4) 4]
          pats = [Stmt . BranchCond $ Wild .== Wild]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match .== for integral (Not (CmpNe ...))" $ do
      let stmts = [branchCond $ cmpE (const 33 4) (const 33 4) 4]
          pats = [Stmt . BranchCond $ Wild .== Wild]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match .== for float (Not (Not (fcmpE ...)))" $ do
      let stmts = [branchCond $ fcmpE (fconst 33.0 4) (fconst 33.0 4) 4]
          pats = [Stmt . BranchCond $ Wild .== Wild]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match .== for float (Not (fcmpNe ...))" $ do
      let stmts = [branchCond $ fcmpE (fconst 33.0 4) (fconst 33.0 4) 4]
          pats = [Stmt . BranchCond $ Wild .== Wild]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match .== if args are flipped" $ do
      let stmts = [branchCond $ cmpE (const 33 4) (var "x" 4) 4]
          pats = [Stmt . BranchCond $ Var "x" .== Immediate]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match inequality if args are flipped" $ do
      let stmts = [branchCond $ cmpSlt (const 33 4) (var "x" 4) 4]
          pats = [Stmt . BranchCond $ Var "x" .> Immediate]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match on a var" $ do
      let stmts = [def "b" (load (var "arg4" 4) 4)]
          pats = [Stmt $ Def (Var "b") Wild]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match on a ConstFuncPtr with Var" $ do
      let funcPtr = Pil.Expression 4
            . Pil.ConstFuncPtr
            . Pil.ConstFuncPtrOp (intToAddr 0x888)
            $ Just "funcTable"
          stmts = [def "b" funcPtr]
          pats = [Stmt $ Def (Var "b") (Var "funcTable")]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should fail to match match a var if prefix of name is different" $ do
      let stmts = [def "b" (load (var "arg4" 4) 4)]
          pats = [Stmt $ Def (Var "a") Wild]
          expected = []
      pureMatch_ pats stmts `shouldBe` expected

    it "should match an expression that Contains a variable" $ do
      let stmts = [def "b" (load (var "arg4" 4) 4)]
          pats = [Stmt $ Def Wild (Contains (Var "arg4"))]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match an expression that Contains an Immediate" $ do
      let stmts = [def "b" (load (const 83483834 8) 8)]
          pats = [Stmt $ Def Wild (Contains Immediate)]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match an expression that Contains an Immediate2" $ do
      let stmts = [def "b" (load (constPtr 83483834 8) 8)]
          pats = [Stmt $ Def Wild (Contains Immediate)]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match first match in OrPattern" $ do
      let stmts = [def "b" (load (var "arg4" 4) 4)]
          pats = [Stmt $ Def (Var "b" .|| Var "a") Wild]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match second match in OrPattern" $ do
      let stmts = [def "b" (load (var "arg4" 4) 4)]
          pats = [Stmt $ Def (Var "a" .|| Var "b") Wild]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match a NotPattern" $ do
      let stmts = [def "b" (load (var "arg4" 4) 4)]
          pats = [Stmt $ Def (NotPattern $ Var "c") Wild]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should use NotPattern to fail to bind if two things are equal" $ do
      let stmts = [store (var "a" 8) (load (var "a" 8) 8)]
          pats = [Stmt $ Store (Bind "dest" Wild) (load (Bind "src" (NotPattern $ Bind "dest" Wild)) ())]
          expected = []
      pureMatch_ pats stmts `shouldBe` expected

    -- it "should use NotPattern to bind if two things are not equal" $ do
    --   let stmts = [store (var "a" 8) (load (var "b" 8) 8)]
    --       pats = [Stmt $ Store (Bind "dest" Wild) (load (Bind "src" (NotPattern $ Bind "dest" Wild)) ())]
    --       expected = ( HashMap.fromList
    --                    [ ("dest", var "a" 8)
    --                    , ("src", var "b" 8)
    --                    ]
                         
    --                  , [stmts]
    --                  )
    --   first (view #boundSyms) (pureMatch pats stmts) `shouldBe` expected

    -- it "should use NotPattern to avoid binding if two things are equal" $ do
    --   let stmts = [store (var "a" 8) (load (var "a" 8) 8)]
    --       pats = [Stmt $ Store (Bind "dest" Wild) (load (Bind "src" (NotPattern $ Bind "dest" Wild)) ())]
    --       expected = [( HashMap.fromList []                         
    --                   , []
    --                   )
    --                  ]
    --   first (view #boundSyms) (pureMatch pats stmts) `shouldBe` expected


    it "should match a more complex expression that Contains a variable" $ do
      let stmts = [def "b" (load (add (var "arg4" 4) (const 44 4) 4) 4)]
          pats = [Stmt $ Def Wild (Contains (Var "arg4"))]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match on two statements in a row" $ do
      let stmts :: [Pil.Stmt]
          stmts = [ def "b" (load (var "arg4" 4) 4)
                  , def "c" (load (var "arg4" 4) 4)
                  ]
          pats = [ Stmt $ Def (Var "b") Wild
                 , Stmt $ Def (Var "c") Wild
                 ]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should fail if first statement is not a match" $ do
      let stmts = [ def "no" (var "match" 4)
                  , def "b" (load (var "arg4" 4) 4)
                  , def "c" (load (var "arg4" 4) 4)
                  ]
          pats = [ Stmt $ Def (Var "b") Wild
                 , Stmt $ Def (Var "c") Wild
                 ]
          expected = []
      pureMatch_ pats stmts `shouldBe` expected

    it "should match a single statement to Star" $ do
      let stmts = [ def "no" (var "match" 4)
                  , def "b" (load (var "arg4" 4) 4)
                  , def "c" (load (var "arg4" 4) 4)
                  ]
          pats = [ Star
                 , Stmt $ Def (Var "b") Wild
                 , Stmt $ Def (Var "c") Wild
                 ]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match multiple statements to Star" $ do
      let stmts = [ def "no" (var "match" 4)
                  , def "nice" (var "try" 4)
                  , def "b" (load (var "arg4" 4) 4)
                  , def "ur" (var "sad" 4)
                  , def "c" (load (var "arg4" 4) 4)
                  ]
          pats = [ Star
                 , Stmt $ Def (Var "b") Wild
                 , Star
                 , Stmt $ Def (Var "c") Wild
                 ]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected


    it "should match an expression has been bound to sym" $ do
      let stmts = [ def "b" (load (var "arg4" 4) 4)
                  , def "c" (load (var "arg4" 4) 4)
                  ]
          pats = [ Stmt $ Def (Var "b") (Bind "x" Wild)
                 , Stmt $ Def (Var "c") (Bind "x" Wild)
                 ]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should fail to match an expression that has been bound to a different sym" $ do
      let stmts = [ def "b" (load (var "arg4" 4) 4)
                  , def "c" (load (var "arg5" 4) 4)
                  ]
          pats = [ Stmt $ Def (Var "b") (Bind "x" Wild)
                 , Stmt $ Def (Var "c") (Bind "x" Wild)
                 ]
          expected = []
      pureMatch_ pats stmts `shouldBe` expected

    it "should return multiple matches if multiple statements match" $ do
      let stmts = [ def "b" (var "arg4" 4)
                  , def "c" (var "b" 4)
                  , def "skip" (const 123 4)
                  , def "d" (var "b" 4)
                  ]
          pats = [ Star
                 , Stmt $ Def (Bind "x" Wild) (Var "b")
                 ]
          expected = [ ( HashMap.fromList
                         [("x", var "c" 4)]
                       , stmts
                       )
                     , ( HashMap.fromList
                         [("x", var "d" 4)]
                       , stmts
                       )
                     ]
      sort (pureMatchWithBinds pats stmts) `shouldBe` sort expected

    it "should handle nested Ordered that returns multiple results" $ do
      let stmts :: [Pil.Stmt]
          stmts = [ def "b" (var "arg4" 4)
                  , def "c" (var "b" 4)
                  , def "skip" (const 123 4)
                  , def "d" (var "b" 4)
                  ]
          pats = [ ordered [ordered [Star, Stmt $ Def (Bind "x" Wild) (Var "b")]]
                 ]
          expected = [ stmts, stmts ]
      PrettyShow' (pureMatch_ pats stmts) `shouldBe` PrettyShow' expected

    it "should skip over statement with expression that has been bound to different sym, but then match a later statement" $ do
      let stmts = [ def "b" (load (var "arg4" 4) 4)
                  , def "z" (const 0 4)
                  , def "c" (load (var "arg4" 4) 4)
                  ]
          pats = [ Stmt $ Def (Var "b") (Bind "x" Wild)
                 , Star
                 , Stmt $ Def Wild (Bind "x" Wild)
                 ]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match on a call to a named function" $ do
      let cdest = Pil.CallFunc func0
          stmts = [ defCall "r" cdest [var "a" 4, load (var "arg4" 4) 4] 8
                  ]
          pats = [ Stmt $ Call (Just Wild) (CallFunc (FuncName "func0")) [Wild, Wild]
                 ]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match on a call to a named function from a set of names" $ do
      let cdest = Pil.CallFunc func0
          stmts = [ defCall "r" cdest [var "a" 4, load (var "arg4" 4) 4] 8
                  ]
          funcNames = HashSet.fromList ["func0", "func1"]
          pats = [ Stmt $ Call (Just Wild) (CallFunc (FuncNames funcNames)) [Wild, Wild]
                 ]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match on a call to a named function using regex" $ do
      let cdest = Pil.CallFunc func2
          stmts = [ defCall "r" cdest [var "a" 4, load (var "arg4" 4) 4] 8
                  ]
          pats = [ Stmt $ Call (Just Wild) (CallFunc (FuncNameRegex "^[a-zA-Z0-9_]+free$")) [Wild, Wild]
                 ]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should not match on a call to a named function if args do not parse" $ do
      let cdest = Pil.CallFunc func0
          stmts = [ defCall "r" cdest [var "a" 4, load (var "arg4" 4) 4] 8
                  ]
          pats = [ Stmt $ Call (Just Wild) (CallFunc (FuncName "func0"))
                   [Var "nope", Wild]
                 ]
          expected = []
      pureMatch_ pats stmts `shouldBe` expected

    it "should not match on a call that has fewer args than expected" $ do
      let cdest = Pil.CallFunc func0
          stmts = [ defCall "r" cdest [var "a" 4] 8
                  ]
          pats = [ Stmt $ Call (Just Wild) (CallFunc (FuncName "func0")) [Wild, Wild]
                 ]
          expected = []
      pureMatch_ pats stmts `shouldBe` expected

    it "should match on a call to a named function with a return variable even if the pattern for the return variable is Nothing" $ do
      let cdest = Pil.CallFunc func0
          stmts = [ defCall "r" cdest [var "a" 4, load (var "arg4" 4) 4] 8
                  ]
          pats = [ Stmt $ Call Nothing (CallFunc (FuncName "func0")) [Wild, Wild]
                 ]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match on an indirect call" $ do
      let cdest = Pil.CallExpr $ var "x" 4
          stmts = [ defCall "r" cdest [var "a" 4, load (var "arg4" 4) 4] 8
                  ]
          pats = [ Stmt $ Call Nothing (CallIndirect $ Var "x") [Wild, Wild]
                 ]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match on an indirect call using Contains" $ do
      let cdest = Pil.CallExpr $ load (add (var "x" 4) (const 1 4) 4) 4
          stmts = [ defCall "r" cdest [var "a" 4, load (var "arg4" 4) 4] 8
                  ]
          pats = [ Stmt $ Call Nothing (CallIndirect . Contains $ Var "x") [Wild, Wild]
                 ]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match on an expr in an indirect call to a const func ptr" $ do
      let funcPtr = Pil.Expression 4
            . Pil.ConstFuncPtr
            . Pil.ConstFuncPtrOp (intToAddr 0x888)
            $ Just "funcTable"
          cdest = Pil.CallExpr $ load (add funcPtr (const 0x4e 4) 4) 4
          stmts = [ defCall "r" cdest [var "a" 4, load (var "arg4" 4) 4] 8
                  ]
          pats = [ Stmt (Call Nothing (CallIndirect . Contains $ Var "funcTable") [Wild, Wild])
                 ]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match on an expanded call to a named function when pattern expects no return" $ do
      let ctx0 = Pil.Ctx func0 0
          stmts :: [Pil.Stmt]
          stmts = [ enterContext ctx0 [var "a" 4, load (var "arg4" 4) 4]
                  ]
          pats = [ Stmt $ Call Nothing (CallFunc (FuncName "func0")) [Wild, Wild]
                 ]
          expected = [stmts]
      PrettyShow' (pureMatch_ pats stmts) `shouldBe` PrettyShow' expected

    it "should match on an expanded call to a named function and on its return" $ do
      let ctx0 = Pil.Ctx func0 0
          ctx1 = Pil.Ctx func1 1
          stmts :: [Pil.Stmt]
          stmts = [ enterContext ctx0 [var "a" 4, load (var "arg4" 4) 4]
                  , ret $ var "r" 4
                  , exitContext ctx0 ctx1
                  ]
          pats = [ Stmt $ Call (Just $ Var "r") (CallFunc (FuncName "func0")) [Wild, Wild]
                 ]
          expected = [stmts]
      PrettyShow' (pureMatch_ pats stmts) `shouldBe` PrettyShow' expected

    it "should fail to match if ret does not match" $ do
      let ctx0 = Pil.Ctx func0 0
          ctx1 = Pil.Ctx func1 1
          stmts = [ enterContext ctx0 [var "a" 4, load (var "arg4" 4) 4]
                  , ret $ const 888 4
                  , exitContext ctx0 ctx1
                  ]
          pats = [ Stmt $ Call (Just $ Var "r") (CallFunc (FuncName "func0")) [Wild, Wild]
                 ]
          expected = []
      pureMatch_ pats stmts `shouldBe` expected

    it "should match on an expanded call to a named function, not match on any statement inside expanded function body, but match on statement after" $ do
      let ctx0 = Pil.Ctx func0 0
          ctx1 = Pil.Ctx func1 1
          stmts :: [Pil.Stmt]
          stmts = [ enterContext ctx1 [var "a" 4, load (var "arg4" 4) 4]
                  , def "x" $ const 42 8
                  , ret $ var "x" 8
                  , exitContext ctx1 ctx0
                  , def "y" $ const 777 8
                  , def "z" $ const 777 8
                  ]
          pats1 = [ Stmt $ Call (Just Wild) (CallFunc (FuncName "func1")) [Wild, Wild]
                  , Stmt $ Def (Var "x") Wild
                  ]
          pats2 = [ Stmt $ Call (Just Wild) (CallFunc (FuncName "func1")) [Wild, Wild]
                  , Stmt $ Def (Var "y") Wild
                  ]

          expected1 = []
          expected2 = [stmts]

      PrettyShow' (pureMatch_ pats1 stmts) `shouldBe` PrettyShow' expected1
      PrettyShow' (pureMatch_ pats2 stmts) `shouldBe` PrettyShow' expected2

    context "EnterContext and ExitContext statements" $ do
      it "should match EnterContext for AnyCtx with empty arg patterns" $ do
        let ctx0 = Pil.Ctx func0 0
            stmts = [ enterContext ctx0 [var "a" 4, load (var "arg4" 4) 4]
                    ]
            pats = [ Stmt $ EnterContext AnyCtx [] ]
            expected = [stmts]
        pureMatch_ pats stmts `shouldBe` expected


      it "should match EnterContext for AnyCtx with proper arg patterns" $ do
        let ctx0 = Pil.Ctx func0 0
            stmts = [ enterContext ctx0 [var "a" 4, load (var "arg4" 4) 4]
                    ]
            pats = [ Stmt $ EnterContext AnyCtx [Var "a", Contains (Var "arg4")] ]
            expected = [stmts]
        pureMatch_ pats stmts `shouldBe` expected

      it "should not match EnterContext if arg pattern not matched" $ do
        let ctx0 = Pil.Ctx func0 0
            stmts = [ enterContext ctx0 [var "a" 4, load (var "arg4" 4) 4]
                    ]
            pats = [ Stmt $ EnterContext AnyCtx [Var "b", Wild] ]
            expected = []
        pureMatch_ pats stmts `shouldBe` expected

      it "should match ExitContext for AnyCtx, AnyCtx" $ do
        let ctx0 = Pil.Ctx func0 0
            ctx1 = Pil.Ctx func1 1
            stmts = [ exitContext ctx1 ctx0
                    , def "c" $ const 42 8
                    ]
            pats = [ Stmt $ ExitContext AnyCtx AnyCtx
                   , Stmt $ Def (Var "c") Wild
                   ]
            expected = [stmts]
        pureMatch_ pats stmts `shouldBe` expected

      it "should match matching bound Ctxs" $ do
        let ctx0 = Pil.Ctx func0 0
            ctx1 = Pil.Ctx func1 1
            stmts = [ enterContext ctx1 [var "a" 4, load (var "arg4" 4) 4]
                    , exitContext ctx1 ctx0
                    ]
            pats = [ Stmt $ EnterContext (BindCtx "x" AnyCtx) []
                   , Stmt $ ExitContext (BindCtx "x" AnyCtx) AnyCtx
                   ]
            
            expected = [stmts]
        pureMatch_ pats stmts `shouldBe` expected

      it "should fail to match non-matching bound Ctxs" $ do
        let ctx0 = Pil.Ctx func0 0
            ctx1 = Pil.Ctx func1 1
            stmts = [ enterContext ctx1 [var "a" 4, load (var "arg4" 4) 4]
                    , exitContext ctx1 ctx0
                    ]
            pats = [ Stmt $ EnterContext (BindCtx "x" AnyCtx) []
                   , Stmt $ ExitContext AnyCtx (BindCtx "x" AnyCtx)
                   ]
            
            expected = []
        pureMatch_ pats stmts `shouldBe` expected

    it "should match on orr" $ do
      let stmts = [ def "b" (const 0 4)
                  ]
          pats = [ orr [ Stmt $ Def (Var "a") Wild
                       , Stmt $ Def (Var "b") Wild
                       ]
                 ]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match ordered statements" $ do
      let stmts = [ def "b" (const 0 4)
                  , def "skip" (const 1 4)
                  , def "c" (const 1 4)
                  , def "d" (const 1 4)
                  ]
          pats = [ ordered [ Stmt $ Def (Var "b") Wild
                           , Star
                           , Stmt $ Def (Var "c") Wild
                           ]
                 , Stmt $ Def (Var "d") Wild
                 ]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should match ADD pattern on FIELD_ADDR expression" $ do
      let stmts = [def "a" (C.fieldAddr (var "esp" 8) 0x24 8)]
          pats = [Stmt $ Def (Var "a") (Expr $ Pil.ADD (Pil.AddOp (Var "esp") Immediate))]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should backtrack on Ordered statements until it finds a match" $ do
      let stmts :: [Pil.Stmt]
          stmts = [ def "a" (const 1 8)
                  , def "b" (const 2 8)
                  , def "c" (const 3 8)
                  , def "d" (const 2 8)
                  ]
          pats = [ ordered [ Star
                           , Stmt $ Def (Bind "dest1" Wild) (Bind "x" Wild)
                           , Star
                           , Stmt $ Def (Bind "dest2" Wild) (Bind "x" Wild)
                           ]
                 ]
          expected = stmts
          r = pureMatch pats stmts
      length r `shouldBe` 1
      let (ms, mr) = fromJust $ headMay r
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
          pats = [ Star
                 , Stmt $ Def (Bind "dest1" Wild) (Bind "x" Wild)
                 , Star
                 , Stmt $ Def (Bind "dest2" Wild) (Bind "x" Wild)
                 ]
          expected = stmts
          r = pureMatch pats stmts
      length r `shouldBe` 1
      let (ms, mr) = fromJust $ headMay r
      mr `shouldBe` expected
      HashMap.lookup "x" (ms ^. #boundSyms) `shouldBe` Just (const 2 8)
      HashMap.lookup "dest1" (ms ^. #boundSyms) `shouldBe` Just (var "b" 8)
      HashMap.lookup "dest2" (ms ^. #boundSyms) `shouldBe` Just (var "d" 8)


    context "Neighbors" $ do
      it "should match two sequential simple patterns with no stmts in between" $ do
        let stmts = [ def "b" (const 0 4)
                    , def "c" (const 1 4)
                    , def "d" (const 1 4)
                    ]
            pats = [ Stmt $ Def (Var "b") Wild
                   , Stmt $ Def (Var "c") Wild
                   , Star
                   , Stmt $ Def (Var "d") Wild
                   ]
            expected = [stmts]
        pureMatch_ pats stmts `shouldBe` expected

      it "should match two sequential complex patterns that consume multiple statements" $ do
        let stmts = [ def "skip" (const 33 4)
                    , def "a" (const 0 4)
                    , def "b" (const 0 4)
                    , def "c" (const 1 4)
                    , def "d" (const 1 4)
                    , def "e" (const 2 4)
                    ]
            pats = [ Star
                   , ordered [ Stmt $ Def (Var "a") Wild
                             , Stmt $ Def (Var "b") Wild
                             ]
                   , ordered [ Stmt $ Def (Var "c") Wild
                             , Stmt $ Def (Var "d") Wild
                             ]
                   , Stmt $ Def (Var "e") Wild
                   ]
            expected = [stmts]
        pureMatch_ pats stmts `shouldBe` expected

      it "should not match two statements separated by a non-match" $ do
        let stmts = [ def "b" (const 0 4)
                    , def "skip" (const 0 4)
                    , def "c" (const 1 4)
                    , def "d" (const 1 4)
                    ]
            pats = [ Stmt $ Def (Var "b") Wild
                   , Stmt $ Def (Var "c") Wild
                   , Star  
                   , Stmt $ Def (Var "d") Wild
                   ]
            expected = []
        pureMatch_ pats stmts `shouldBe` expected

    context "EndOfPath" $ do
      it "should match on end of path" $ do
        let stmts = [ def "b" (const 0 4)
                    , def "c" (const 1 4)
                    ]
            pats = [ Star
                   , EndOfPath
                   ]
            expected = [stmts]
        pureMatch_ pats stmts `shouldBe` expected

      it "should match on end of path in 'until' of AvoidUntil" $ do
        let stmts = [ def "b" (const 0 4)
                    , def "c" (const 1 4)
                    ]
            pats = [ AvoidUntil $ AvoidSpec
                     { until = Star `And` EndOfPath
                     , avoid = Stmt $ Def (Var "z") Wild
                     }
                   ]
            expected = [stmts]
        pureMatch_ pats stmts `shouldBe` expected

      it "should match on end of path in an Ordered" $ do
        let stmts = [ def "b" (const 0 4)
                    , def "c" (const 1 4)
                    ]
            pats = [ ordered [ Star
                             , Stmt $ Def (Var "b") Wild
                             , Star
                             , EndOfPath
                             ]
                   ]
            expected = [stmts]
        pureMatch_ pats stmts `shouldBe` expected

    context "Locations" $ do
      let loc addr s = s & #addr .~ addr
      it "should store location for single statement" $ do
        let stmts = [loc (intToAddr 0x888) $ def "b" (load (var "arg4" 4) 4)]
            pats = [Location "varPlace" . Stmt $ Def (Var "b") Wild]
            expected = Just $ HashMap.fromList
              [("varPlace", Right (intToAddr 0x888))]
        (fmap (view (_1 . #locations)) . headMay $ pureMatch pats stmts) `shouldBe` expected

      it "should ignore unmatched statements preceeding matched location" $ do
        let stmts = [ loc (intToAddr 0x777) $ def "a" (load (var "arg1" 4) 4)
                    , loc (intToAddr 0x888) $ def "b" (load (var "arg4" 4) 4)
                    ]
            pats = [ Star
                   , Location "varPlace" . Stmt $ Def (Var "b") Wild
                   ]
            expected = Just $ HashMap.fromList
              [("varPlace", Right (intToAddr 0x888))]
        (fmap (view (_1 . #locations)) . headMay $ pureMatch pats stmts) `shouldBe` expected

      it "should get location of AnyOne statement" $ do
        let stmts = [ loc (intToAddr 0x777) $ def "a" (load (var "arg1" 4) 4)
                    , loc (intToAddr 0x888) $ def "b" (load (var "arg4" 4) 4)
                    , loc (intToAddr 0x999) $ def "c" (load (var "arg5" 4) 4)
                    ]
            pats = [ Star
                   , Location "varPlace" $ orr
                     [ Stmt $ Def (Var "zzz") Wild
                     , Stmt $ Def (Var "b") Wild
                     ]
                   ]
            expected = Just $ HashMap.fromList
              [("varPlace", Right (intToAddr 0x888))]
        (fmap (view (_1 . #locations)) . headMay $ pureMatch pats stmts) `shouldBe` expected

    it "should avoid until" $ do
      let stmts = [ def "a" (const 0 4)
                  , def "wiff" (const 1 4)
                  , def "b" (const 0 4)
                  ]
          pats = [ AvoidUntil $ AvoidSpec
                   { avoid = Stmt $ Def (Var "b") Wild
                   , until = ordered
                             [ Star
                             , Stmt $ Def (Var "wiff") Wild
                             ]
                   }
                 ]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should fail if Until not reached" $ do
      let stmts = [ def "a" (const 0 4)
                  , def "wiff" (const 1 4)
                  , def "b" (const 0 4)
                  ]
          pats = [ AvoidUntil $ AvoidSpec
                   { avoid = Stmt $ Def (Var "b") Wild
                   , until = ordered
                             [ Star
                             , Stmt $ Def (Var "c") Wild
                             ]
                   }
                 ]
          expected = []
      pureMatch_ pats stmts `shouldBe` expected

    it "should fail if avoid is reached before until" $ do
      -- The problem here currently is that if the avoid matches the first stmt
      -- it moves on and tries from the second, forgetting that the first failed big time
      let stmts = [ def "a" (const 0 4)
                  , def "wiff" (const 1 4)
                  , def "b" (const 0 4)
                  ]
          pats = [ AvoidUntil $ AvoidSpec
                   { avoid = Stmt $ Def (Var "a") Wild
                   , until = ordered
                     [ Star
                     , Stmt $ Def (Var "b") Wild
                     ]
                   }
                 ]
          expected = []
      pureMatch_ pats stmts `shouldBe` expected


    it "should find the 'until' first, then backtrack and check the 'avoid' after" $ do
      -- If AvoidUntil is implemented without backtracking, it will bind "ptr" to the var named "x"
      -- Then it will fail because (Bind "ptr" Wild) in the Until part doesn't bind to "x"
      let stmts = [ constraint $ cmpSlt (load (var "x" 8) 8) (const 888 8) 8
                  , store (var "a" 8) $ add (load (var "a" 8) 8) (const 1 8) 8
                  ]
          pats = [ AvoidUntil $ AvoidSpec
                   { avoid = Stmt . Constraint $ load (Bind "ptr" Wild) () .< Wild
                   , until = ordered
                             [ Star
                             , Stmt $ Store (Bind "ptr" Wild)
                               $ add (load (Bind "ptr" Wild) ()) Wild ()
                             ]
                   }
                 ]
          expected = [stmts]
      pureMatch_ pats stmts `shouldBe` expected

    it "should find the 'until' first, then backtrack and check the 'avoid' after version 2" $ do
      let stmts = [ constraint $ cmpSlt (load (var "a" 8) 8) (const 888 8) 8
                  , store (var "a" 8) $ add (load (var "a" 8) 8) (const 1 8) 8
                  ]
          pats = [ AvoidUntil $ AvoidSpec
                   { avoid = Stmt . Constraint $ load (Bind "ptr" Wild) () .< Wild
                   , until = ordered
                             [ Star
                             , Stmt $ Store (Bind "ptr" Wild)
                               $ add (load (Bind "ptr" Wild) ()) Wild ()
                             ]
                   }
                 ]
          expected = []
      pureMatch_ pats stmts `shouldBe` expected


    it "should handle avoid/until for real world example" $ do
      
      let addr :: Pil.Expression
          addr = add (var "arg1" 0x8) (const 0x8 0x8) 0x8
          stmts :: [Pil.Stmt]
          stmts =
            [ constraint $ not (cmpUgt (add (load addr 0x8) (const 0x1 0x8) 0x8) (var "x" 0x8) 0x8) 0x8
            , store addr $ add (load addr 0x8) (const 0x1 0x8) 0x8
            ]
          pats = [ AvoidUntil $ AvoidSpec
                   { avoid = Stmt . Constraint
                     $   (Contains (load (Bind "ptr" Wild) ()) .< Wild)
                     .|| (Contains (load (Bind "ptr" Wild) ()) .<= Wild)
                     .|| (Contains (load (Bind "ptr" Wild) ()) .> Wild)
                     .|| (Contains (load (Bind "ptr" Wild) ()) .>= Wild)
                     .|| (Contains (load (Bind "ptr" Wild) ()) .== Wild)
                     .|| (Contains (load (Bind "ptr" Wild) ()) ./= Wild)
                   , until = ordered
                     [ Star
                     , Stmt $ Store (Bind "ptr" Wild) (add (load (Bind "ptr" Wild) ()) (Bind "n" Wild) ())
                     ]
                   }
                 ]
          expected = []
      pureMatch_ pats stmts `shouldBe` expected

    context "assertions" $ do

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
            expected = [stmts']
        pureMatch_ pats stmts `shouldBe` expected

--       it "should fail if assertion fails" $ do
--         let stmts = [ def "a" (const 0 4)
--                     , def "b" (const 777 4)
--                     ]
--             pats = [ Stmt $ Def (Var "a") (Bind "x" Wild)
--                    , Stmt (Def (Var "b") (Bind "y" Wild))
--                      `Where`
--                      [ cmpE (Bound "x") (Bound "y") (ConstSize 4) ]
--                    ]
--             expected = NoMatch
--         matchStmtsIO [] pats stmts `shouldReturn` expected

      it "should create multiple possible paths with assertions" $ do
        let stmts = [ def "a" (const 100 4)
                    , def "b" (const 777 4)
                    , def "c" (const 0 4)
                    ]
            pats = [ Stmt $ Def (Bind "x" Wild) Immediate
                   , Star
                   , Stmt (Def (Bind "y" Wild) Immediate)
                     `Where`
                     [ cmpUgt (Bound "x") (Bound "y") (ConstSize 4) ]
                   ]
            expected = [ stmts <> [constraint (cmpUgt (var "a" 4) (var "c" 4) 4)]
                       , [ def "a" (const 100 4)
                         , def "b" (const 777 4)
                         , constraint (cmpUgt (var "a" 4) (var "b" 4) 4)
                         , def "c" (const 0 4)
                         ]
                       ]
        PrettyShow' (sort (pureMatch_ pats stmts)) `shouldBe` PrettyShow' (sort expected)

    context "necessarily constraint" $ do

      it "should require vars to be satisfiable if necessary constraints are met" $ do
        let stmts = [ def "a" (const 0 4)
                    , def "b" (const 777 4)
                    ]
            pats = [ ordered [ Stmt $ Def (Var "a") (Bind "x" Wild)
                             , Stmt (Def (Var "b") (Bind "y" Wild))
                             ]
                     `Necessarily`
                     [ cmpUlt (Bound "x") (Bound "y") (ConstSize 4)
                     ]
                   ]
            expected = [ stmts <> [constraint (cmpUlt (const 0 4) (const 777 4) 4)] ]
        solveMatch_ pats stmts `shouldReturn` expected

--       -- TODO: Understand how binding works. Results are surprising for this test.
-- --       it "should require vars to be satisfiable if multiple necessary constraints are met (where test)" $ do
-- --         let stmts = [ def "a" (const 0 4)
-- --                     , def "b" (const 777 4)
-- -- --                    , constraint $ cmpUge (var "b" 4) (const 10 4) 4
-- --                     , def "c" (var "b" 4)
-- --                     ]
-- --             pats = [ Stmt $ Def (Var "a") (Bind "x" Wild)
-- --                    , Stmt $ Def (Var "b") (Bind "y" Wild)
-- --                    , Stmt (Def (Var "c") (Bind "z" Wild))
-- --                      `Where`
-- --                      [
-- --                        (not (or
-- --                              (cmpUge (Bound "x") (Bound "y") (ConstSize 4))
-- --                              (cmpNE (Bound "z") (Bound "y") (ConstSize 4))
-- --                              (ConstSize 4))
-- --                          (ConstSize 4))
-- --                      ]
-- --                    ]
-- --             expected = NoMatch
-- --         matchStmtsIO [] pats stmts `shouldReturn` expected

--       -- TODO: Unclear why NoMatch is returned when using multiple necessary constraints.
-- --       it "should require vars to be satisfiable if multiple necessary constraints are met" $ do
-- --         let stmts = [ def "a" (const 0 4)
-- --                     , def "b" (const 777 4)
-- -- --                    , constraint $ cmpUge (var "b" 4) (const 10 4) 4
-- --                     , def "c" (var "b" 4)
-- --                     ]
-- --             pats = [ Stmt $ Def (Var "a") (Bind "x" Wild)
-- --                    , Stmt $ Def (Var "c") (Bind "z" Wild)
-- --                    , Stmt (Def (Var "b") (Bind "y" Wild))
-- --                      `Necessarily`
-- --                      [ cmpUlt (Bound "x") (Bound "y") (ConstSize 4)
-- --                      , cmpE (Bound "z") (Bound "y") (ConstSize 4)
-- --                      ]
-- --                    ]
-- --             stmts' = stmts <> [constraint (cmpUlt (const 0 4) (const 777 4) 4)]
-- --             expected = Match stmts'
-- --         matchStmtsIO [] pats stmts `shouldReturn` expected

--       -- it "should require vars to be unsatisfiable if necessary constrints are not met" $ do
--       --   let stmts = [ def "a" (const 10 4)
--       --               , def "b" (var "c" 4)
--       --               ]
--       --       pats = [ Stmt $ Def (Var "a") (Bind "x" Wild)
--       --              , Stmt (Def (Var "b") (Bind "y" Wild))
--       --                `Necessarily`
--       --                [ cmpUlt (Bound "x") (Bound "y") (ConstSize 4) ]
--       --              ]
--       --   matchStmtsIO [] pats stmts `shouldReturn` NoMatch

      -- TODO: Unclear why NoMatch is returned when using multiple necessary constraints.
      it "should require vars to be satisfiable if multiple necessary constraints are met" $ do
        let stmts = [ def "a" (const 0 4)
                    , def "b" (const 777 4)
                    , def "c" (var "b" 4)
                    ]
            pats = [ ordered [ Stmt $ Def (Var "a") (Bind "x" Wild)
                             , Stmt $ Def (Var "b") (Bind "y" Wild)
                             , Stmt (Def (Var "c") (Bind "z" Wild))
                             ]
                     `Necessarily`
                     [ cmpUlt (Bound "x") (Bound "y") (ConstSize 4)
                     , cmpE (Bound "z") (Bound "y") (ConstSize 4)
                     ]
                   ]
            expected =
              [ stmts <> [ constraint (cmpUlt (const 0 4) (const 777 4) 4)
                         , constraint (cmpE (var "b" 4) (const 777 4) 4)
                         ]
              ]
        solveMatch_ pats stmts `shouldReturn` expected

      it "should require vars to be unsatisfiable if necessary constrints are not met" $ do
        let stmts = [ def "a" (const 10 4)
                    , def "b" (var "c" 4)
                    ]
            pats = [ ordered [ Stmt $ Def (Var "a") (Bind "x" Wild)
                             , Stmt (Def (Var "b") (Bind "y" Wild))
                             ]
                     `Necessarily`
                     [ cmpUlt (Bound "x") (Bound "y") (ConstSize 4) ]
                   ]
        solveMatch_ pats stmts `shouldReturn` []

    context "solving" $ do

      it "should always run solver at end of match" $ do
        let stmts = [ def "a" (const 0 4)
                    , def "b" (const 777 4)
                    , constraint $ cmpE (var "a" 4) (var "b" 4) 4
                    ]
            pats = [ Stmt $ Def (Var "a") Wild
                   , Stmt $ Def (Var "b") Wild
                   ]
            expected = []
        solveMatch pats stmts `shouldReturn` expected


{-
    context "taint propagators" $ do
      let f = Function Nothing "myfunc" (intToAddr 0x888) []
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
-}

--       it "should propagate taint through pure expressions" $ do
--         let pats =
--               [ Stmt $
--                   Store
--                     (Bind "out" Wild)
--                     ( Bind "in" Wild
--                         `TaintedBy` (mkExpr (ConstSize 4) . Pil.VAR . Pil.VarOp $ Pil.PilVar 4 Nothing "a" False Pil.UnknownLocation)
--                     )
--               ]
--             (ms, mr) = pureMatchStmts tps pats stmts
--         mr `shouldBe` Match stmts
--         HashMap.lookup "in" (ms ^. #boundSyms) `shouldBe` Just (vexp "c")
--         HashMap.lookup "out" (ms ^. #boundSyms) `shouldBe` Just (vexp "d")

--       it "should propagate taint through custom taint propagators" $ do
--         let pats =
--               [ Stmt $
--                   Def
--                     (Bind "out" Wild)
--                     ( Bind "in" Wild
--                       `TaintedBy` var' (pilVar 4 "d") (ConstSize 4)
--                     )
--               ]
--             (ms, mr) = pureMatchStmts tps pats stmts
--         mr `shouldBe` Match stmts
--         HashMap.lookup "in" (ms ^. #boundSyms) `shouldBe` Just (vexp "r")
--         HashMap.lookup "out" (ms ^. #boundSyms) `shouldBe` Just (vexp "x")

    context "Primitives" $ do
      let pureMatchFull
            :: MatcherCtx Pil.Stmt Identity
            -> MatcherState Pil.Expression Pil.Stmt
            -> [StmtPattern]
            -> [( MatcherState Pil.Expression Pil.Stmt
                , [Pil.Stmt]
                )]
          pureMatchFull mctx mstate = runIdentity . match_ 20 mctx mstate
          pureMatchFullWithBinds mctx mstate = fmap f . pureMatchFull mctx mstate
            where f (ms, stmts') = (ms ^. #boundSyms, stmts')

      it "should match callable primitive" $ do
        let callablePrim = fooCallableWMI3
            -- outerFunc = bar
            outerPath = barPath3
            prim = copyPrim
            varPats = HashMap.fromList
              [ ("dest", Bind "newdest" Wild)
              , ("src", Bind "newsrc" Wild)
              ]
            pats = [ Star
                   , Primitive prim varPats
                   ]
            pprep = mkPathPrep [] outerPath
            solver :: StmtSolver stmt Identity
            solver _ = return $ Solver.Sat HashMap.empty
            callablePrims = HashMap.fromList
              [( (copyPrim, callablePrim ^. #func), HashSet.singleton callablePrim )]
            (ctx, initMs) = mkMatcherState solver pprep
            initMs' = initMs & #callablePrimitives .~ callablePrims

            expectedBoundSyms = HashMap.fromList
              [ ("newdest", var "global1" 8)
              , ("newsrc"
                , load (add (var_ bar "arg4" 8) (const 4 8) 8) 8
                )
              ]
            expectedParsedSmts :: [Pil.Stmt]
            expectedParsedSmts =
              [ constraint $ cmpSgt (var_ bar "arg1" 8) (const 0 8) 8
              , defCall "r" (Pil.CallFunc foo)
                [ var_ bar "arg4" 8
                , load (var_ bar "arg2" 8) 8
                ]
                8
              -- Should add in the constraint from prim
              , constraint $ cmpNE (load (var_ bar "arg2" 8) 8) (const 0 8) 8
              , ret $ var_ bar "r" 8
              ]
            expected = [(expectedBoundSyms, expectedParsedSmts)]
            
        pureMatchFullWithBinds ctx initMs' pats `shouldBe` expected
      
      it "should match format string Primitive pattern" $ do        
        let stdLibPrims = memcpyPrims <> sscanfPrims <> strdupPrims <> printfPrims
            allFuncs = [memcpy, sscanf, printf, foo, bar]
            initialCPrims = getInitialWMIs stdLibPrims . fmap Func.Internal $ allFuncs
            outerPath = fooPath1
            pprep = mkPathPrep [] outerPath
            solver :: StmtSolver stmt Identity
            solver _ = return $ Solver.Sat HashMap.empty
            (ctx, initMs) = mkMatcherState solver pprep
            initMs' = initMs & #callablePrimitives .~ initialCPrims
            varPats = HashMap.empty
            pat = [ Primitive controlledFormatStringPrim varPats ]

        null (pureMatchFullWithBinds ctx initMs' pat) `shouldBe` False

      it "should create CallableWMI from StdLibPrimites, then use them to match Primitive pattern" $ do
        let stdLibPrims = memcpyPrims <> sscanfPrims <> printfPrims
            allFuncs = [memcpy, sscanf, printf, foo, bar]
            initialCPrims = getInitialWMIs stdLibPrims . fmap Func.Internal $ allFuncs
            outerPath = fooPath1
            pprep = mkPathPrep [] outerPath
            solver :: StmtSolver stmt Identity
            solver _ = return $ Solver.Sat HashMap.empty
            (ctx, initMs) = mkMatcherState solver pprep
            initMs' = initMs & #callablePrimitives .~ initialCPrims
            varPats = HashMap.empty
            pat = [ Primitive controlledFormatStringPrim varPats ]

        null (pureMatchFullWithBinds ctx initMs' pat) `shouldBe` False
