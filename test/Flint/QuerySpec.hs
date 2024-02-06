{- HLINT ignore "Evaluate" -}

module Flint.QuerySpec where

import Flint.Prelude hiding (sym, const, until)

import Flint.Analysis.Path.Matcher
import Flint.Query

import Blaze.Types.Function (Function(Function))
import qualified Blaze.Types.Graph as G

import Test.Hspec


func0 :: Function
func0 = Function Nothing "func0" 0x888 []

spec :: Spec
spec = describe "Flint.Query" $ do
  let fromTupleEdges = G.fromEdges . fmap (\(a, b) -> G.LEdge () (G.Edge a b))
  context "getCallSequenceGraph" $ do
    it "should return empty graph for empty pattern list" $ do
      let pats = []
          expected = G.empty
      getCallSequenceGraph pats `shouldBe` expected

    it "should return empty graph for pattern list that has no calls"  $ do
      let pats = [Stmt $ Def Wild Wild]
          expected = G.empty
      getCallSequenceGraph pats `shouldBe` expected

    it "should return singleton graph for pattern list with one call"  $ do
      let pats = [ Stmt $ Def Wild Wild
                 , Stmt $ Call (Just Wild) (CallFunc (FuncName "func0")) [Wild, Wild]
                 ]
          expected = G.fromNode $ FuncNode (FuncName "func0") 0
      getCallSequenceGraph pats `shouldBe` expected

    it "should return single path graph for pattern list with two sequential calls"  $ do
      let pats = [ Stmt $ Def Wild Wild
                 , Stmt $ Call (Just Wild) (CallFunc (FuncName "func0")) [Wild, Wild]
                 , Stmt $ Call (Just Wild) (CallFunc (FuncName "func1")) [Wild, Wild]
                 ]
          f0 = FuncNode (FuncName "func0") 0
          f1 = FuncNode (FuncName "func1") 1
          expected = fromTupleEdges [ (f0, f1) ]
      getCallSequenceGraph pats `shouldBe` expected

    it "should return graph with two disparate nodes when parsing pattern with two parallel calls"  $ do
      let pats = [ Stmt $ Def Wild Wild
                 , AnyOne
                   [ Stmt $ Call (Just Wild) (CallFunc (FuncName "func0")) [Wild, Wild]
                   , Stmt $ Call (Just Wild) (CallFunc (FuncName "func1")) [Wild, Wild]
                   ]
                 ]
          f0 = FuncNode (FuncName "func0") 0
          f1 = FuncNode (FuncName "func1") 1
          expected = G.addNodes [f1] $ G.fromNode f0
      getCallSequenceGraph pats `shouldBe` expected

    it "should return diamond pattern graph for call->(AnyOne of 2 calls)->call"  $ do
      let pats = [ Stmt $ Def Wild Wild
                 , Stmt $ Call (Just Wild) (CallFunc (FuncName "func0")) [Wild, Wild]
                 , AnyOne
                   [ Stmt $ Call (Just Wild) (CallFunc (FuncName "func1")) [Wild, Wild]
                   , Stmt $ Call (Just Wild) (CallFunc (FuncName "func2")) [Wild, Wild]
                   ]
                 , Stmt $ Call (Just Wild) (CallFunc (FuncName "func3")) [Wild, Wild]
                 ]
          f0 = FuncNode (FuncName "func0") 0
          f1 = FuncNode (FuncName "func1") 1
          f2 = FuncNode (FuncName "func2") 2
          f3 = FuncNode (FuncName "func3") 3

          expected = fromTupleEdges
            [ (f0, f1)
            , (f0, f2)
            , (f1, f3)
            , (f2, f3)
            ]
      getCallSequenceGraph pats `shouldBe` expected

    it "should shortcut non-call pattern in AnyOne"  $ do
      let pats = [ Stmt $ Def Wild Wild
                 , Stmt $ Call (Just Wild) (CallFunc (FuncName "func0")) [Wild, Wild]
                 , AnyOne
                   [ Stmt $ Def Wild Wild
                   , Stmt $ Call (Just Wild) (CallFunc (FuncName "func1")) [Wild, Wild]
                   ]
                 , Stmt $ Call (Just Wild) (CallFunc (FuncName "func2")) [Wild, Wild]
                 ]
          f0 = FuncNode (FuncName "func0") 0
          f1 = FuncNode (FuncName "func1") 1
          f2 = FuncNode (FuncName "func2") 2

          expected = fromTupleEdges
            [ (f0, f2)
            , (f0, f1)
            , (f1, f2)
            ]
      getCallSequenceGraph pats `shouldBe` expected

    it "should get calls inside 'until' in AvoidUntil and ignore calls in 'avoid' (for now)"  $ do
      let pats = [ AvoidUntil $ AvoidSpec
                   { avoid = Stmt $ Call (Just Wild) (CallFunc (FuncName "func3")) [Wild, Wild]
                   , until = Ordered
                     [ Stmt $ Def Wild Wild
                     , Stmt $ Call (Just Wild) (CallFunc (FuncName "func0")) [Wild, Wild]
                     , Stmt $ Call (Just Wild) (CallFunc (FuncName "func1")) [Wild, Wild]
                     ]
                   }
                 ]
          f0 = FuncNode (FuncName "func0") 0
          f1 = FuncNode (FuncName "func1") 1
          expected = fromTupleEdges [ (f0, f1) ]
      getCallSequenceGraph pats `shouldBe` expected
