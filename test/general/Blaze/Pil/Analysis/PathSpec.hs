{- HLINT ignore "Evaluate" -}

module Blaze.Pil.Analysis.PathSpec where

import Blaze.Prelude hiding (const, sym)

import Blaze.Pil.Analysis.Path (expandVars)
import Blaze.Pil.Construct
import Blaze.Types.Pil (Stmt)
import qualified Blaze.Types.Pil as Pil

import Test.Hspec


constStmts :: [Stmt]
constStmts =
  [ def "a" (const 42 4)
  , def "b" (var "a" 4)
  , def "c" (add (var "b" 4) (const 1 4) 4)
  ]

spec :: Spec
spec = describe "Blaze.Pil.Analysis" $ do
  describe "expandVars" $ do
    it "should do nothing for an empty list of statements" $ do
      let stmts = []
          result = expandVars stmts
          expected = []
      result `shouldBe` expected

    it "should eliminate a single assignemnt statement" $ do
      let stmts = [ def "x" (const 44 8) ]
          result = expandVars stmts
          expected = []
      result `shouldBe` expected

    it "should substitute an expression for var everywhere it appears" $ do
      let stmts = [ def "x" (add (var "y" 8) (const 1 8) 8)
                  , def "z" (var "x" 8)
                  , constraint $ cmpE (var "x" 8) (var "b" 8) 8
                  ]
          result = expandVars stmts
          expected =
            [ constraint $ cmpE (add (var "y" 8) (const 1 8) 8) (var "b" 8) 8
            ]
      result `shouldBe` expected

    it "should substitute two parallel vars" $ do
      let stmts =
            [ def "x" (add (var "y" 8) (const 1 8) 8)
            , def "z" (const 88 8)
            , constraint $ cmpE (var "x" 8) (var "z" 8) 8
            ]
          result = expandVars stmts
          expected =
            [ constraint $ cmpE (add (var "y" 8) (const 1 8) 8) (const 88 8) 8
            ]
      result `shouldBe` expected

    it "should trickle down substitutions" $ do
      let stmts =
            [ def "x" (add (var "y" 8) (const 1 8) 8)
            , def "z" (sub (var "x" 8) (const 1 88) 8)
            , constraint $ cmpE (const 777 8) (var "z" 8) 8
            ]
          result = expandVars stmts
          expected =
            [ constraint $ cmpE (const 777 8)
              (sub (add (var "y" 8) (const 1 8) 8) (const 1 88) 8)
              8
            ]
      result `shouldBe` expected

    it "should not subst vars equal to loads" $ do
      let stmts =
            [ def "y" (load (var "x" 8) 8)
            , constraint $ cmpE (const 777 8) (var "y" 8) 8
            ]
          result = expandVars stmts
          expected = stmts
      result `shouldBe` expected

    it "should not subst vars equal to calls" $ do
      let stmts =
            [ defCall "y" Pil.CallUnk [var "x" 8] 8
            , constraint $ cmpE (const 777 8) (var "y" 8) 8
            ]
          result = expandVars stmts
          expected = stmts
      result `shouldBe` expected
