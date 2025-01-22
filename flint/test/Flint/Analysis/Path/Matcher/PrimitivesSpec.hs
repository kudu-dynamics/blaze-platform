{- HLINT ignore "Evaluate" -}

module Flint.Analysis.Path.Matcher.PrimitivesSpec where

import Flint.Prelude hiding (const)

import qualified Flint.Analysis.Path.Matcher as M
import qualified Flint.Analysis.Path.Matcher.Primitives as Prim
import Flint.Analysis.Path.Matcher.Primitives

import Blaze.Pil.Construct
import Blaze.Pil.Summary (CodeSummary)
import qualified Blaze.Pil.Summary as Summary
import Blaze.Types.Function (Function(Function), FuncParamInfo)
import qualified Blaze.Types.Function as Func
import Blaze.Types.Pil (Stmt)
import qualified Blaze.Types.Pil as Pil

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import Test.Hspec

memcpy :: Function
memcpy = Function Nothing "memcpy" 0x111 []

sscanf :: Function
sscanf = Function Nothing "sscanf" 0x222 []


fooParams :: [FuncParamInfo]
fooParams = [ Func.FuncParamInfo $ Func.ParamInfo "arg1" Func.Unknown
            , Func.FuncParamInfo $ Func.ParamInfo "arg2" Func.Unknown
            ]
            
foo :: Function
foo = Function Nothing "foo" 0x999 fooParams

fooPath1 :: [Stmt]
fooPath1 =
  [ defCall "r" (Pil.CallFunc sscanf)
    [ constPtr 0x3234 8
    , (load (add (var "arg1" 8) (const 4 8) 8) 8)
    ]
    8
  , ret $ var "r" 8
  ]

fooPath2 :: [Stmt]
fooPath2 =
  [ defCall "r" (Pil.CallFunc sscanf)
    [ constPtr 0x3234 8
    , (load (add (var "global1" 8) (const 4 8) 8) 8)
    ]
    8
  , ret $ var "r" 8
  ]

fooPath3 :: [Stmt]
fooPath3 =
  [ constraint $ cmpNE (var "arg2" 8) (const 0 8) 8
  , defCall "r" (Pil.CallFunc memcpy)
    [ (var "global1" 8)
    , (load (add (var "arg1" 8) (const 4 8) 8) 8)
    ]
    8
  , ret $ var "r" 8
  ]

fooCodeSummary1 :: CodeSummary
fooCodeSummary1 = Summary.fromStmts fooPath1

fooCodeSummary2 :: CodeSummary
fooCodeSummary2 = Summary.fromStmts fooPath2

fooCodeSummary3 :: CodeSummary
fooCodeSummary3 = Summary.fromStmts fooPath3




spec :: Spec
spec = describe "Flint.Analysis.Path.Matcher.Primitives" $ do
  context "toFuncVarExpr" $ do
    it "should properly convert nested arg to FuncVar in Expression" $ do
      let params = fooParams
          codeSum = fooCodeSummary1
          expr = load (add (var "arg1" 8) (const 4 8) 8) 8
          expected =
            ( load (add (FuncVar $ Prim.Arg 0) (const 4 (ConstSize 8)) (ConstSize 8)) (ConstSize 8)
            , HashSet.fromList [Prim.Arg 0]
            )
          
      PShow (toFuncVarExpr params codeSum expr) `shouldBe` PShow expected

    it "should properly convert global to FuncVar in Expression" $ do
      let params = fooParams
          codeSum = fooCodeSummary2
          globalVar = var "global1" 8
          expr = load (add globalVar (const 4 8) 8) 8
          expected =
            ( load (add (FuncVar $ Prim.Global globalVar) (const 4 (ConstSize 8)) (ConstSize 8)) (ConstSize 8)
            , HashSet.fromList [Prim.Global globalVar]
            )

      PShow (toFuncVarExpr params codeSum expr) `shouldBe` PShow expected


  context "mkCallablePrimitive" $ do
    it "should create callable primitive with single nested arg and no constraints" $ do
      let func = foo
          prim = Prim.PrimType
            { name = "copy"
            , vars = HashSet.fromList ["dest", "src"]
            , locations = HashSet.fromList ["write"]
            }
          locationMap = HashMap.fromList
            [ ("write", HashSet.singleton 0x1234) ]

          path = fooPath3
          codeSum = fooCodeSummary3
          varMap = HashMap.fromList
            [ ("dest", var "global1" 8)
            , ("src", (load (add (var "arg1" 8) (const 4 8) 8) 8))
            ]
          -- expr = load (add (var "arg1" 8) (const 4 8) 8) 8
          expr' = ( load (add (FuncVar $ Prim.Arg 0) (const 4 (ConstSize 8)) (ConstSize 8)) (ConstSize 8)
                  , HashSet.fromList [Prim.Arg 0]
                  )
          globalFuncVar = Prim.Global $ var "global1" 8
          expected = Prim.CallablePrimitive
            { prim = prim
            , callDest = M.FuncName "foo"
            , varMapping = HashMap.fromList
              [ ( "src"
                , ( load (add (FuncVar $ Prim.Arg 0) (const 4 (ConstSize 8)) (ConstSize 8)) (ConstSize 8)
                  , HashSet.fromList [Prim.Arg 0]
                  )
                )
              , ("dest"
                , ( FuncVar globalFuncVar
                  , HashSet.fromList [globalFuncVar]
                  )
                )
              ]
            , constraints =
                [ ( cmpNE (FuncVar $ Prim.Arg 1) (const 0 (ConstSize 8)) (ConstSize 8)
                  , HashSet.fromList [Prim.Arg 1]
                  )
                ]
            , locations = locationMap
            , linkedVars = HashSet.fromList
              [ Prim.Arg 0
              , Prim.Arg 1
              , globalFuncVar
              ]
            }
          
      PShow (mkCallablePrimitive func codeSum prim varMap locationMap path) `shouldBe` PShow expected
