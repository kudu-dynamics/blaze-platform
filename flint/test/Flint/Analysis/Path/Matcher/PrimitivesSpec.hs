{- HLINT ignore "Evaluate" -}

module Flint.Analysis.Path.Matcher.PrimitivesSpec where

import Flint.Prelude hiding (const)

import Helper.Primitives

import Flint.Types.Analysis.Path.Matcher.Func
import qualified Flint.Analysis.Path.Matcher.Primitives as Prim
import Flint.Analysis.Path.Matcher.Primitives

import Blaze.Pil.Construct

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import Test.Hspec


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

    -- it "should properly convert global to FuncVar in Expression" $ do
    --   let params = fooParams
    --       codeSum = fooCodeSummary2
    --       globalVar = var "global1" 8
    --       expr = load (add globalVar (const 4 8) 8) 8
    --       expected =
    --         ( load (add (FuncVar $ Prim.Global globalVar) (const 4 8) 8) 8
    --         , HashSet.fromList [Prim.Global globalVar]
    --         )

    --   PShow (toFuncVarExpr params codeSum expr) `shouldBe` PShow expected

  context "mkCallablePrimitive" $ do
    it "should create callable primitive with single nested arg and no constraints" $ do
      let func = foo
          prim = copyPrim
          locationMap = HashMap.fromList
            [ ("write", HashSet.singleton 0x1234) ]

          path = fooPath3
          codeSum = fooCodeSummary3
          varMap = HashMap.fromList
            [ ("dest", var "global1" 8)
            , ("src", load (add (var "arg1" 8) (const 4 8) 8) 8)
            ]
          expected = Right fooCallablePrimitive3
          
      PShow (mkCallablePrimitive func codeSum prim varMap locationMap path) `shouldBe` PShow expected

    it "should create CallablePrimitives from StdLibPrimites" $ do
      let stdLibPrims = memcpyPrims <> sscanfPrims <> printfPrims
          allFuncs = [memcpy, sscanf, printf, foo, bar]
          initialCPrims = getInitialPrimitives stdLibPrims allFuncs
          memcpyCPrim
            = CallablePrimitive
              { prim = copyPrim
              , func = memcpy
              , callDest = FuncName "memcpy"
              , varMapping = HashMap.fromList
                [ ("dest", (FuncVar $ Arg 0, HashSet.fromList [Arg 0]))
                , ("src", (FuncVar $ Arg 1, HashSet.fromList [Arg 1]))
                ]
              , constraints = []
              , locations = HashMap.fromList
                [ ( "write"
                  , HashSet.singleton $ memcpy ^. #address
                  )
                ]
              , linkedVars = HashSet.fromList [ Arg 0, Arg 1 ]
              }
          sscanfCPrim
            = CallablePrimitive
              { prim = controlledFormatStringPrim
              , func = sscanf
              , callDest = FuncName "sscanf"
              , varMapping = HashMap.fromList
                [ ("fmt", (FuncVar $ Arg 1, HashSet.fromList [Arg 1])) ]
              , constraints = []
              , locations = HashMap.fromList
                [ ( "usage"
                  , HashSet.singleton $ sscanf ^. #address
                  )
                ]
              , linkedVars = HashSet.fromList [ Arg 1 ]
              }

          printfCPrim
            = CallablePrimitive
              { prim = controlledFormatStringPrim
              , func = printf
              , callDest = FuncName "printf"
              , varMapping = HashMap.fromList
                [ ("fmt", (FuncVar $ Arg 0, HashSet.fromList [Arg 0])) ]
              , constraints = []
              , locations = HashMap.fromList
                [ ( "usage"
                  , HashSet.singleton $ printf ^. #address
                  )
                ]
              , linkedVars = HashSet.fromList [ Arg 0 ]
              }

          expected = HashMap.fromList
            [ ( copyPrim
              , HashSet.fromList
                [ memcpyCPrim ] )
            , ( controlledFormatStringPrim
              , HashSet.fromList
                [ sscanfCPrim, printfCPrim ] )
            ]
      initialCPrims `shouldBe` expected
