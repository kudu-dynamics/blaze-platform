{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Evaluate" -}

module Blaze.Cfg.SolverSpec where

import Blaze.Prelude hiding (Type, sym, bitSize, Constraint, const, Symbol)

import Blaze.Cfg hiding
  ( BasicBlockNode (ctx)
  , CallNode (ctx)
  , func
  )
import qualified Blaze.Cfg as Cfg
import Blaze.Function (Function (Function))
import qualified Blaze.Function as Func
import qualified Blaze.Pil.Construct as C
import Blaze.Types.Pil (Ctx (Ctx))
import qualified Blaze.Types.Pil as Pil
import Blaze.Util.Spec (mkUuid1)
import qualified Data.HashMap.Strict as HashMap
import Blaze.Cfg.Checker
import Blaze.Types.Pil.Checker hiding (signed, len, ret)
import Blaze.Pil.Construct
import Test.Hspec


bbp :: Ctx -> Text -> [Pil.Stmt] -> CfNode [Pil.Stmt]
bbp ctx name stmts = BasicBlock $ BasicBlockNode
  { ctx = ctx
  , start = 0
  , end = 0
  , uuid = uuid'
  , nodeData = stmts
  }
  where
    uuid' = mkUuid1 . hash $ name

callerFunc :: Function
callerFunc = Function
  { symbol = Nothing
  , name = "caller"
  , address = 0
  , params = []
  }

targetFunc :: Function
targetFunc = Function
  { symbol = Nothing
  , name = "targetFunc"
  , address = 100
  , params = [ Func.FuncParamInfo $ Func.ParamInfo "arg1" Func.In
             , Func.FuncParamInfo $ Func.ParamInfo "arg2" Func.In
             ]
  }

callerCtx :: Ctx
callerCtx = Ctx callerFunc 0

targetCtx :: Ctx
targetCtx = Ctx targetFunc 1

spec :: Spec
spec = describe "Blaze.Cfg.Solver" $ do
  context "solveCfg" $ do
    let pv = C.pilVar
        mkVarSymTypeMap = HashMap.fromList
        checkVars = fmap (view $ _3 . #varSymTypeMap) . checkCfg
        signed = Just True
        bw = Just
    it "should check a single basic block" $ do
      let rootNode = bbp callerCtx "root"
                     [ def "x" $ sx (const 888 4) 4
                     , def "a" $ sub (var "x" 4) (const 999 4) 4]
          cfg = mkCfg 0 rootNode [] []
          rvars = [ (pv 4 "x", DSType (TInt (bw 32) signed))
                  , (pv 4 "a", DSType (TInt (bw 32) signed))]

      checkVars cfg `shouldBe` Right (mkVarSymTypeMap rvars)

    it "should check multiple basic blocks" $ do
      let rootNode = bbp callerCtx "root"
                     [ def "a" $ sub (const 888 4) (const 999 4) 4]
          middleNode = bbp callerCtx "middle"
                     [ def "b" $ sub (const 888 4) (const 999 4) 4]
          returnNode = bbp callerCtx "return"
                       [ def "c" $ add (var "a" 4) (var "b" 4) 4
                       , ret $ var "c" 4
                       ]
          cfg = mkCfg 0 rootNode
                [ middleNode, returnNode ]
                [ CfEdge rootNode middleNode Cfg.UnconditionalBranch
                , CfEdge middleNode returnNode Cfg.UnconditionalBranch
                ]
          rvars = [ (pv 4 "a", DSType (TInt (bw 32) Nothing))
                  , (pv 4 "b", DSType (TInt (bw 32) Nothing))
                  , (pv 4 "c", DSType (TInt (bw 32) Nothing))
                  ]

      PShow (checkVars cfg) `shouldBe` PShow (Right (mkVarSymTypeMap rvars))
