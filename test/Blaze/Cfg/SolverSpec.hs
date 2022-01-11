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
import Blaze.Types.Pil (Ctx (Ctx), CtxId (CtxId), Symbol)
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

pilCall :: Symbol -> Function -> [Pil.Expression] -> Pil.Stmt
pilCall varSym func args =
  C.defCall varSym (Pil.CallFunc func) args 8

mkCallNode :: Ctx -> Text -> Symbol -> Function -> [Pil.Expression] -> (Cfg.CallNode [Pil.Stmt], Pil.Stmt)
mkCallNode ctx name retVarSym targetFunc' args =
  ( CallNode
    { ctx = ctx
    , start = 0
    , callDest = Pil.CallFunc targetFunc'
    , uuid = uuid'
    , nodeData = [callStmt']
    }
  , callStmt'
  )
  where
    callStmt' = pilCall retVarSym targetFunc' args
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
callerCtx = Ctx callerFunc . CtxId $ mkUuid1 (1 :: Int)

targetCtx :: Ctx
targetCtx = Ctx targetFunc . CtxId $ mkUuid1 (2 :: Int)

spec :: Spec
spec = describe "Blaze.Cfg.Solver" $ do
  context "solveCfg" $ do
    let pv = C.pilVar
        mkVarSymTypeMap = HashMap.fromList . fmap (over _1 pv)
        checkVars = fmap (view $ _3 . #varSymTypeMap) . checkCfg
        signed = DSType $ TVSign True
        bw = DSType . TVBitWidth
    it "should check a single basic block" $ do
      let rootNode = bbp callerCtx "root"
                     [ def "x" $ sx (const 888 4) 4
                     , def "a" $ sub (var "x" 4) (const 999 4) 4]
          cfg = mkCfg rootNode [] []
          rvars = [ ("x", DSType (TInt (bw 32) signed))
                  , ("a", DSType (TInt (bw 32) signed))]

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
          cfg = mkCfg rootNode
                [ middleNode, returnNode ]
                [ CfEdge rootNode middleNode Cfg.UnconditionalBranch
                , CfEdge middleNode returnNode Cfg.UnconditionalBranch
                ]
          rvars = [ ("a", DSType (TInt (bw 32) (DSVar (Sym 25))))
                  , ("b", DSType (TInt (bw 32) (DSVar (Sym 25))))
                  , ("c", DSType (TInt (bw 32) (DSVar (Sym 25))))
                  ]

      PShow (checkVars cfg) `shouldBe` PShow (Right (mkVarSymTypeMap rvars))
