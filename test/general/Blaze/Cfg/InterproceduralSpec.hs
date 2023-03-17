{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

module Blaze.Cfg.InterproceduralSpec where

import Blaze.Cfg hiding
  ( BasicBlockNode (ctx)
  , CallNode (ctx)
  , func
  )
import qualified Blaze.Cfg as Cfg
import Blaze.Function (Function (Function))
import qualified Blaze.Function as Func
import Blaze.Pil.Construct (defaultSize)
import qualified Blaze.Pil.Construct as C
import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Pil (Ctx (Ctx), CtxId (CtxId), Symbol)
import qualified Blaze.Types.Pil as Pil
import Blaze.Util.Spec (mkUuid1)
import qualified Blaze.Cfg.Interprocedural as ICfg
import Blaze.Pretty (PrettyShow'(PrettyShow'))
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

callerCurrentCtxId :: CtxId
callerCurrentCtxId = CtxId 0

callerCtx :: Ctx
callerCtx = Ctx callerFunc callerCurrentCtxId

targetCurrentCtxId :: CtxId
targetCurrentCtxId = CtxId 1

targetCtx :: Ctx
targetCtx = Ctx targetFunc targetCurrentCtxId

callerCfg :: PilCfg
callNode :: Cfg.CallNode [Pil.Stmt]
callStmt :: Pil.CallStatement
targetCfg :: PilCfg
expandedCfg :: PilCfg
leaveFuncUUID :: UUID
(callerCfg, callNode, callStmt, targetCfg, expandedCfg, leaveFuncUUID) =
  (callerCfg', callNodeInner1, callStmt1, targetCfg', expandedCfg', leaveFuncUUID')
  where
    rootNode1 = bbp callerCtx "root1"
      [ C.def "a1" (C.const 52 8)
      , C.def "b1" (C.const 84 8)
      ]
    (callNodeInner1, rawCallStmt1) = mkCallNode callerCtx "call1" "funcRet1" targetFunc [C.var "a1" 8, C.var "b1" 8]
    termNode1 = bbp callerCtx "term1"
      [ C.def "r1" $ C.var "funcRet1" 8
      , C.ret $ C.var "r1" 8
      ]
    callNode1 = Cfg.Call callNodeInner1
    callStmt1 = fromJust . Pil.mkCallStatement $ rawCallStmt1

    callerCfg' = mkCfg
      (callerCurrentCtxId + 1)
      rootNode1
      [ callNode1
      , termNode1
      ]
      [ CfEdge rootNode1 callNode1 Cfg.UnconditionalBranch
      , CfEdge callNode1 termNode1 Cfg.UnconditionalBranch
      ]

    -------------------------

    arg1 = Pil.PilVar defaultSize (Just targetCtx) "arg1"
    arg2 = Pil.PilVar defaultSize (Just targetCtx) "arg2"

    rootNode2 = bbp targetCtx "root2"
      [ C.def "a2" (C.var' arg1 8)
      , C.def "b2" (C.var' arg2 8)
      ]
    termNode2 = bbp targetCtx "term2"
      [ C.def "r2" $ C.add (C.var "a2" 8) (C.var "b2" 8) 8
      , C.ret $ C.var "r2" 8
      ]

    targetCfg' = mkCfg
      (targetCurrentCtxId + 1)
      rootNode2
      [ termNode2 ]
      [ CfEdge rootNode2 termNode2 Cfg.UnconditionalBranch ]

    --------------------

    enterFuncNode3 = Cfg.EnterFunc
      $ Cfg.EnterFuncNode callerCtx targetCtx (callNodeInner1 ^. #uuid)
        [ C.def' arg1 (C.var "a1" 8)
        , C.def' arg2 (C.var "b1" 8)
        ]

    leaveFuncUUID' = mkUuid1 (88 :: Int)
    leaveFuncNode3 = Cfg.LeaveFunc
      $ Cfg.LeaveFuncNode targetCtx callerCtx leaveFuncUUID'
        [ Pil.Def $ Pil.DefOp retVar0 (C.var "r2" 8)
        , Pil.DefPhi $ Pil.DefPhiOp (C.pilVar "funcRet1") [retVar0]
        ]
      where
        retVar0 = Pil.PilVar defaultSize (Just targetCtx) "retVar_0"

    expandedCfg' = mkCfg
      (targetCurrentCtxId + 1)
      rootNode1
      [ enterFuncNode3
      , rootNode2
      , termNode2
      , leaveFuncNode3
      , termNode1
      ]
      [ CfEdge rootNode1 enterFuncNode3 Cfg.UnconditionalBranch
      , CfEdge enterFuncNode3 rootNode2 Cfg.UnconditionalBranch
      , CfEdge rootNode2 termNode2 Cfg.UnconditionalBranch
      , CfEdge termNode2 leaveFuncNode3 Cfg.UnconditionalBranch
      , CfEdge leaveFuncNode3 termNode1 Cfg.UnconditionalBranch
      ]

spec :: Spec
spec = describe "Blaze.Cfg.Interprocedural" $ do
  context "mkEnterFuncNode" $ do
    let uuid' = mkUuid1 (1 :: Int)
        rawCallStmt :: Pil.Stmt
        rawCallStmt = C.defCall' (C.pilVar' callerCtx "outerRet")
                      (Pil.CallFunc targetFunc)
                      [ C.var' (C.pilVar' callerCtx "x") 8
                      , C.var' (C.pilVar' callerCtx "y") 8
                      ]
                      8
        callStmt' :: Pil.CallStatement
        callStmt' = fromJust $ Pil.mkCallStatement rawCallStmt
        result = ICfg.mkEnterFuncNode uuid' callerCtx targetCtx callStmt'
        expected :: Cfg.EnterFuncNode [Pil.Stmt]
        expected = Cfg.EnterFuncNode
          { prevCtx = callerCtx
          , nextCtx = targetCtx
          , uuid = uuid'
          , nodeData =
            [ C.def' (C.pilVar' targetCtx "arg1") (C.var' (C.pilVar' callerCtx "x") 8)
            , C.def' (C.pilVar' targetCtx "arg2") (C.var' (C.pilVar' callerCtx "y") 8)
            ]
          }
    it "should make enter func node" $ do
      PrettyShow' (result ^. #nodeData) `shouldBe` PrettyShow' (expected ^. #nodeData)
      PrettyShow' result `shouldBe` PrettyShow' expected

  context "mkLeaveFuncNode" $ do
    let uuid' = mkUuid1 (1 :: Int)
        rawCallStmt :: Pil.Stmt
        rawCallStmt = C.defCall' (C.pilVar' callerCtx "outerRet")
                      (Pil.CallFunc targetFunc)
                      [ C.var' (C.pilVar' callerCtx "x") 8
                      , C.var' (C.pilVar' callerCtx "y") 8
                      ]
                      8
        callStmt' :: Pil.CallStatement
        callStmt' = fromJust $ Pil.mkCallStatement rawCallStmt
        retExprs = [ C.var' (C.pilVar' targetCtx "innerRet1") 8
                   , C.var' (C.pilVar' targetCtx "innerRet2") 8
                   ]
        result = ICfg.mkLeaveFuncNode uuid' callerCtx targetCtx callStmt' retExprs
        retVar0 = Pil.PilVar defaultSize (Just targetCtx) "retVar_0"
        retVar1 = Pil.PilVar defaultSize (Just targetCtx) "retVar_1"
        expected :: Cfg.LeaveFuncNode [Pil.Stmt]
        expected = Cfg.LeaveFuncNode
          { prevCtx = targetCtx
          , nextCtx = callerCtx
          , uuid = uuid'
          , nodeData =
            [ C.def' retVar0 (C.var' (C.pilVar' targetCtx "innerRet1") 8)
            , C.def' retVar1 (C.var' (C.pilVar' targetCtx "innerRet2") 8)
            , Pil.DefPhi $ Pil.DefPhiOp (C.pilVar' callerCtx "outerRet") [retVar0, retVar1]
            ]
          }
    it "should make enter func node" $ do
      PrettyShow' (result ^. #nodeData) `shouldBe` PrettyShow' (expected ^. #nodeData)
      PrettyShow' result `shouldBe` PrettyShow' expected

  context "expandCall" $ do
    let expanded = Cfg.incNextCtxIndex
          $ ICfg.expandCall_ callerCfg callNode callStmt targetCfg targetCtx leaveFuncUUID
    it "should expand a call" $ do
      PrettyShow' expanded `shouldBe` PrettyShow' expandedCfg
