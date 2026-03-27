{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

module Blaze.Cfg.InterproceduralSpec where

import Blaze.Prelude hiding (Symbol)

import Blaze.Cfg hiding
  ( BasicBlockNode (ctx)
  , CallNode (ctx)
  , func
  )
import qualified Blaze.Cfg as Cfg
import Blaze.Function (Function (Function))
import qualified Blaze.Function as Func
import qualified Blaze.Pil.Construct as C
import Blaze.Types.Pil (Ctx (Ctx), CtxId (CtxId))
import qualified Blaze.Types.Pil as Pil
import Blaze.Util.Spec (mkUuid1, mkCallNode, defaultSize)
import qualified Blaze.Cfg.Interprocedural as ICfg
import Blaze.Pretty (PrettyShow'(PrettyShow'))
import Test.Hspec


bbp :: Ctx -> Text -> [Pil.Stmt] -> CfNode [Pil.Stmt]
bbp ctx name stmts = BasicBlock $ BasicBlockNode
  { ctx = ctx
  , start = intToAddr 0
  , end = intToAddr 0
  , uuid = uuid'
  , nodeData = stmts
  }
  where
    uuid' = mkUuid1 . hash $ name

callerFunc :: Function
callerFunc = Function
  { symbol = Nothing
  , name = "caller"
  , address = intToAddr 0
  , params = []
  }

targetFunc :: Function
targetFunc = Function
  { symbol = Nothing
  , name = "targetFunc"
  , address = intToAddr 100
  , params = [ Func.FuncParamInfo $ Func.ParamInfo "arg1" Nothing Func.In
             , Func.FuncParamInfo $ Func.ParamInfo "arg2" Nothing Func.In
             ]
  }

callerCurrentCtxId :: CtxId
callerCurrentCtxId = CtxId 0

callerCtx :: Ctx
callerCtx = Ctx callerFunc callerCurrentCtxId False

targetCurrentCtxId :: CtxId
targetCurrentCtxId = CtxId 1

targetCtx :: Ctx
targetCtx = Ctx targetFunc targetCurrentCtxId False

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
    (callNodeInner1, rawCallStmt1) = mkCallNode callerCtx "call1" (C.pilVar 8 "funcRet1") targetFunc [C.var "a1" 8, C.var "b1" 8]
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

    arg1 = Pil.PilVar defaultSize (Just targetCtx) Nothing "arg1" False Pil.UnknownLocation
    arg2 = Pil.PilVar defaultSize (Just targetCtx) Nothing "arg2" False Pil.UnknownLocation

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
      $ Cfg.EnterFuncNode callerCtx targetCtx (intToAddr 0) (callNodeInner1 ^. #uuid)
        [ C.def' arg1 (C.var "a1" 8)
        , C.def' arg2 (C.var "b1" 8)
        ]

    leaveFuncUUID' = mkUuid1 (88 :: Int)
    leaveFuncNode3 = Cfg.LeaveFunc
      $ Cfg.LeaveFuncNode targetCtx callerCtx (intToAddr 0) leaveFuncUUID'
        [ C.def' retVar0 (C.var "r2" 8)
        , C.defPhi' (C.pilVar 8 "funcRet1") [retVar0]
        ]
      where
        retVar0 = Pil.PilVar defaultSize (Just targetCtx) Nothing "retVar_0" False Pil.UnknownLocation

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
  let pilVar' ctx = C.pilVar_ defaultSize (Just ctx)
  context "mkEnterFuncNode" $ do
    let uuid' = mkUuid1 (1 :: Int)
        rawCallStmt :: Pil.Stmt
        rawCallStmt = C.defCall' (pilVar' callerCtx "outerRet")
                      (Pil.CallFunc targetFunc)
                      [ C.var' (pilVar' callerCtx "x") 8
                      , C.var' (pilVar' callerCtx "y") 8
                      ]
                      8
        callStmt' :: Pil.CallStatement
        callStmt' = fromJust $ Pil.mkCallStatement rawCallStmt
        result = ICfg.mkEnterFuncNode uuid' callerCtx targetCtx callStmt'
        expected :: Cfg.EnterFuncNode [Pil.Stmt]
        expected = Cfg.EnterFuncNode
          { prevCtx = callerCtx
          , nextCtx = targetCtx
          , callSiteAddress = intToAddr 0
          , uuid = uuid'
          , nodeData =
            [ C.def' (pilVar' targetCtx "arg1") (C.var' (pilVar' callerCtx "x") 8)
            , C.def' (pilVar' targetCtx "arg2") (C.var' (pilVar' callerCtx "y") 8)
            ]
          }
    it "should make enter func node" $ do
      PrettyShow' (result ^. #nodeData) `shouldBe` PrettyShow' (expected ^. #nodeData)
      PrettyShow' result `shouldBe` PrettyShow' expected

  context "mkLeaveFuncNode" $ do
    let uuid' = mkUuid1 (1 :: Int)
        rawCallStmt :: Pil.Stmt
        rawCallStmt = C.defCall' (pilVar' callerCtx "outerRet")
                      (Pil.CallFunc targetFunc)
                      [ C.var' (pilVar' callerCtx "x") 8
                      , C.var' (pilVar' callerCtx "y") 8
                      ]
                      8
        callStmt' :: Pil.CallStatement
        callStmt' = fromJust $ Pil.mkCallStatement rawCallStmt
        retExprs = [ C.var' (pilVar' targetCtx "innerRet1") 8
                   , C.var' (pilVar' targetCtx "innerRet2") 8
                   ]
        result = ICfg.mkLeaveFuncNode uuid' callerCtx targetCtx callStmt' retExprs
        retVar0 = Pil.PilVar defaultSize (Just targetCtx) Nothing "retVar_0" False Pil.UnknownLocation
        retVar1 = Pil.PilVar defaultSize (Just targetCtx) Nothing "retVar_1" False Pil.UnknownLocation
        expected :: Cfg.LeaveFuncNode [Pil.Stmt]
        expected = Cfg.LeaveFuncNode
          { prevCtx = targetCtx
          , nextCtx = callerCtx
          , callSiteAddress = intToAddr 0
          , uuid = uuid'
          , nodeData =
            [ C.def' retVar0 (C.var' (pilVar' targetCtx "innerRet1") 8)
            , C.def' retVar1 (C.var' (pilVar' targetCtx "innerRet2") 8)
            , Pil.Stmt (intToAddr 0)
              . Pil.DefPhi
              $ Pil.DefPhiOp (pilVar' callerCtx "outerRet") [retVar0, retVar1]
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
