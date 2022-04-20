{- HLINT ignore "Redundant do" -}

module Blaze.CfgSpec where

import Blaze.Function (Function (Function))
import Blaze.Cfg hiding (func, BasicBlockNode(ctx), CallNode(ctx))
import qualified Blaze.Cfg as Cfg
import Blaze.Prelude
import Test.Hspec
import Blaze.Types.Pil (Ctx(Ctx))
import Blaze.Util.Spec (bb, mkUuid1)


ctx :: Ctx
ctx = Ctx func 0
  where
    func = Function Nothing "foo" 0x00 []

bbn :: Text -> CfNode Text
bbn name = bb ctx x x name
  where
    x = fromIntegral $ hash name

simpleCfg :: Cfg Text
simpleCfg = mkCfg 0
  (bbn "root")
  [ bbn "goto"
  , bbn "if node"
  , bbn "true node"
  , bbn "gotoFalse"
  , bbn "false node"
  , bbn "terminal"
  ]
  [ CfEdge
    (bbn "root")
    (bbn "goto")
    UnconditionalBranch
  , CfEdge
    (bbn "goto")
    (bbn "if node")
    UnconditionalBranch
  , CfEdge
    (bbn "if node")
    (bbn "true node")
    TrueBranch
  , CfEdge
    (bbn "if node")
    (bbn "gotoFalse")
    FalseBranch
  , CfEdge
    (bbn "gotoFalse")
    (bbn "false node")
    UnconditionalBranch
  , CfEdge
    (bbn "true node")
    (bbn "terminal")
    UnconditionalBranch
  , CfEdge
    (bbn "false node")
    (bbn "terminal")
    UnconditionalBranch
  ]
  
cfgWithSimpleLoop :: Cfg ()
cfgWithSimpleLoop =
  mkCfg 0
    (bb ctx 0x00 0x0F ())
    [ bb ctx 0x10 0x1F ()
    , bb ctx 0x20 0x2F ()
    , bb ctx 0x30 0x3F ()
    , bb ctx 0x40 0x4F ()
    , bb ctx 0x50 0x5F ()
    ]
    [ CfEdge
        (bb ctx 0x00 0x0F ())
        (bb ctx 0x10 0x1F ())
        UnconditionalBranch
    , CfEdge
        (bb ctx 0x10 0x1F ())
        (bb ctx 0x20 0x2F ())
        TrueBranch
    , CfEdge
        (bb ctx 0x10 0x1F ())
        (bb ctx 0x30 0x3F ())
        FalseBranch
    , CfEdge
        (bb ctx 0x20 0x2F ())
        (bb ctx 0x40 0x4F ())
        UnconditionalBranch
    , CfEdge
        (bb ctx 0x30 0x3F ())
        (bb ctx 0x40 0x4F ())
        UnconditionalBranch
    , CfEdge
        (bb ctx 0x40 0x4F ())
        (bb ctx 0x10 0x1F ())
        TrueBranch
    , CfEdge
        (bb ctx 0x40 0x4F ())
        (bb ctx 0x50 0x5F ())
        FalseBranch
    ]

spec :: Spec
spec = describe "Blaze.Cfg" $ do
  context "Cfg Graph API" $ do
    it "should remove node and attached edges" $ do
      let expected = mkCfg 0
            (bbn "root")
            [ bbn "if node"
            , bbn "true node"
            , bbn "false node"
            , bbn "gotoFalse"
            , bbn "terminal"
            ]
            [ CfEdge
              (bbn "if node")
              (bbn "true node")
              TrueBranch
            , CfEdge
              (bbn "if node")
              (bbn "gotoFalse")
              FalseBranch
            , CfEdge
              (bbn "gotoFalse")
              (bbn "false node")
              UnconditionalBranch
            , CfEdge
              (bbn "true node")
              (bbn "terminal")
              UnconditionalBranch
            , CfEdge
              (bbn "false node")
              (bbn "terminal")
              UnconditionalBranch
            ]
      Cfg.removeNode (bbn "goto") simpleCfg `shouldBe` expected

  context "removeAndRebindEdges" $ do
    it "should remove node and rebind edges" $ do
      let expected = mkCfg 0
            (bbn "root")
            [ bbn "if node"
            , bbn "true node"
            , bbn "gotoFalse"
            , bbn "false node"
            , bbn "terminal"
            ]
            [ CfEdge
              (bbn "root")
              (bbn "if node")
              UnconditionalBranch
            , CfEdge
              (bbn "if node")
              (bbn "true node")
              TrueBranch
            , CfEdge
              (bbn "if node")
              (bbn "gotoFalse")
              FalseBranch
            , CfEdge
              (bbn "gotoFalse")
              (bbn "false node")
              UnconditionalBranch
            , CfEdge
              (bbn "true node")
              (bbn "terminal")
              UnconditionalBranch
            , CfEdge
              (bbn "false node")
              (bbn "terminal")
              UnconditionalBranch
            ]
      Cfg.removeAndRebindEdges (bbn "goto") simpleCfg
        `shouldBe`
        expected

    it "should rebind node with pred->node edge branch label" $ do
      let inputCfg = mkCfg 0
            (bbn "root")
            [ bbn "goto"
            , bbn "terminal"
            ]
            [ CfEdge
              (bbn "root")
              (bbn "goto")
              TrueBranch
            , CfEdge
              (bbn "goto")
              (bbn "Terminal")
              UnconditionalBranch
            ]
          expected = mkCfg 0
            (bbn "root")
            [ bbn "terminal" ]
            [ CfEdge
              (bbn "root")
              (bbn "Terminal")
              TrueBranch
            ]
      Cfg.removeAndRebindEdges (bbn "goto") inputCfg
        `shouldBe`
        expected

  context "getDominators" $ do
    -- TODO: Add tests
    it "should find dominators" $ do
      True `shouldBe` True
