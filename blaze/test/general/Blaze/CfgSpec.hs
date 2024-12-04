{- HLINT ignore "Redundant do" -}

module Blaze.CfgSpec where

import qualified Data.HashSet as HashSet

import Blaze.Function (Function (Function))
import Blaze.Cfg hiding (func, BasicBlockNode(ctx), CallNode(ctx))
import qualified Blaze.Cfg as Cfg
import Blaze.Prelude
import Test.Hspec
import Blaze.Types.Pil (Ctx(Ctx))
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Pil.Construct as C
import Blaze.Util.Spec (bb, mkUuid2)


ctx :: Ctx
ctx = Ctx func 0
  where
    func = Function Nothing "foo" 0x00 []

bbn :: Text -> CfNode Text
bbn name = bb ctx x x name
  where
    x = fromIntegral $ hash name

{-
        [root]
           |
        [goto]
           |
       [if node]
       ___/ \___
      /         \
[true node] [gotoFalse]
     |           |
     |      [false node]
      \___   ___/
          \ /
    [terminal node]
-}

simpleCfg :: Cfg (CfNode Text)
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

{-
     [00]
      |
     [10]
      |
     [20]
     / \
    /   \
  [30] [40]
   |    |
   |   [50]
    \   /
     \ /
     [60]
-}

simplePilCfg :: PilCfg
simplePilCfg = mkCfg 0
  (bb ctx 0x00 0x0f [C.nop])
  [ bb ctx 0x00 0x0f [C.nop]
  , bb ctx 0x10 0x1f [C.nop]
  , bb ctx 0x20 0x2f [C.nop]
  , bb ctx 0x30 0x3f [C.nop]
  , bb ctx 0x40 0x4f [C.nop]
  , bb ctx 0x50 0x5f [C.nop]
  ]
  [ CfEdge
    (bb ctx 0x00 0x0f [C.nop])
    (bb ctx 0x10 0x1f [C.nop])
    UnconditionalBranch
  , CfEdge
    (bb ctx 0x10 0x1f [C.nop])
    (bb ctx 0x20 0x2f [C.nop])
    UnconditionalBranch
  , CfEdge
    (bb ctx 0x20 0x2f [C.nop])
    (bb ctx 0x30 0x3f [C.nop])
    TrueBranch
  , CfEdge
    (bb ctx 0x20 0x2f [C.nop])
    (bb ctx 0x40 0x4f [C.nop])
    FalseBranch
  , CfEdge
    (bb ctx 0x30 0x3f [C.nop])
    (bb ctx 0x60 0x6f [C.nop])
    UnconditionalBranch
  , CfEdge
    (bb ctx 0x40 0x4f [C.nop])
    (bb ctx 0x50 0x5f [C.nop])
    UnconditionalBranch
  , CfEdge
    (bb ctx 0x50 0x5f [C.nop])
    (bb ctx 0x60 0x6f [C.nop])
    UnconditionalBranch
  ]

{-
        [root]
           |
        [goto]
           |
       [if node]
       ___/ \___
      /         \
[true node] [gotoFalse]
                 |
            [false node]
-}

cfgWithTwoTerminals :: Cfg (CfNode Text)
cfgWithTwoTerminals = mkCfg 0
  (bbn "root")
  [ bbn "goto"
  , bbn "if node"
  , bbn "true node"
  , bbn "gotoFalse"
  , bbn "false node"
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
  ]

{-
  [00]
    |
  [10]
    |
  [20]
   / \
  /   \
[30] [40]
      |
     [50]
-}

pilCfgWithTwoTerminals :: PilCfg
pilCfgWithTwoTerminals = mkCfg 0
  (bb ctx 0x00 0x0f [C.nop])
  [ bb ctx 0x00 0x0f [C.nop]
  , bb ctx 0x10 0x1f [C.nop]
  , bb ctx 0x20 0x2f [C.nop]
  , bb ctx 0x30 0x3f [C.nop]
  , bb ctx 0x40 0x4f [C.nop]
  ]
  [ CfEdge
    (bb ctx 0x00 0x0f [C.nop])
    (bb ctx 0x10 0x1f [C.nop])
    UnconditionalBranch
  , CfEdge
    (bb ctx 0x10 0x1f [C.nop])
    (bb ctx 0x20 0x2f [C.nop])
    UnconditionalBranch
  , CfEdge
    (bb ctx 0x20 0x2f [C.nop])
    (bb ctx 0x30 0x3f [C.nop])
    TrueBranch
  , CfEdge
    (bb ctx 0x20 0x2f [C.nop])
    (bb ctx 0x40 0x4f [C.nop])
    FalseBranch
  , CfEdge
    (bb ctx 0x40 0x4f [C.nop])
    (bb ctx 0x50 0x5f [C.nop])
    UnconditionalBranch
  ]

{-
          [00]
     _____  |
    /     \ |
    |     [10]
    |     /  \
    |  [20]  [30]
    |     \  /
    |     [40]
    \_____/  \
            [50]
-}

pilCfgWithSimpleLoop :: PilCfg
pilCfgWithSimpleLoop =
  mkCfg 0
    (bb ctx 0x00 0x0F [C.nop])
    [ bb ctx 0x10 0x1F [C.nop]
    , bb ctx 0x20 0x2F [C.nop]
    , bb ctx 0x30 0x3F [C.nop]
    , bb ctx 0x40 0x4F [C.nop]
    , bb ctx 0x50 0x5F [C.nop]
    ]
    [ CfEdge
        (bb ctx 0x00 0x0F [C.nop])
        (bb ctx 0x10 0x1F [C.nop])
        UnconditionalBranch
    , CfEdge
        (bb ctx 0x10 0x1F [C.nop])
        (bb ctx 0x20 0x2F [C.nop])
        TrueBranch
    , CfEdge
        (bb ctx 0x10 0x1F [C.nop])
        (bb ctx 0x30 0x3F [C.nop])
        FalseBranch
    , CfEdge
        (bb ctx 0x20 0x2F [C.nop])
        (bb ctx 0x40 0x4F [C.nop])
        UnconditionalBranch
    , CfEdge
        (bb ctx 0x30 0x3F [C.nop])
        (bb ctx 0x40 0x4F [C.nop])
        UnconditionalBranch
    , CfEdge
        (bb ctx 0x40 0x4F [C.nop])
        (bb ctx 0x10 0x1F [C.nop])
        TrueBranch
    , CfEdge
        (bb ctx 0x40 0x4F [C.nop])
        (bb ctx 0x50 0x5F [C.nop])
        FalseBranch
    ]

{-
          [00]
     _____  |
    /     \ |
    |     [10]
    |     /  \
    |  [20]  [30]
    |     \  /
    |     [40]
    \_____/
-}

pilCfgWithInfiniteLoop :: PilCfg
pilCfgWithInfiniteLoop =
  mkCfg 0
    (bb ctx 0x00 0x0F [C.nop])
    [ bb ctx 0x10 0x1F [C.nop]
    , bb ctx 0x20 0x2F [C.nop]
    , bb ctx 0x30 0x3F [C.nop]
    , bb ctx 0x40 0x4F [C.nop]
    ]
    [ CfEdge
        (bb ctx 0x00 0x0F [C.nop])
        (bb ctx 0x10 0x1F [C.nop])
        UnconditionalBranch
    , CfEdge
        (bb ctx 0x10 0x1F [C.nop])
        (bb ctx 0x20 0x2F [C.nop])
        TrueBranch
    , CfEdge
        (bb ctx 0x10 0x1F [C.nop])
        (bb ctx 0x30 0x3F [C.nop])
        FalseBranch
    , CfEdge
        (bb ctx 0x20 0x2F [C.nop])
        (bb ctx 0x40 0x4F [C.nop])
        UnconditionalBranch
    , CfEdge
        (bb ctx 0x30 0x3F [C.nop])
        (bb ctx 0x40 0x4F [C.nop])
        UnconditionalBranch
    , CfEdge
        (bb ctx 0x40 0x4F [C.nop])
        (bb ctx 0x10 0x1F [C.nop])
        UnconditionalBranch
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

  let bb' start end = Cfg.BasicBlockNode ctx start end (mkUuid2 start end)

  context "parseTerminalNode" $ do
    it "should parse Return nodes" $ do
      let ret = Pil.RetOp $ C.const 172844 4
          retStmt = Pil.Stmt 0x0 . Pil.Ret $ ret
          node = bb' 0x00 0x0f [C.nop, retStmt]
          node' = bb' 0x00 0x0f [retStmt]
      Cfg.parseTerminalNode (Cfg.BasicBlock node) `shouldBe` Cfg.TermRet (Cfg.ReturnNode node ret)
      Cfg.parseTerminalNode (Cfg.BasicBlock node') `shouldBe` Cfg.TermRet (Cfg.ReturnNode node' ret)

    it "should parse Exit nodes" $ do
      let exitStmt = Pil.Stmt 0 Pil.Exit
          node = bb' 0x00 0x0f [C.nop, exitStmt]
          node' = bb' 0x00 0x0f [exitStmt]
      Cfg.parseTerminalNode (Cfg.BasicBlock node) `shouldBe` Cfg.TermExit (Cfg.ExitNode node)
      Cfg.parseTerminalNode (Cfg.BasicBlock node') `shouldBe` Cfg.TermExit (Cfg.ExitNode node')

    it "should parse NoRet nodes" $ do
      let noRetStmt = Pil.Stmt 0 Pil.NoRet
          node = bb' 0x00 0x0f [C.nop, noRetStmt]
          node' = bb' 0x00 0x0f [noRetStmt]
      Cfg.parseTerminalNode (Cfg.BasicBlock node) `shouldBe` Cfg.TermNoRet (Cfg.NoRetNode node)
      Cfg.parseTerminalNode (Cfg.BasicBlock node') `shouldBe` Cfg.TermNoRet (Cfg.NoRetNode node')

    it "should parse Jump nodes" $ do
      let jump = Pil.JumpOp $ C.const 385018 4
          jumpStmt = Pil.Stmt 0 . Pil.Jump $ jump
          node = bb' 0x00 0x0f [C.nop, jumpStmt]
          node' = bb' 0x00 0x0f [jumpStmt]
      Cfg.parseTerminalNode (Cfg.BasicBlock node) `shouldBe` Cfg.TermJump (Cfg.JumpNode node jump)
      Cfg.parseTerminalNode (Cfg.BasicBlock node') `shouldBe` Cfg.TermJump (Cfg.JumpNode node' jump)

    it "should parse any other nodes" $ do
      let node1 = bb ctx 0x00 0x0f []
          node2 = bb ctx 0x10 0x1f [C.nop]
          node3 = bb ctx 0x20 0x2f [C.def "x" $ C.add (C.const 1234 4) (C.const 1947 4) 4]
      Cfg.parseTerminalNode node1 `shouldBe` Cfg.TermOther node1
      Cfg.parseTerminalNode node2 `shouldBe` Cfg.TermOther node2
      Cfg.parseTerminalNode node3 `shouldBe` Cfg.TermOther node3

  context "getTerminalBlocks" $ do
    it "should find a single terminal block" $ do
      getTerminalBlocks simplePilCfg
        `shouldBe` HashSet.fromList [TermOther $ bb ctx 0x60 0x6f [C.nop]]
      getTerminalBlocks pilCfgWithSimpleLoop
        `shouldBe` HashSet.fromList [TermOther $ bb ctx 0x50 0x5f [C.nop]]

    it "should find multiple terminal blocks" $ do
      getTerminalBlocks pilCfgWithTwoTerminals
        `shouldBe`
        HashSet.fromList
          [ TermOther $ bb ctx 0x30 0x3f [C.nop]
          , TermOther $ bb ctx 0x50 0x5f [C.nop]
          ]

    it "should fail to find terminal blocks given a loop with no exit" $ do
      getTerminalBlocks pilCfgWithInfiniteLoop `shouldBe` HashSet.empty
