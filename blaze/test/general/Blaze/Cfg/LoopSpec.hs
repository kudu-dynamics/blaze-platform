{- HLINT ignore "Redundant do" -}

module Blaze.Cfg.LoopSpec where

import Blaze.Cfg hiding (func, BasicBlockNode(ctx), CallNode(ctx))
import Blaze.Cfg.Loop
import Blaze.Function (Function (Function))
import Blaze.Graph as G
import Blaze.Prelude
import Blaze.Types.Pil (Ctx(Ctx))
import qualified Data.HashSet as HS
import Test.Hspec
import Blaze.Util.Spec (bb)

ia :: Int64 -> Address
ia = intToAddr

ctx :: Ctx
ctx = Ctx func 0
  where
    func = Function Nothing "foo" (intToAddr 0) []


cfgWithSimpleLoop :: Cfg (CfNode ())
cfgWithSimpleLoop = do
  mkCfg 0
    (bb ctx (ia 0x00) (ia 0x0F) ())
    [ bb ctx (ia 0x10) (ia 0x1F) ()
    , bb ctx (ia 0x20) (ia 0x2F) ()
    , bb ctx (ia 0x30) (ia 0x3F) ()
    , bb ctx (ia 0x40) (ia 0x4F) ()
    , bb ctx (ia 0x50) (ia 0x5F) ()
    ]
    [ CfEdge
        (bb ctx (ia 0x00) (ia 0x0F) ())
        (bb ctx (ia 0x10) (ia 0x1F) ())
        UnconditionalBranch
    , CfEdge
        (bb ctx (ia 0x10) (ia 0x1F) ())
        (bb ctx (ia 0x20) (ia 0x2F) ())
        TrueBranch
    , CfEdge
        (bb ctx (ia 0x10) (ia 0x1F) ())
        (bb ctx (ia 0x30) (ia 0x3F) ())
        FalseBranch
    , CfEdge
        (bb ctx (ia 0x20) (ia 0x2F) ())
        (bb ctx (ia 0x40) (ia 0x4F) ())
        UnconditionalBranch
    , CfEdge
        (bb ctx (ia 0x30) (ia 0x3F) ())
        (bb ctx (ia 0x40) (ia 0x4F) ())
        UnconditionalBranch
    , CfEdge
        (bb ctx (ia 0x40) (ia 0x4F) ())
        (bb ctx (ia 0x10) (ia 0x1F) ())
        TrueBranch
    , CfEdge
        (bb ctx (ia 0x40) (ia 0x4F) ())
        (bb ctx (ia 0x50) (ia 0x5F) ())
        FalseBranch
    ]

spec :: Spec
spec = describe "Blaze.Cfg" $ do
  describe "isBackEdge" $ do
    it "should identify back edges" $ do
      let expected =
            HS.fromList
              [ BackEdge $
                  CfEdge
                    (bb ctx (ia 0x40) (ia 0x4F) ())
                    (bb ctx (ia 0x10) (ia 0x1F) ())
                    TrueBranch
              ]
          actual = HS.fromList (getBackEdges cfgWithSimpleLoop)
      actual `shouldBe` expected

  describe "getBodyNodes" $ do
    it "should get body nodes for loop" $ do

      let expected =
            HS.fromList
              [ bb ctx (ia 0x20) (ia 0x2F) ()
              , bb ctx (ia 0x30) (ia 0x3F) ()
              ]
          actual =
            getBodyNodes cfgWithSimpleLoop
            . BackEdge
            $ CfEdge
              (bb ctx (ia 0x40) (ia 0x4F) ())
              (bb ctx (ia 0x10) (ia 0x1F) ())
              TrueBranch

      PShow actual `shouldBe` PShow expected
  describe "fromBackEdge" $ do
    it "should find a loop from a back edge" $ do
      let expected =
            NatLoop
              (LoopHeader $ bb ctx (ia 0x10) (ia 0x1F) ())
              ( LoopBody $
                  HS.fromList
                    [ bb ctx (ia 0x20) (ia 0x2F) ()
                    , bb ctx (ia 0x30) (ia 0x3F) ()
                    ]
              )
              (LoopTail $ bb ctx (ia 0x40) (ia 0x4F) ())
              ( LoopCfg $
                  mkCfg 0
                    (bb ctx (ia 0x10) (ia 0x1F) ())
                    [ bb ctx (ia 0x20) (ia 0x2F) ()
                    , bb ctx (ia 0x30) (ia 0x3F) ()
                    , bb ctx (ia 0x40) (ia 0x4F) ()
                    ]
                    [ CfEdge
                        (bb ctx (ia 0x10) (ia 0x1F) ())
                        (bb ctx (ia 0x20) (ia 0x2F) ())
                        TrueBranch
                    , CfEdge
                        (bb ctx (ia 0x20) (ia 0x2F) ())
                        (bb ctx (ia 0x40) (ia 0x4F) ())
                        UnconditionalBranch
                    , CfEdge
                        (bb ctx (ia 0x10) (ia 0x1F) ())
                        (bb ctx (ia 0x30) (ia 0x3F) ())
                        FalseBranch
                    , CfEdge
                        (bb ctx (ia 0x30) (ia 0x3F) ())
                        (bb ctx (ia 0x40) (ia 0x4F) ())
                        UnconditionalBranch
                    , CfEdge
                        (bb ctx (ia 0x40) (ia 0x4F) ())
                        (bb ctx (ia 0x10) (ia 0x1F) ())
                        TrueBranch
                    ]
              )
              ( BackEdge $
                  CfEdge
                    (bb ctx (ia 0x40) (ia 0x4F) ())
                    (bb ctx (ia 0x10) (ia 0x1F) ())
                    TrueBranch
              )
          actual =
            fromBackEdge
              cfgWithSimpleLoop
              ( BackEdge $
                  CfEdge
                    (bb ctx (ia 0x40) (ia 0x4F) ())
                    (bb ctx (ia 0x10) (ia 0x1F) ())
                    TrueBranch
              )
      (actual ^. #backEdge) `shouldBe` (expected ^. #backEdge)
      (actual ^. #header) `shouldBe` (expected ^. #header)
      (actual ^. #tail) `shouldBe` (expected ^. #tail)
      (actual ^. #body) `shouldBe` (expected ^. #body)
      G.nodes (actual ^. (#cfg . #cfg)) `shouldBe` G.nodes (expected ^. (#cfg . #cfg))
      G.edges (actual ^. (#cfg . #cfg)) `shouldBe` G.edges (expected ^. (#cfg . #cfg))
