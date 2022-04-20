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

ctx :: Ctx
ctx = Ctx func 0
  where
    func = Function Nothing "foo" 0x00 []
  

cfgWithSimpleLoop :: Cfg ()
cfgWithSimpleLoop = do
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
  describe "isBackEdge" $ do
    it "should identify back edges" $ do
      let expected =
            HS.fromList
              [ BackEdge $
                  CfEdge
                    (bb ctx 0x40 0x4F ())
                    (bb ctx 0x10 0x1F ())
                    TrueBranch
              ]
          actual = HS.fromList (getBackEdges cfgWithSimpleLoop)
      actual `shouldBe` expected

  describe "getBodyNodes" $ do
    it "should get body nodes for loop" $ do
      
      let expected =
            HS.fromList
              [ bb ctx 0x20 0x2F ()
              , bb ctx 0x30 0x3F ()
              ]
          actual =
            getBodyNodes cfgWithSimpleLoop
            . BackEdge
            $ CfEdge
              (bb ctx 0x40 0x4F ())
              (bb ctx 0x10 0x1F ())
              TrueBranch

      PShow actual `shouldBe` PShow expected
  describe "fromBackEdge" $ do
    it "should find a loop from a back edge" $ do
      let expected =
            NatLoop
              (LoopHeader $ bb ctx 0x10 0x1F ())
              ( LoopBody $
                  HS.fromList
                    [ bb ctx 0x20 0x2F ()
                    , bb ctx 0x30 0x3F ()
                    ]
              )
              (LoopTail $ bb ctx 0x40 0x4F ())
              ( LoopCfg $
                  mkCfg 0
                    (bb ctx 0x10 0x1F ())
                    [ bb ctx 0x20 0x2F ()
                    , bb ctx 0x30 0x3F ()
                    , bb ctx 0x40 0x4F ()
                    ]
                    [ CfEdge
                        (bb ctx 0x10 0x1F ())
                        (bb ctx 0x20 0x2F ())
                        TrueBranch
                    , CfEdge
                        (bb ctx 0x20 0x2F ())
                        (bb ctx 0x40 0x4F ())
                        UnconditionalBranch
                    , CfEdge
                        (bb ctx 0x10 0x1F ())
                        (bb ctx 0x30 0x3F ())
                        FalseBranch
                    , CfEdge
                        (bb ctx 0x30 0x3F ())
                        (bb ctx 0x40 0x4F ())
                        UnconditionalBranch
                    , CfEdge
                        (bb ctx 0x40 0x4F ())
                        (bb ctx 0x10 0x1F ())
                        TrueBranch
                    ]
              )
              ( BackEdge $
                  CfEdge
                    (bb ctx 0x40 0x4F ())
                    (bb ctx 0x10 0x1F ())
                    TrueBranch
              )
          actual =
            fromBackEdge
              cfgWithSimpleLoop
              ( BackEdge $
                  CfEdge
                    (bb ctx 0x40 0x4F ())
                    (bb ctx 0x10 0x1F ())
                    TrueBranch
              )
      (actual ^. #backEdge) `shouldBe` (expected ^. #backEdge)
      (actual ^. #header) `shouldBe` (expected ^. #header)
      (actual ^. #tail) `shouldBe` (expected ^. #tail)
      (actual ^. #body) `shouldBe` (expected ^. #body)
      G.nodes (actual ^. (#cfg . #cfg)) `shouldBe` G.nodes (expected ^. (#cfg . #cfg))
      G.edges (actual ^. (#cfg . #cfg)) `shouldBe` G.edges (expected ^. (#cfg . #cfg))
