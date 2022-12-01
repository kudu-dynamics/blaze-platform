module Ghidra.PcodeBlockSpec where

import Ghidra.Prelude

import Ghidra.PcodeBlock
import Ghidra.Types.PcodeBlock (PcodeBlockGraph(PcodeBlockGraph))
import qualified Ghidra.Types.PcodeBlock as PB
import qualified Ghidra.State as State
import qualified Ghidra.Function as Function
import Ghidra.Core
import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.gzf"

a1Bin :: FilePath
a1Bin = "res/test_bins/a1/a1.gzf"

spec :: Spec
spec = describe "Ghidra.PcodeBlock" $ do
  (_gs, _jfunc, hfunc) <- runIO . runGhidraOrError $ do
    gs <- State.openDatabase_ a1Bin >>! State.analyze
    -- b <- isNil' $ gs ^. #unGhidraState
    -- when b $ error "Couldn't open a1"
    let jaddr = 0x1348
    jaddr' <- State.mkAddressBased gs jaddr
    (Just jfunc) <- Function.fromAddr gs jaddr'
    highJfunc <- Function.getHighFunction gs jfunc
    return (gs, jfunc, highJfunc)
  
  context "getCodeBlocks" $ do
    blocks <- runIO . runGhidraOrError $ do
      getBlocksFromHighFunction hfunc
      
    it "should get 4 blocks for j function" $ do
      length blocks `shouldBe` 4

  context "getPcodeBlockGraph" $ do
    g <- runIO . runGhidraOrError $ do
      g <- getPcodeBlockGraph hfunc
      -- g' :: PcodeBlockGraph Address <- traverse (view #startAddress) g
      return $ view #index <$> g
    let g' = g & #nodes %~ sort
               & #edges %~ sort
    let expected = PcodeBlockGraph
          { PB.nodes = sort [0, 1, 2, 3]
          , PB.edges = sort [ (PB.UnconditionalBranch, (0, 1))
                            , (PB.TrueBranch, (1, 2))
                            , (PB.FalseBranch, (1, 3))
                            , (PB.UnconditionalBranch, (2, 1))
                            ]
          }
    it "should generate basic block graph" $ do
      g' `shouldBe` expected
