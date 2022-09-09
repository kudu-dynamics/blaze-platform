module Ghidra.BasicBlockSpec where

import Ghidra.Prelude

import Ghidra.BasicBlock
import Ghidra.Types.BasicBlock (BasicBlockGraph(BasicBlockGraph))
import qualified Ghidra.Types.BasicBlock as BB
import qualified Ghidra.State as State
import qualified Ghidra.Function as Function
import Ghidra.Types.Variable
import Ghidra.Types.Address
import qualified Ghidra.Types as J
import Ghidra.Address (getAddressSpaceMap)
import Ghidra.Core
import Language.Clojure.Core
import Test.Hspec



diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger"

a1Bin :: FilePath
a1Bin = "res/test_bins/a1/a1"

spec :: Spec
spec = describe "Ghidra.BasicBlock" $ do
  (gs, jfunc, highJfunc) <- runIO . runGhidra $ do
    gs <- State.openDatabase a1Bin >>= State.analyze
    b <- isNil' $ gs ^. #unGhidraState
    when b $ error "Couldn't open a1"
    let jaddr = 0x1348
    jaddr' <- State.mkAddressBased gs jaddr
    (Just jfunc) <- Function.fromAddr gs jaddr'
    highJfunc <- Function.getHighFunction gs jfunc
    return (gs, jfunc, highJfunc)
  
  context "getCodeBlocks" $ do
    blocks <- runIO . runGhidra $ do
      getCodeBlocks gs jfunc
      
    it "should get 4 blocks for j function" $ do
      length blocks `shouldBe` 4

  context "getBasicBlockGraph" $ do
    g <- runIO . runGhidra $ do
      g <- getBasicBlockGraph gs jfunc
      -- g' :: BasicBlockGraph Address <- traverse (view #startAddress) g
      return $ view (#startAddress . #offset) <$> g
    let expected = BasicBlockGraph
          { BB.nodes = [1053512, 1053557, 1053592, 1053598]
          , BB.edges = [(1053512, 1053592), (1053557, 1053592), (1053592, 1053557), (1053592, 1053598)]
          }
    it "should generate basic block graph" $ do
      g `shouldBe` expected
