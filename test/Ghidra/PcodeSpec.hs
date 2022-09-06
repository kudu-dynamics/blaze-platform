module Ghidra.PcodeSpec where

import Ghidra.Prelude

import qualified Ghidra.State as State
import qualified Ghidra.Function as Function
import Ghidra.Pcode (getRawPcodeOps, getHighPcodeOps, mkRawPcodeInstruction, mkBareRawPcodeInstruction, getHighPcode)
import Ghidra.Types.Pcode
import Ghidra.Types.Variable
import Ghidra.Types.Address
import Ghidra.Address (getAddressSpaceMap)
import Ghidra.Core
import Language.Clojure.Core
import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger"

a1Bin :: FilePath
a1Bin = "res/test_bins/a1/a1"

spec :: Spec
spec = describe "Ghidra.Pcode" $ do
  gs <- runIO . runGhidra $ do
    gs <- State.openDatabase a1Bin >>= State.analyze
    b <- isNil' $ gs ^. #unGhidraState
    when b $ error "Couldn't open a1"
    return gs
  
  context "getRawPcode" $ do
    let faddr = 0x13ad
    raws <- runIO . runGhidra $ do
      faddr' <- State.mkAddressBased gs faddr
      (Just func) <- Function.fromAddr gs faddr'
      getRawPcodeOps gs func
      
    it "should get raw pcode" $ do
      length raws `shouldBe` 104

    rawInstr <- runIO . runGhidra $ do
      x <- mkBareRawPcodeInstruction $ head raws
      x' <- mkRawPcodeInstruction x
      print x'
      return x'

    let expected = PcodeInstruction
          { op = COPY
          , output = Nothing
          , inputs =
              [ VarNode
                { varType =
                    Addr (Address
                           { space = AddressSpace
                                     { ptrSize = 4
                                     , addressableUnitSize = 1
                                     , name = "register"
                                     }
                           , offset = 40
                           })
                , size = Bytes 8
                }]}

    it "should convert to raw pcode instruction" $ do
      rawInstr `shouldBe` expected

  context "getHighPcodeOps" $ do
    let faddr = 0x13ad
    (highs, liftedHighs) <- runIO . runGhidra $ do
      faddr' <- State.mkAddressBased gs faddr
      (Just func) <- Function.fromAddr gs faddr'
      hfunc <- Function.getHighFunction gs func
      highs <- getHighPcodeOps gs hfunc func
      addrSpaceMap <- getAddressSpaceMap gs
      liftedHighs <- getHighPcode gs addrSpaceMap hfunc
      return (highs, liftedHighs)
      
    it "should get high pcode ops" $ do
      length highs `shouldBe` 15

    it "should lift high pcode ops" $ do
      length liftedHighs `shouldBe` 15

