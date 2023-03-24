module Ghidra.PcodeSpec where

import Ghidra.Prelude

import qualified Ghidra.State as State
import qualified Ghidra.Function as Function
import Ghidra.Pcode (getRawPcodeOps, getHighPcodeOps, mkRawPcodeInstruction, mkBareRawPcodeInstruction, getHighPcode, getRawPcode)
import Ghidra.Types.Pcode
import Ghidra.Types.Pcode.Lifted (PcodeOp)
import Ghidra.Types.Variable
import Ghidra.Types.Address
import Ghidra.Address (getAddressSpaceMap)
import Ghidra.Core
import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.gzf"

a1Bin :: FilePath
a1Bin = "res/test_bins/a1/a1.gzf"

spec :: Spec
spec = describe "Ghidra.Pcode" $ do
  gs <- runIO . runGhidraOrError $ do
    gs <- State.openDatabase_ a1Bin >>! State.analyze
    return gs

  context "getRawPcode" $ do
    let faddr = 0x13ad
    (raws, liftedRaws) <- runIO . runGhidraOrError $ do
      faddr' <- State.mkAddressBased gs faddr
      (Just func) <- Function.fromAddr gs faddr'
      raws <- getRawPcodeOps gs func
      addrSpaceMap <- getAddressSpaceMap gs
      liftedRaws <- getRawPcode gs addrSpaceMap func
      return (raws, liftedRaws)

    it "should get raw pcode" $ do
      length raws `shouldBe` 104

    it "should lift raw pcode ops" $ do
      length liftedRaws `shouldBe` 104

    rawInstr <- runIO . runGhidraOrError $ do
      x <- mkBareRawPcodeInstruction . head . fmap snd $ raws
      x' <- mkRawPcodeInstruction x
      return x'

    let expected = PcodeInstruction
          { op = COPY
          , output = Just VarNode
            { varType = Addr Address
                        { space = AddressSpace
                          { ptrSize = 4
                          , addressableUnitSize = 1
                          , name = Unique
                          }
                        , offset = 60672
                        }
            , size = Bytes 8
            }
          , inputs =
              [ VarNode
                { varType =
                    Addr (Address
                           { space = AddressSpace
                                     { ptrSize = 4
                                     , addressableUnitSize = 1
                                     , name = Register
                                     }
                           , offset = 40
                           })
                , size = Bytes 8
                }]}

    it "should convert to raw pcode instruction" $ do
      rawInstr `shouldBe` expected

  context "getHighPcodeOps" $ do
    let faddr = 0x13ad
    (highs, liftedHighs) <- runIO . runGhidraOrError $ do
      faddr' <- State.mkAddressBased gs faddr
      (Just func) <- Function.fromAddr gs faddr'
      hfunc <- Function.getHighFunction gs func
      highs <- getHighPcodeOps gs hfunc func
      addrSpaceMap <- getAddressSpaceMap gs
      liftedHighs <- getHighPcode gs addrSpaceMap hfunc func
      return (highs, liftedHighs)

    it "should get high pcode ops" $ do
      length highs `shouldBe` 15

    it "should lift high pcode ops" $ do
      length liftedHighs `shouldBe` 15


getHighPcodeDemo :: IO [PcodeOp HighVarNode]
getHighPcodeDemo = runGhidraOrError $ do
  gs <- State.openDatabase_ diveBin >>! State.analyze
  -- b <- isNil' $ gs ^. #unGhidraState
  -- when b $ error "Couldn't open a1"
  let cgc_printf_addr = 0x804c6e0
  addr <- State.mkAddress gs cgc_printf_addr
  (Just func) <- Function.fromAddr gs addr
  hfunc <- Function.getHighFunction gs func
  -- highs <- getHighPcodeOps gs hfunc func
  addrSpaceMap <- getAddressSpaceMap gs
  fmap snd <$> getHighPcode gs addrSpaceMap hfunc func
