module Ghidra.PcodeSpec where

import Ghidra.Prelude

import Ghidra.Core
import qualified Ghidra.Function as Function
import Ghidra.Pcode
import Ghidra.Program (getAddressSpaceMap)
import qualified Ghidra.State as State
import Ghidra.Types.Address
import Ghidra.Types.Variable

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
  let db = gs ^. #program

  context "getRawPcode" $ do
    let faddr = 0x13ad
    (raws, liftedRaws) <- runIO . runGhidraOrError $ do
      faddr' <- State.mkAddressBased gs faddr
      (Just func) <- Function.fromAddr gs faddr'
      raws <- getRawPcodeOps gs func
      addrSpaceMap <- getAddressSpaceMap db
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
                          { id = AddressSpaceId 291
                          , ptrSize = 4
                          , addressableUnitSize = 1
                          , name = Unique
                          }
                        , offset = 61312
                        }
            , size = Bytes 8
            }
          , inputs =
              [ VarNode
                { varType =
                    Addr (Address
                           { space = AddressSpace
                                     { id = AddressSpaceId 548
                                     , ptrSize = 4
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
      addrSpaceMap <- getAddressSpaceMap db
      liftedHighs <- getHighPcode gs addrSpaceMap hfunc func
      return (highs, liftedHighs)

    it "should get high pcode ops" $ do
      length highs `shouldBe` 15

    it "should lift high pcode ops" $ do
      length liftedHighs `shouldBe` 15
