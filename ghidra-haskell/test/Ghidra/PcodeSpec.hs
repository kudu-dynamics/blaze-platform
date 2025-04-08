module Ghidra.PcodeSpec where

import Ghidra.Prelude hiding (Const)

import qualified Data.BinaryAnalysis as BA
import Ghidra.Address (mkAddress)
import Ghidra.Core
import qualified Ghidra.Function as Function
import Ghidra.Pcode
import Ghidra.Program (getAddressSpaceMap)
import qualified Ghidra.State as State
import qualified Ghidra.Types as J
import Ghidra.Types.Address hiding (Const)
import qualified Ghidra.Types.Pcode.Lifted as Lifted
import Ghidra.Types.Pcode.Lifted as Lifted (Destination(..), Input(..), Output(..))
import Ghidra.Types.Variable

import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.gzf"

a1Bin :: FilePath
a1Bin = "res/test_bins/a1/a1.gzf"

getHighs :: State.GhidraState -> BA.Address -> IO ([(Address, J.PcodeOpAST)], [(Address, Lifted.PcodeOp HighVarNode)])
getHighs gs faddr = runGhidraOrError $ do
  faddr' <- State.mkAddressBased gs faddr
  (Just func) <- Function.fromAddr gs faddr'
  hfunc <- Function.getHighFunction gs func
  highs <- getHighPcodeOps gs hfunc func
  addrSpaceMap <- getAddressSpaceMap (gs ^. #program)
  liftedHighs <- getHighPcode gs addrSpaceMap hfunc func
  return (highs, liftedHighs)

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
      -- runIO $ pprint $ length liftedRaws

    it "should get raw pcode" $ do
      length raws `shouldBe` 100

    it "should lift raw pcode ops" $ do
      length liftedRaws `shouldBe` 100

    rawInstr <- runIO . runGhidraOrError $ do
      x <- mkBareRawPcodeInstruction . head . fmap snd $ raws
      x' <- mkRawPcodeInstruction x
      return x'
    -- runIO $ pprint rawInstr

    let expected = PcodeInstruction
          { op = COPY
          , output = Just VarNode
            { varType =
              Addr
              { location =
                  Address
                  { space = AddressSpace
                    { id = AddressSpaceId 291
                    , ptrSize = 4
                    , addressableUnitSize = 1
                    , name = Unique
                    }
                  , offset = 146432
                  }
              , pcAddress = Nothing
              }
            , size = Bytes 8
            }
          , inputs =
              [ VarNode
                { varType =
                    Addr
                    { location =
                        Address
                        { space = AddressSpace
                                  { id = AddressSpaceId 548
                                  , ptrSize = 4
                                  , addressableUnitSize = 1
                                  , name = Register
                                  }
                        , offset = 40
                        }
                    , pcAddress = Nothing
                    }
                , size = Bytes 8
                }]}

    it "should convert to raw pcode instruction" $ do
      rawInstr `shouldBe` expected

  context "getHighPcodeOps" $ do
    context "jenkins" $ do
      beforeAll (getHighs gs 0x13ad) $ do
        it "should get high pcode ops" $ \(highs, _) -> do
          length highs `shouldBe` 15

        it "should lift high pcode ops" $ \(_, liftedHighs) -> do
          length liftedHighs `shouldBe` 15

        it "param HighVariable should be named param_1" $ \(_, liftedHighs) -> do
          let expected = Just "param_1"
              name = liftedHighs ^? ix 1 . _2 . #_INT_NOTEQUAL . _2 . #value . #highVariable . _Just . #highSymbol . _Just . #name . _Just
              isParam = liftedHighs ^? ix 1 . _2 . #_INT_NOTEQUAL . _2 . #value . #highVariable . _Just . #highSymbol . _Just . #isParameter
          name `shouldBe` expected
          isParam `shouldBe` Just True

    context "g" $ do
      beforeAll (getHighs gs 0x115e) $ do
        it "should lift high pcode ops" $ \(_, liftedHighs) -> do
          -- FIXME: I believe this should be 134 after we change how 'getHighPcodeOps' works
          length liftedHighs `shouldBe` 176
          fAddr <- runGhidraOrError $ State.mkAddressBased gs 0x1145 >>= mkAddress
          snd (liftedHighs !! 7) `shouldSatisfy` \case
            -- rax <- f(rdi, 80)
            Lifted.CALL
              (Just (Output HighVarNode{varType=Addr{location=Address{space=AddressSpace{name=Register}, offset=0}}}))
              (Input 0 (Absolute fAddr'))
              [ Input 1 (HighVarNode{varType=Addr{location=Address{space=AddressSpace{name=Register}, offset = 56}}})
              , Input 2 (HighVarNode{varType=Const 80})
              ]
              | fAddr == fAddr' -> True
            _ -> False
