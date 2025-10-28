module Ghidra.AddressSpec where

import Ghidra.Prelude

import Test.Hspec

import Ghidra.Core
import Ghidra.Program
import qualified Ghidra.State as State
import Ghidra.Address (mkAddress)

import qualified Data.BinaryAnalysis as BA
import qualified Data.HashMap.Strict as HashMap


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger"

a1Bin :: FilePath
a1Bin = "res/test_bins/a1/a1"

spec :: Spec
spec = describe "Ghidra.Address" $ do
  gs <- runIO . runGhidraOrError $ do
    gs <- State.openDatabase_ a1Bin >>! State.analyze
    -- b <- isNil' $ gs ^. #unGhidraState
    -- when b $ error "Couldn't open a1"
    return gs

  let db = gs ^. #program

  context "getAddressSpaces" $ do
    spaces <- runIO . runGhidraOrError $ getAddressSpaceMap db
    let names = view (_2 . #name) <$> HashMap.toList spaces
        expectedNames =
          [ BA.EXTERNAL
          , BA.HASH
          , BA.Const
          , BA.Ram
          , BA.Register
          , BA.Stack
          , BA.Unique
          , BA.Other ".comment"
          , BA.Other ".shstrtab"
          , BA.Other ".strtab"
          , BA.Other ".symtab"
          , BA.Other "OTHER"
          , BA.Other "_elfSectionHeaders"
          ]

    it "should get all address spaces" $ do
      sort names `shouldBe` sort expectedNames

  context "getSegment" $ do
    seg <- runIO . runGhidraOrError $ State.getSegmentBlockFromAddress gs 0x010403c

    it "should get proper segment" $ do
      seg `shouldBe` Just ".bss"

  context "getAddressBase" $ do
    baseAddr <- runIO . runGhidraOrError $ State.getImageBase gs >>= mkAddress
    
    it "should get proper base address" $ do
      baseAddr ^. #offset `shouldBe` 0x0100000
