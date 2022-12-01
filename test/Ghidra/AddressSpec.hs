module Ghidra.AddressSpec where

import Ghidra.Prelude

import qualified Ghidra.State as State
import Ghidra.Address
import qualified Ghidra.Types.Address as Addr
import Ghidra.Core
import Test.Hspec
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

  context "getAddressSpaces" $ do
    spaces <- runIO . runGhidraOrError $ getAddressSpaceMap gs
    let names = view (_2 . #name) <$> HashMap.toList spaces
        expectedNames =
          [ Addr.EXTERNAL
          , Addr.HASH
          , Addr.Const
          , Addr.Ram
          , Addr.Register
          , Addr.Stack
          , Addr.Unique
          , Addr.Other ".comment"
          , Addr.Other ".shstrtab"
          , Addr.Other ".strtab"
          , Addr.Other ".symtab"
          , Addr.Other "OTHER"
          , Addr.Other "_elfSectionHeaders"
          ]

    it "should get all address spaces" $ do
      sort names `shouldBe` sort expectedNames
