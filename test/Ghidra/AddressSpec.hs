module Ghidra.AddressSpec where

import Ghidra.Prelude

import qualified Ghidra.State as State
import qualified Ghidra.Function as Function
import Ghidra.Address
import Ghidra.Function (Function)
import Ghidra.Core
import qualified Language.Java as Java
import Language.Clojure.Core
import Test.Hspec
import qualified Data.HashMap.Strict as HashMap


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger"

a1Bin :: FilePath
a1Bin = "res/test_bins/a1/a1"

spec :: Spec
spec = describe "Ghidra.Address" $ do
  gs <- runIO . runGhidra $ do
    gs <- State.openDatabase a1Bin >>= State.analyze
    b <- isNil' $ gs ^. #unGhidraState
    when b $ error "Couldn't open a1"
    return gs
  
  context "getAddressSpaces" $ do
    spaces <- runIO . runGhidra $ getAddressSpaceMap gs
    let names = (view $ _2 . #name) <$> HashMap.toList spaces
        expectedNames =
          [ "unique"
          , "register"
          , "EXTERNAL"
          , "const"
          , "ram"
          , "stack"
          , ".comment"
          , "OTHER"
          , "_elfSectionHeaders"
          , ".shstrtab"
          , ".strtab"
          , ".symtab"
          , "HASH"
          ]

    it "should get all address spaces" $ do
      names `shouldBe` expectedNames
