module Data.BinaryAnalysisSpec where

import Data.BinaryAnalysis
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Test.Hspec

mkAddr :: AddressSpaceName -> Int64 -> Address
mkAddr spaceName off = Address
  { space = AddressSpace
    { ptrSize = Bytes 8
    , addressableUnitSize = Bytes 1
    , name = spaceName
    }
  , offset = off
  }

testAddr :: AddressSpaceName -> Int64 -> String -> Expectation
testAddr spaceName off expected = show (mkAddr spaceName off) `shouldBe` expected

spec :: Spec
spec = do
  describe "AddressSpace show instances" $ do
    it "Ensure addresss in RAM AddressSpace pretty prints normally" $ do
      testAddr Ram 0x1234 "Address 0x1234"
    
    it "Ensure address with different AddressSpace pretty prints properly " $ do
      traverse_
        (\(spaceName, off, expected) -> testAddr spaceName off expected)
        [ (EXTERNAL, 0x1234, "Address EXTERNAL:0x1234")
        , (HASH, 0x5678, "Address HASH:0x5678")
        , (Const, 0x67, "Address Const:0x67")
        , (Register, 0x0, "Address Register:0x0")
        , (Stack, 0x69, "Address Stack:0x69")
        , (Unique, 0x7fffffff, "Address Unique:0x7fffffff")
        , (Other "test", 0x4000, "Address Other \"test\":0x4000")
        ]
