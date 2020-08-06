{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Binja.Header.ParseEnumsSpec where

import Binja.Header.Prelude

import Binja.Header.ParseEnums
import Text.RawString.QQ
import Data.Attoparsec.Text ( parseOnly )
import Test.Hspec


spec :: Spec
spec = describe "Binja.Header.ParseEnumsSpec" $ do
  context "parseEnumType" $ do
    it "simple enum" $ do
      let t = [r|enum BNSaveOption
	{
		RemoveUndoData,
		TrimSnapshots
	};|]
          
          result = Right . EnumType "BNSaveOption" . SimpleEnumVals $
            [ "RemoveUndoData"
            , "TrimSnapshots"
            ]
      parseOnly parseEnumType t `shouldBe` result

    it "specific enum" $ do
      let t = [r|enum BNSaveOption
	{
		RemoveUndoData = 34,
		TrimSnapshots = 88
	};|]
          
          result = Right . EnumType "BNSaveOption" . SpecificEnumVals $
            [ ("RemoveUndoData", 34)
            , ("TrimSnapshots", 88)
            ]
      parseOnly parseEnumType t `shouldBe` result

    it "simple enum with trailing comma in last field" $ do
      let t = [r|enum BNSaveOption
	{
		RemoveUndoData,
		TrimSnapshots,
	};|]
          
          result = Right . EnumType "BNSaveOption" . SimpleEnumVals $
            [ "RemoveUndoData"
            , "TrimSnapshots"
            ]
      parseOnly parseEnumType t `shouldBe` result

    it "specific enum with trailing comma in last field" $ do
      let t = [r|enum BNSaveOption
	{
		RemoveUndoData = 34,
		TrimSnapshots = 88,
	};|]
          
          result = Right . EnumType "BNSaveOption" . SpecificEnumVals $
            [ ("RemoveUndoData", 34)
            , ("TrimSnapshots", 88)
            ]
      parseOnly parseEnumType t `shouldBe` result

