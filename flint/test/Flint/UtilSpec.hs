module Flint.UtilSpec where

import Flint.Prelude

import Flint.Util (offsetUUID)
import qualified Data.UUID as UUID
import Test.Hspec


spec :: Spec
spec = describe "Flint.Util" $ do
  describe "offsetUUID" $ do
    let uuid = UUID.fromWords 0x12345678 0x9abcdef0 0x11111111 0x22222222

    it "offset 0 returns the same UUID" $ do
      offsetUUID 0 uuid `shouldBe` uuid

    it "same offset always produces the same result" $ do
      offsetUUID 42 uuid `shouldBe` offsetUUID 42 uuid

    it "different offsets produce different UUIDs" $ do
      offsetUUID 1 uuid `shouldNotBe` offsetUUID 2 uuid

    it "offset result differs from original" $ do
      offsetUUID 1 uuid `shouldNotBe` uuid
