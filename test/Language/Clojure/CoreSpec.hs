module Language.Clojure.CoreSpec where

import Ghidra.Prelude

import Ghidra.Core
import Language.Clojure.Core
import qualified Language.Java as Java
import Test.Hspec


spec :: Spec
spec = describe "Language.Clojure.Core" $ do
  context "plus" $ do
    r :: Int64 <- runIO . runGhidra $ do
      a :: JLong <- Java.new (11 :: Int64)
      b :: JLong <- Java.new (22 :: Int64)
      c :: JLong <- Java.new (33 :: Int64)
      r <- fmap coerce . plus $ coerce <$> [a, b, c]
      Java.reify r
    it "should add three numbers" $ do
      r `shouldBe` 66

    r' :: Int64 <- runIO . runGhidra $ do
      a :: JLong <- Java.new (11 :: Int64)
      b :: JLong <- Java.new (22 :: Int64)
      c :: JLong <- Java.new (33 :: Int64)
      d :: JLong <- Java.new (44 :: Int64)
      r' <- fmap coerce . plus $ coerce <$> [a, b, c, d]
      Java.reify r'
    it "should add four numbers" $ do
      r' `shouldBe` 110
