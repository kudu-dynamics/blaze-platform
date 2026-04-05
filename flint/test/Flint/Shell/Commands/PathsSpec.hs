{- HLINT ignore "Evaluate" -}

module Flint.Shell.Commands.PathsSpec where

import Flint.Prelude hiding (const, sym)

import Flint.Shell.Commands.Paths (parseDepthArg, parseContextDepthArg)

import Test.Hspec


spec :: Spec
spec = describe "Flint.Shell.Commands.Paths" $ do
  describe "parseContextDepthArg" $ do
    it "returns 0 with empty args" $
      parseContextDepthArg [] `shouldBe` (0, [])

    it "parses --context-depth N" $
      parseContextDepthArg ["--context-depth", "3"]
        `shouldBe` (3, [])

    it "preserves other args" $
      parseContextDepthArg ["foo", "--context-depth", "2", "bar"]
        `shouldBe` (2, ["foo", "bar"])

    it "ignores missing value" $
      parseContextDepthArg ["--context-depth"]
        `shouldBe` (0, ["--context-depth"])

    it "ignores non-numeric value" $
      parseContextDepthArg ["--context-depth", "abc"]
        `shouldBe` (0, ["--context-depth", "abc"])

  describe "parseDepthArg" $ do
    it "returns 0 with empty args" $
      parseDepthArg [] `shouldBe` (0, [])

    it "parses --depth N" $
      parseDepthArg ["--depth", "5"]
        `shouldBe` (5, [])

    it "preserves other args" $
      parseDepthArg ["func", "--depth", "1", "@", "0x1000"]
        `shouldBe` (1, ["func", "@", "0x1000"])

  describe "combined parsing" $ do
    it "parses both --depth and --context-depth together" $
      let args = ["func", "--depth", "2", "--context-depth", "1"]
          (depth, rest1) = parseDepthArg args
          (ctxDepth, rest2) = parseContextDepthArg rest1
      in do
        depth `shouldBe` 2
        ctxDepth `shouldBe` 1
        rest2 `shouldBe` ["func"]
