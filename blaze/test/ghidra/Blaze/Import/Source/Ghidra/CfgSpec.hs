{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{- HLINT ignore "Redundant do" -}

module Blaze.Import.Source.Ghidra.CfgSpec where

import Blaze.Prelude hiding (Symbol)

import Blaze.Import.CallGraph (CallGraphImporter (getFunctions))
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Cfg as Cfg
import Blaze.Types.Cfg (CfNode)
import qualified Blaze.Import.Source.Ghidra as G
import Blaze.Import.Source.Ghidra.Cfg (getRawPcodeCfg, getHighPcodeCfg, getPilCfgFromRawPcode, getPilCfgFromHighPcode)

import Data.HashSet as HashSet
import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.gzf"

spec :: Spec
spec = describe "Blaze.Import.Source.Ghidra.Cfg" $ do
  importer <- runIO $ G.getImporter diveBin
  let gs = importer ^. #ghidraState
  funcs <- runIO $ getFunctions importer

  context "getRawPcodeCfg" $ do
    cfgs <- runIO $ traverse (\func -> getRawPcodeCfg gs func 0) funcs
    let f (Left _) = 0
        f (Right cfg) = HashSet.size . Cfg.nodes $ cfg
        nodeCounts = f <$> cfgs
        expected = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,5,5,5,5,5,6,6,6,6,6,6,7,7,7,7,8,9,9,9,9,10,10,10,11,11,11,14,14,15,16,16,17,21,32,34,36,37,40,41,117,117]

    it "should import Cfgs for all functions without crashing" $ do
      sort nodeCounts `shouldBe` expected

  context "getHighPcodeCfg" $ do
    cfgs <- runIO $ traverse (\func -> getHighPcodeCfg gs func 0) funcs
    let f (Left _) = 0
        f (Right cfg) = HashSet.size . Cfg.nodes $ cfg
        nodeCounts = f <$> cfgs
        expected = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,6,6,6,6,6,6,6,6,6,7,7,8,8,8,8,9,9,9,9,11,11,11,12,13,15,16,24,28,28,30,31,31,98,98]

    it "should import Cfgs for all functions without crashing" $ do
      sort nodeCounts `shouldBe` expected

  context "getPilCfgFromRawPcode" $ do
    cfgs :: [Maybe (Cfg.Cfg (CfNode [(Address, Pil.Stmt)]))] <-
      runIO $ traverse
        (\func -> view #result <<$>> getPilCfgFromRawPcode gs func 0)
        funcs

    let f = maybe 0 (HashSet.size . Cfg.nodes)
        nodeCounts = f <$> cfgs
        expected = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,3,3,3,3,3,4,4,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,8,8,8,8,9,9,9,9,9,10,10,10,10,11,11,11,12,12,12,13,15,16,16,17,17,18,20,21,22,23,25,27,28,29,34,36,39,41,41,54,58,59,145,187]

    it "should import Cfgs for all functions without crashing" $ do
      sort nodeCounts `shouldBe` expected

  context "getPilCfgFromHighPcode" $ do
    cfgs :: [Maybe (Cfg.Cfg (CfNode [(Address, Pil.Stmt)]))] <-
      runIO $ traverse
        (\func -> view #result <<$>> getPilCfgFromHighPcode gs func 0)
        funcs

    let f = maybe 0 (HashSet.size . Cfg.nodes)
        nodeCounts = f <$> cfgs
        expected = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,3,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,7,7,7,7,7,7,7,8,8,8,9,9,9,9,9,9,10,10,10,10,10,12,14,15,15,16,16,17,17,21,22,24,24,27,27,28,28,34,34,35,46,47,49,123,160]

    it "should import Cfgs for all functions without crashing" $ do
      sort nodeCounts `shouldBe` expected
