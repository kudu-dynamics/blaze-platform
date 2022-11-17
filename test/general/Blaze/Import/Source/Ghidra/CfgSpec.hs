{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{- HLINT ignore "Redundant do" -}

module Blaze.Import.Source.Ghidra.CfgSpec where

import qualified Ghidra.Function as GFunc
import Blaze.Function (
  Access (Unknown),
  FuncParamInfo (FuncParamInfo),
  Function (Function),
  ParamInfo (ParamInfo),
 )
import Blaze.Import.CallGraph (CallGraphImporter (getCallSites, getFunctions))
import Blaze.Import.Cfg (CfgImporter (getCfg))
import Blaze.Prelude hiding (Symbol)
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Cfg as Cfg
import Blaze.Types.Cfg (CfNode, CfEdge)
import qualified Blaze.Import.Source.Ghidra as G
import Blaze.Import.Source.Ghidra.Cfg
import Blaze.Import.Source.BinaryNinja.Cfg (convertNode, runNodeConverter)
import Blaze.Import.Source.BinaryNinja.Types (MlilSsaInstructionIndex)
import Blaze.Types.Import (ImportResult(ImportResult))
import Control.Arrow ((&&&))
import Data.HashMap.Strict as HMap
import Data.HashSet as HashSet
import Blaze.Import.Source.Ghidra.Cfg (getRawPcodeCfg, getHighPcodeCfg, getPilCfgFromRawPcode, getPilCfgFromHighPcode)
import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger"

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
    cfgs :: [(Cfg.Cfg (CfNode [(Address, Pil.Stmt)]))] <-
      runIO $ traverse
        (\func -> view #result . fromJust <$> getPilCfgFromRawPcode gs func 0)
        funcs
    
    let f cfg = HashSet.size . Cfg.nodes $ cfg
        nodeCounts = f <$> cfgs
        expected = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,5,5,5,5,5,6,6,6,6,6,6,7,7,7,7,8,9,9,9,9,10,10,10,11,11,11,14,14,15,16,16,17,21,32,34,36,37,40,41,117,117]

    it "should import Cfgs for all functions without crashing" $ do
      sort nodeCounts `shouldBe` expected
