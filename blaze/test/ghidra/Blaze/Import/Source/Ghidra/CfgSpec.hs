{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{- HLINT ignore "Redundant do" -}

module Blaze.Import.Source.Ghidra.CfgSpec where

import Blaze.Prelude hiding (Symbol)

import Blaze.Import.CallGraph (CallGraphImporter (getFunctions, getFunction))
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Cfg as Cfg
import Blaze.Types.Cfg (CfNode(..))
import qualified Blaze.Import.Source.Ghidra as G
import Blaze.Import.Source.Ghidra.Cfg (getRawPcodeCfg, getHighPcodeCfg, getPilCfgFromRawPcode, getPilCfgFromHighPcode)

import qualified Data.HashSet as HashSet
import qualified Data.List as List
import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.gzf"

-- | Get (start, end) ranges for all BasicBlock nodes in a CFG.
bbRanges :: (Hashable a) => Cfg.Cfg (CfNode a) -> [(Address, Address)]
bbRanges cfg =
  [ (bb ^. #start, bb ^. #end)
  | BasicBlock bb <- HashSet.toList . Cfg.nodes $ cfg
  ]

-- | Check that an address is covered by some node in the CFG.
-- BasicBlocks use range check, Call nodes use the Cfg.nodeContainsAddress check.
addrCoveredByNode :: (Hashable a) => Cfg.Cfg (CfNode a) -> Address -> Bool
addrCoveredByNode cfg addr =
  any (Cfg.nodeContainsAddress addr) . HashSet.toList . Cfg.nodes $ cfg

-- | Get all statement addresses from a raw P-code CFG's BasicBlock nodes.
rawBBStmtAddrs :: (Hashable a) => Cfg.Cfg (CfNode [(Address, a)]) -> [Address]
rawBBStmtAddrs cfg =
  [ addr
  | BasicBlock bb <- HashSet.toList . Cfg.nodes $ cfg
  , (addr, _) <- bb ^. #nodeData
  ]

spec :: Spec
spec = describe "Blaze.Import.Source.Ghidra.Cfg" $ do
  context "Getting CFGs from DiveLogger" $ do
    imp <- runIO $ G.getImporter diveBin
    allFuncRefs <- runIO $ getFunctions imp
    let internalRefs = mapMaybe (^? #_InternalRef) allFuncRefs
    funcs <- runIO $ mapMaybeM (\fm -> fmap (>>= (^? #_Internal)) . getFunction imp $ fm ^. #address) internalRefs

    context "getRawPcodeCfg" $ do
      cfgs <- fmap catMaybes . runIO $ traverse (\func -> getRawPcodeCfg (imp ^. #ghidraState) func 0) funcs
      let f = HashSet.size . Cfg.nodes
          nodeCounts = f <$> cfgs
          expected = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,5,5,5,5,5,6,6,6,6,6,6,7,7,7,7,8,9,9,9,9,10,10,10,11,11,11,14,14,15,16,16,17,21,32,34,36,37,40,41,117,117]

      it "should import Cfgs for all functions without crashing" $ do
        sort nodeCounts `shouldBe` expected

    context "getHighPcodeCfg" $ do
      cfgs <- fmap catMaybes . runIO $ traverse (\func -> getHighPcodeCfg imp func 0) funcs
      let f = HashSet.size . Cfg.nodes
          nodeCounts = f <$> cfgs
          expected = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,6,6,6,6,6,6,6,6,6,7,7,8,8,8,8,9,9,9,9,11,11,11,12,13,15,16,21,28,29,29,30,31,98,98]

      it "should import Cfgs for all functions without crashing" $ do
        sort nodeCounts `shouldBe` expected

    context "getPilCfgFromRawPcode" $ do
      cfgs :: [Maybe (Cfg.Cfg (CfNode [Pil.Stmt]))] <- runIO $
        traverse (\func -> view #result <<$>> getPilCfgFromRawPcode imp func 0) funcs

      let f = maybe 0 (HashSet.size . Cfg.nodes)
          nodeCounts = f <$> cfgs
          expected = [1,1,1,1,1,1,3,3,3,3,3,4,4,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,8,8,8,8,9,9,9,9,9,10,10,10,10,11,11,11,12,12,12,13,15,16,16,17,17,18,20,21,22,23,25,27,28,29,34,36,39,41,41,54,58,59,145,187]

      it "should import Cfgs for all functions without crashing" $ do
        sort nodeCounts `shouldBe` expected

    context "getPilCfgFromHighPcode" $ do
      cfgs :: [Maybe (Cfg.Cfg (CfNode [Pil.Stmt]))] <- runIO $
        traverse (\func -> view #result <<$>> getPilCfgFromHighPcode imp func 0) funcs

      let f = maybe 0 (HashSet.size . Cfg.nodes)
          nodeCounts = f <$> cfgs
          expected = [1,1,1,1,1,1,1,2,2,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,6,6,6,6,6,6,7,7,7,7,7,7,7,8,8,8,8,9,9,9,9,9,10,10,10,10,10,11,14,15,15,16,16,16,17,21,22,24,24,25,27,27,28,33,34,34,42,44,47,123,160]

      it "should import Cfgs for all functions without crashing" $ do
        sort nodeCounts `shouldBe` expected

    context "BasicBlock address ranges in PIL CFG" $ do
      let removeDive = List.find (\f -> f ^. #name == "cgc_RemoveDive") funcs

      it "BasicBlock ranges should cover xref addresses folded into calls in cgc_RemoveDive" $ do
        let func = fromJust removeDive
            addrSpace = func ^. #address . #space
            mkAddr n = intToAddr n & #space .~ addrSpace
        pilCfg <- view #result . fromJust <$> getPilCfgFromHighPcode imp func 0
        let ranges = bbRanges pilCfg
        -- 0x804d509 and 0x804d518: LEA instructions loading the string argument
        -- for cgc_SelectDive, folded into the CALL by the decompiler
        length ranges `shouldSatisfy` (> 0)
        addrCoveredByNode pilCfg (mkAddr 0x804d509) `shouldBe` True
        addrCoveredByNode pilCfg (mkAddr 0x804d518) `shouldBe` True

