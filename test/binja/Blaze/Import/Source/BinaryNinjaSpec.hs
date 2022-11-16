{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{- HLINT ignore "Redundant do" -}

module Blaze.Import.Source.BinaryNinjaSpec where

import qualified Binja.BasicBlock as BNBb
import qualified Binja.Core as BN
import qualified Binja.Function as BNFunc
import Blaze.Cfg (BranchType (FalseBranch, TrueBranch, UnconditionalBranch))
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
import Blaze.Import.Source.BinaryNinja (
  BNImporter (
    BNImporter
  ),
 )
import Blaze.Import.Source.BinaryNinja.Cfg (convertNode, runNodeConverter)
import Blaze.Import.Source.BinaryNinja.Types (MlilSsaInstructionIndex)
import Blaze.Types.Import (ImportResult(ImportResult))
import Control.Arrow ((&&&))
import Data.HashMap.Strict as HMap
import Data.HashSet as HashSet
import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.bndb"

findFunc :: Text -> [Function] -> Maybe Function
findFunc funcName = find ((== funcName) . (^. #name))

spec :: Spec
spec = describe "Blaze.Import.Source.BinaryNinja" $ do
  context "Importing call graphs" $ do
    bv <- unsafeFromRight <$> runIO (BN.getBinaryView diveBin)
    runIO $ BN.updateAnalysisAndWait bv
    let importer = BNImporter bv
    funcs <- runIO $ getFunctions importer
    it "should import all functions" $ do
      length funcs `shouldBe` 94
    let changeDiveFunc = fromJust $ findFunc "cgc_ChangeDive" funcs
    changeDiveCalls <- runIO $ getCallSites importer changeDiveFunc
    let printfFunc = fromJust $ findFunc "cgc_printf" funcs
    printfCalls <- runIO $ getCallSites importer printfFunc
    it "should import call sites" $ do
      length changeDiveCalls `shouldBe` 3
      length printfCalls `shouldBe` 35
  context "Importing CFGs" $ do
    bv <- unsafeFromRight <$> runIO (BN.getBinaryView diveBin)
    runIO $ BN.updateAnalysisAndWait bv
    let importer = BNImporter bv
        logNewDiveFunc =
          Function
            Nothing
            "cgc_LogNewDive"
            (Address 0x0804d1c0)
            [FuncParamInfo $ ParamInfo "arg1#0" Unknown]
    logNewDiveBnFunc <- runIO $ fromJust <$> BNFunc.getFunctionStartingAt bv Nothing (Address 0x0804d1c0)
    logNewDiveMlilSsaFunc <- runIO $ BNFunc.getMLILSSAFunction logNewDiveBnFunc
    mlilBbs <- runIO $ BNBb.getBasicBlocks logNewDiveMlilSsaFunc
    (ImportResult ctx mapping cfg) <- runIO $ fromJust <$> getCfg importer logNewDiveFunc 0
    (cfNodes, nodeMapEntries) <- runIO $ runNodeConverter $ convertNode ctx (head mlilBbs)

    it "should convert a BN basic block with calls into multiple nodes" $ do
      length cfNodes `shouldBe` 3

    it "should convert a BN CFG to a Blaze CFG" $ do
      (length . Cfg.nodes $ cfg) `shouldBe` 8
      (length . Cfg.edges $ cfg) `shouldBe` 8

    it "should build up a node mapping when converting nodes" $ do
      length nodeMapEntries `shouldBe` 3

    it "should provide a complete mapping with the CFG" $ do
      let codeRefs = HMap.elems mapping
      HMap.size mapping `shouldBe` 8
      HashSet.fromList ((fromIntegral . view #startIndex &&& fromIntegral . view #endIndex) <$> codeRefs)
        `shouldBe` (HashSet.fromList [(0, 1), (2, 2), (3, 4), (6, 7), (8, 8), (9, 11), (12, 12), (14, 18)] :: HashSet (Int, Int))

    it "should include correct edges" $ do
      let nodes :: [CfNode [Pil.Stmt]]
          nodes = HashSet.toList . Cfg.nodes $ cfg
          nodesByStart :: HashMap MlilSsaInstructionIndex (CfNode [Pil.Stmt])
          nodesByStart = HMap.fromList $ zip (view #startIndex . (mapping HMap.!) <$> nodes) nodes
          nodeAt = (nodesByStart HMap.!)
          edges :: [CfEdge (CfNode [Pil.Stmt])]
          edges = Cfg.edges cfg
      HashSet.fromList edges
        `shouldBe` HashSet.fromList
          [ Cfg.fromTupleEdge (UnconditionalBranch, (nodeAt 0, nodeAt 2))
          , Cfg.fromTupleEdge (UnconditionalBranch, (nodeAt 2, nodeAt 3))
          , Cfg.fromTupleEdge (TrueBranch, (nodeAt 3, nodeAt 14))
          , Cfg.fromTupleEdge (FalseBranch, (nodeAt 3, nodeAt 6))
          , Cfg.fromTupleEdge (UnconditionalBranch, (nodeAt 6, nodeAt 8))
          , Cfg.fromTupleEdge (UnconditionalBranch, (nodeAt 8, nodeAt 9))
          , Cfg.fromTupleEdge (UnconditionalBranch, (nodeAt 9, nodeAt 12))
          , Cfg.fromTupleEdge (UnconditionalBranch, (nodeAt 12, nodeAt 14))
          ]
