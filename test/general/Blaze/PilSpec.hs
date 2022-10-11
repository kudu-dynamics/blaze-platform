{- HLINT ignore "Redundant do" -}

module Blaze.PilSpec where

-- import Binja.BasicBlock (
--   BasicBlock,
--   BlockEdge,
--  )
-- import qualified Binja.BasicBlock as BB
-- import qualified Binja.Core as BN
-- import qualified Binja.Function as BNFunc
-- import qualified Binja.MLIL as MLIL
-- import qualified Blaze.Graph as Graph
-- import Blaze.Types.Path.AlgaPath (AlgaPath)
-- import qualified Blaze.Path as Path
-- import Blaze.Import.Source.BinaryNinja.Pil (convertToPilVar)
-- import qualified Blaze.Pil as Pil
-- import qualified Blaze.Import.Source.BinaryNinja.Pil as Pil
-- -- import Blaze.Import.Source.BinaryNinja.Pil.Path (convertPath)
-- import qualified Blaze.Import.Source.BinaryNinja.CallGraph as CG
import Blaze.Prelude
-- import Blaze.Types.Graph.Alga (AlgaGraph)
-- import Control.Lens (ix)
-- import Data.HashMap.Strict as HM
import Test.Hspec

-- type F = BNFunc.MLILSSAFunction

-- diveBin :: FilePath
-- diveBin = "res/test_bins/Dive_Logger/Dive_Logger.bndb"

-- diveBv :: IO BN.BNBinaryView
-- diveBv = do
--   (Right bv) <- BN.getBinaryView diveBin
--   BN.updateAnalysisAndWait bv
--   return bv

spec :: Spec
spec = describe "Blaze.Pil" $ do
  context "when avoiding" $ do
    it "placeholder" $
      True `shouldBe` True

--   bv <- runIO diveBv
--   addrWidth <- runIO $ BN.getViewAddressSize bv
--   sanitizeTime <- runIO $ fromJust <$> BNFunc.getFunctionStartingAt bv Nothing 0x0804de20
--   cgSanitizeTime <- runIO $ CG.convertFunction bv sanitizeTime
--   mlilSanitizeTime <- runIO $ BNFunc.getMLILSSAFunction sanitizeTime
--   bbgSanitizeTime <- runIO (Graph.constructBasicBlockGraph mlilSanitizeTime :: IO (AlgaGraph (BlockEdge F) (BasicBlock F)))
--   callBb <- runIO $ fromJust <$> BB.getBasicBlockForInstruction mlilSanitizeTime 90
--   callPath <- runIO (Path.pathFromBasicBlockList bv bbgSanitizeTime [callBb] :: IO AlgaPath)

--   context "convertToPilVar" $ do
--     mlilDef <- runIO $ MLIL.instruction mlilSanitizeTime 90
--     let mlilVar = mlilDef ^?! (MLIL.op . MLIL._SET_VAR_SSA . MLIL.dest)
--     (pilVar, converterState) <-
--       runIO $
--         Pil.runConverter
--           (convertToPilVar mlilVar)
--           (Pil.mkConverterState bv Pil.knownFuncDefs addrWidth cgSanitizeTime callPath)
--     it "stores a mapping of the new PilVar to the import source var" $ do
--       let savedVar = HM.lookup pilVar $ converterState ^. #sourceVars
--       Just mlilVar `shouldBe` savedVar ^? _Just . #var

--   context "convertCallInstruction" $ do
--     stmts <- runIO $ convertPath bv callPath

--     it "should generate store statements for output arguments" $ do
--       let defCallStmt = stmts !! 7
--           firstArg = defCallStmt ^?! #_Def . #value . #op . #_CALL . #params . ix 0
--           storeStmt = stmts !! 8
--           storeDest = storeStmt ^?! #_Store . #addr
--       firstArg `shouldBe` storeDest
