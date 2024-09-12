module Blaze.Import.Source.BinaryNinja (
  module Blaze.Import.Source.BinaryNinja,
  module Exports,
) where

import Binja.Core (BNBinaryView)
import qualified Binja.Core as Bn
import qualified Binja.Function as BnFunc
import qualified Binja.MLIL as Mlil
import Binja.View as BnView
import Blaze.Import.Binary (BinaryImporter (..))
import Blaze.Import.CallGraph (CallGraphImporter (getCallSites, getFunction, getFunctions))
import Blaze.Import.Cfg (CfgImporter (..))
import Blaze.Import.Pil (PilImporter (..))
import qualified Blaze.Import.Source.BinaryNinja.CallGraph as CallGraph
import qualified Blaze.Import.Source.BinaryNinja.Cfg as Cfg
import qualified Blaze.Import.Source.BinaryNinja.Pil as PilImp
-- import qualified Blaze.Import.Source.BinaryNinja.Pil.Path as Path
import Blaze.Import.Source.BinaryNinja.Types as Exports
import qualified Blaze.Pil as Pil
import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Cfg (PilNode)

newtype BNImporter = BNImporter
  { binaryView :: BNBinaryView
  }
  deriving (Eq, Ord, Show, Generic)

getImporter :: FilePath -> IO BNImporter
getImporter fp = Bn.getBinaryView fp >>= \case
  Left err -> error $ "Could not open binary: " <> cs err
  Right bv -> do
    Bn.updateAnalysisAndWait bv
    return $ BNImporter bv

{- |This type is used to provide an alternative instance of the
 'CfgImporter'.
-}
newtype BNImporterAlt = BNImporterAlt
  { bnImporter :: BNImporter
  }
  deriving (Eq, Ord, Show, Generic)

instance BinaryImporter BNImporter where
  openBinary fp = Bn.getBinaryView fp >>= \case
    Left err -> return $ Left err
    Right bv -> do
      Bn.updateAnalysisAndWait bv
      return . Right $ BNImporter bv

  shutdown = Bn.shutdown

  saveToDb fp (BNImporter bv) = do
    let fp' = fp <> if ".bndb" `isSuffixOf` fp then "" else ".bndb"
    Bn.saveBndb bv fp' >>= \case
      False -> return . Left $ "Failed to save bndb to " <> show fp'
      True -> return $ Right fp'

  rebaseBinary (BNImporter bv) off = do
    BnView.rebase bv (fromIntegral off) >>= \case
      Left t -> error . cs $ "Rebase failed: " <> t
      Right bv' -> do
        Bn.updateAnalysisAndWait bv'
        return $ BNImporter bv'

  getStart (BNImporter bv) = Bn.getStartOffset bv

  getEnd (BNImporter bv) = Bn.getEndOffset bv

  getOriginalBinaryPath (BNImporter bv) = BnView.getOriginalFileName bv

instance CallGraphImporter BNImporter where
  getFunction imp = CallGraph.getFunction (imp ^. #binaryView)

  getFunctions = CallGraph.getFunctions . view #binaryView

  getCallSites imp = CallGraph.getCallSites (imp ^. #binaryView)

instance CfgImporter BNImporterAlt where
  type NodeDataType BNImporterAlt = MlilSsaCfNode
  type NodeMapType BNImporterAlt = MlilNodeRefMap
  getCfg imp = Cfg.getCfgAlt (imp ^. #bnImporter . #binaryView)

instance CfgImporter BNImporter where
  type NodeDataType BNImporter = PilNode
  type NodeMapType BNImporter = PilMlilNodeMap
  getCfg imp = Cfg.getCfg imp (imp ^. #binaryView)

instance PilImporter BNImporter where
  type IndexType BNImporter = MlilSsaInstructionIndex
  getFuncStatements imp =
    PilImp.getFuncStatements (imp ^. #binaryView)

  getMappedStatements imp func ctxId =
    fmap (first snd) <$> PilImp.getFuncStatementsMapped (imp ^. #binaryView) func ctxId

  getCodeRefStatements imp ctxIndex' codeRef = do
    let fn = codeRef ^. #function
        funcAddr = fn ^. #address
    mBnFunc <- BnFunc.getFunctionStartingAt bv Nothing funcAddr
    case mBnFunc of
      Nothing -> error $ "No function found at " <> show funcAddr
      Just bnFunc -> do
        mlilSsaFunc <- BnFunc.getMLILSSAFunction bnFunc
        let start = fromIntegral $ codeRef ^. #startIndex
            end = fromIntegral $ codeRef ^. #endIndex
        nodeInstrs <- traverse (Mlil.instruction mlilSsaFunc . Bn.InstructionIndex) [start .. end]
        addrWidth <- Bn.getViewAddressSize bv
        let convSt =
              PilImp.mkConverterState
                bv
                ctxIndex'
                Pil.knownFuncDefs
                addrWidth
                fn
        PilImp.convert convSt $ PilImp.convertInstrsSplitPhi nodeInstrs
   where
    bv :: BNBinaryView
    bv = imp ^. #binaryView
