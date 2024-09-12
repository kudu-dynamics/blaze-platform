module Blaze.Import.Source.Ghidra (
  module Blaze.Import.Source.Ghidra,
  module Exports,
) where

import Blaze.Import.Binary (BinaryImporter (..))
import Blaze.Import.CallGraph (CallGraphImporter (getCallSites, getFunction, getFunctions))
import Blaze.Import.Cfg (CfgImporter (..))
import Blaze.Import.Pil (PilImporter (..))
import Blaze.Import.Source.Ghidra.CallGraph qualified as CallGraph
import Blaze.Import.Source.Ghidra.Cfg qualified as Cfg
import Blaze.Import.Source.Ghidra.Pil qualified as PilImp
import Ghidra.Core (runGhidraOrError, stopJVMIfRunning)
import Ghidra.Program qualified as GProg
import Ghidra.State (GhidraState)
import Ghidra.State qualified as GState

import Blaze.Import.Source.Ghidra.Types as Exports
import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Cfg (PilNode)
import Ghidra.Types.Variable (VarNode)
import Text.Pretty.Simple (pHPrint)

newtype GhidraImporter = GhidraImporter
  { ghidraState :: GhidraState
  }
  deriving (Eq, Ord, Show, Generic)

getImporter :: FilePath -> IO GhidraImporter
getImporter fp =
  runGhidraOrError $
    GState.openDatabase fp >>= \case
      Left err -> error $ "Could not open binary: " <> show err
      Right gs -> do
        GState.analyze gs
        return $ GhidraImporter gs

instance BinaryImporter GhidraImporter where
  openBinary fp =
    runGhidraOrError $
      GState.openDatabase fp >>= \case
        Left err -> return . error $ "Could not open binary: " <> show err
        Right gs -> do
          GState.analyze gs
          return . Right $ GhidraImporter gs

  shutdown = stopJVMIfRunning

  saveToDb fp (GhidraImporter gs) = do
    let fp' = fp <> if ".gzf" `isSuffixOf` fp then "" else ".gzf"
    runGhidraOrError $ GState.saveDatabase gs fp'
    return $ Right fp'

  rebaseBinary (GhidraImporter gs) off = runGhidraOrError $ do
    GProg.withTransaction (gs ^. #program) "BinaryImporter: Set Image Base" $ do
      GProg.setImageBase (gs ^. #program) (fromIntegral off) True
      GState.analyze gs
    return $ GhidraImporter gs

  getStart (GhidraImporter gs) = fmap convertAddress . runGhidraOrError $ GProg.getMinAddress (gs ^. #program)

  getEnd (GhidraImporter gs) = fmap convertAddress . runGhidraOrError $ GProg.getMaxAddress (gs ^. #program)

  getOriginalBinaryPath (GhidraImporter gs) = do
    binPath <- runGhidraOrError $ GProg.getExecutablePath (gs ^. #program)
    return $ cs binPath

instance CallGraphImporter GhidraImporter where
  getFunction imp = CallGraph.getFunction (imp ^. #ghidraState)

  getFunctions = CallGraph.getFunctions . view #ghidraState

  getCallSites imp = CallGraph.getCallSites (imp ^. #ghidraState)

instance CfgImporter GhidraImporter where
  type NodeDataType GhidraImporter = PilNode
  type NodeMapType GhidraImporter = PilPcodeMap VarNode
  getCfg imp func ctxId = fmap (fmap Cfg.removeStmtAddrs) <$> Cfg.getPilCfgFromHighPcode (imp ^. #ghidraState) func ctxId

instance PilImporter GhidraImporter where
  type IndexType GhidraImporter = Address
  getFuncStatements imp f ctx = do
    (errors, stmts) <- partitionEithers <$> PilImp.getFuncStatementsFromHighPcode st f ctx
    unless (null errors) $ do
      hPutStrLn @String stderr "Errors during conversion:"
      traverse_ (pHPrint stderr) errors
    pure . fmap (view #stmt) $ stmts
    where
      st = imp ^. #ghidraState

  getMappedStatements imp f ctx = do
    (errors, stmts) <- partitionEithers <$> PilImp.getFuncStatementsFromHighPcode st f ctx
    unless (null errors) $ do
      hPutStrLn @String stderr "Errors during conversion:"
      traverse_ (pHPrint stderr) errors
    pure stmts
    where
      st = imp ^. #ghidraState

  getCodeRefStatements imp ctx ref = do
    (errors, stmts) <- partitionEithers <$> PilImp.getCodeRefStatementsFromHighPcode st ctx ref
    unless (null errors) $ do
      putStrLn @String "Errors during conversion:"
      traverse_ pprint errors
    pure . fmap (view #stmt) $ stmts
    where
      st = imp ^. #ghidraState
