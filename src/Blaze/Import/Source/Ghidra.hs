module Blaze.Import.Source.Ghidra (
  module Blaze.Import.Source.Ghidra,
  module Exports,
) where

import Ghidra.State (GhidraState)
import Ghidra.Core (runGhidraOrError)
import qualified Ghidra.State as GState
import Blaze.Import.CallGraph (CallGraphImporter (getCallSites, getFunction, getFunctions))
import Blaze.Import.Cfg (CfgImporter (..))
import Blaze.Import.Pil (PilImporter (..))
import qualified Blaze.Import.Source.Ghidra.CallGraph as CallGraph
import qualified Blaze.Import.Source.Ghidra.Cfg as Cfg
import qualified Blaze.Import.Source.Ghidra.Pil as PilImp
-- import qualified Blaze.Import.Source.Ghidra.Pil.Path as Path
import Blaze.Import.Source.Ghidra.Types as Exports
import Ghidra.Types.Variable (VarNode)
import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Cfg (PilNode)
import Text.Pretty.Simple (pHPrint)

newtype GhidraImporter = GhidraImporter
  { ghidraState :: GhidraState
  }
  deriving (Eq, Ord, Show, Generic)

getImporter :: FilePath -> IO GhidraImporter
getImporter fp = runGhidraOrError $ GState.openDatabase fp >>= \case
  Left err -> error $ "Could not open binary: " <> show err
  Right gs -> do
    GState.analyze gs
    return $ GhidraImporter gs

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
