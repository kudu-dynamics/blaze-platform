{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blaze.Import.Source.Ghidra (
  module Blaze.Import.Source.Ghidra,
  module Exports,
) where

import Blaze.Import.Binary (BinaryImporter (..))
import Blaze.Import.CallGraph (CallGraphImporter (getCallSites, getFunction, getFunctions))
import Blaze.Import.Cfg (CfgImporter (..))
import Blaze.Import.Pil (PilImporter (..))
import Blaze.Import.Xref (XrefImporter (..))
import Blaze.Import.Source.Ghidra.CallGraph qualified as CallGraph
import Blaze.Import.Source.Ghidra.Cfg qualified as Cfg
import Blaze.Import.Source.Ghidra.Pil qualified as PilImp
import Blaze.Import.Source.Ghidra.Xref qualified as Xref
import Ghidra.Core (runGhidraOrError, stopJVMIfRunning)
import Ghidra.Program qualified as GProg
import Ghidra.State qualified as GState
import Ghidra.Address qualified as GAddr
import qualified Ghidra.Inspect as GInspect
import qualified Ghidra.Symbol as GSym

import Blaze.Import.Source.Ghidra.Types as Exports
import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Cfg (PilNode)
import Blaze.Types.CachedCalc qualified as CC
import Ghidra.Types.Variable (VarNode)
import Text.Pretty.Simple (pHPrint)
import qualified Data.HashMap.Strict as HashMap

-- | Filter out bogus string entries from low addresses (e.g. ELF header data
-- that Ghidra's hasStringValue() incorrectly reports as strings).
filterStringsMap :: HashMap Int64 Text -> HashMap Address Text
filterStringsMap = HashMap.mapKeys intToAddr . HashMap.filterWithKey (\k _ -> k >= 0x100)

getImporter :: FilePath -> IO GhidraImporter
getImporter fp = do
  highFnCalc <- atomically CC.create
  runGhidraOrError $
    GState.openDatabase fp >>= \case
      Left err -> error $ "Could not open binary: " <> show err
      Right gs -> do
        GState.analyze gs
        smap <- GState.getDefinedStrings (gs ^. #program)
        return $ GhidraImporter gs highFnCalc (filterStringsMap smap)

instance BinaryImporter GhidraImporter where
  openBinary fp = do
    highFnCalc <- atomically CC.create
    runGhidraOrError $
      GState.openDatabase fp >>= \case
        Left err -> return . error $ "Could not open binary: " <> show err
        Right gs -> do
          GState.analyze gs
          smap <- GState.getDefinedStrings (gs ^. #program)
          return . Right $ GhidraImporter gs highFnCalc (filterStringsMap smap)

  shutdown = stopJVMIfRunning

  saveToDb fp imp = do
    let fp' = fp <> if ".gzf" `isSuffixOf` fp then "" else ".gzf"
        gs = imp ^. #ghidraState
    runGhidraOrError $ GState.saveDatabase gs fp'
    return $ Right fp'

  -- uh, won't this invalidate the cache??
  rebaseBinary imp off = do
    let gs = imp ^. #ghidraState
    runGhidraOrError $ do
      GProg.withTransaction (gs ^. #program) "BinaryImporter: Set Image Base" $ do
        GProg.setImageBase (gs ^. #program) (addrToInt off) True
        GState.analyze gs
      smap <- GState.getDefinedStrings (gs ^. #program)
      return $ imp { stringsMap = filterStringsMap smap }

  getBase imp = do
    let gs = imp ^. #ghidraState
    runGhidraOrError $ do
      prg <- GState.getProgram gs
      fmap convertAddress $ GState.getImageBase prg >>= GAddr.mkAddress

  getStart imp = fmap convertAddress . runGhidraOrError $ GProg.getMinAddress (gs ^. #program)
    where gs = imp ^. #ghidraState

  getEnd imp = fmap convertAddress . runGhidraOrError $ GProg.getMaxAddress (gs ^. #program)
    where gs = imp ^. #ghidraState

  getOriginalBinaryPath imp = do
    let gs = imp ^. #ghidraState
    binPath <- runGhidraOrError $ GProg.getExecutablePath (gs ^. #program)
    return $ cs binPath

  getStringsMap = return . view #stringsMap

  inspectAddress imp addr =
    runGhidraOrError $ GInspect.inspectAddress (imp ^. #ghidraState) addr

  lookupGlobalSymbol imp name = do
    let prg = imp ^. #ghidraState . #program
    mAddr <- runGhidraOrError $ GSym.lookupGlobalAddress prg name
    case mAddr of
      Nothing -> return Nothing
      Just jAddr -> Just . convertAddress <$> runGhidraOrError (GAddr.mkAddress jAddr)

instance CallGraphImporter GhidraImporter where
  getFunction = CallGraph.getFunction

  getFunctions = CallGraph.getFunctions

  getCallSites = CallGraph.getCallSites

instance CfgImporter GhidraImporter where
  type NodeDataType GhidraImporter = PilNode
  type NodeMapType GhidraImporter = PilPcodeMap VarNode
  getCfg = Cfg.getPilCfgFromHighPcode
  getCfgWithTypeHints = Cfg.getPilCfgAndTypeHintsFromHighPcode

instance PilImporter GhidraImporter where
  type IndexType GhidraImporter = Address
  getFuncStatements imp f ctx = do
    (errors, stmts) <- partitionEithers <$> PilImp.getFuncStatementsFromHighPcode imp f ctx
    unless (null errors) $ do
      hPutStrLn @String stderr "Errors during conversion:"
      traverse_ (pHPrint stderr) errors
    pure . fmap (view #stmt) $ stmts

  getMappedStatements imp f ctx = do
    (errors, stmts) <- partitionEithers <$> PilImp.getFuncStatementsFromHighPcode imp f ctx
    unless (null errors) $ do
      hPutStrLn @String stderr "Errors during conversion:"
      traverse_ (pHPrint stderr) errors
    pure stmts

  getCodeRefStatements imp ctx ref = do
    (errors, stmts) <- partitionEithers <$> PilImp.getCodeRefStatementsFromHighPcode imp ctx ref
    unless (null errors) $ do
      putStrLn @String "Errors during conversion:"
      traverse_ pprint errors
    pure . fmap (view #stmt) $ stmts


  getFuncStatementsWithTypeHints imp f ctx = do
    (stmtsWithErr, typeHints) <- PilImp.getFuncStatementsWithTypeHintsFromHighPcode imp f ctx
    let (errors, stmts) = partitionEithers stmtsWithErr
    unless (null errors) $ do
      hPutStrLn @String stderr "Errors during conversion:"
      traverse_ (pHPrint stderr) errors
    pure (view #stmt <$> stmts, typeHints)

  getMappedStatementsWithTypeHints imp f ctx = do
    (stmtsWithErr, typeHints) <- PilImp.getFuncStatementsWithTypeHintsFromHighPcode imp f ctx
    let (errors, stmts) = partitionEithers stmtsWithErr
    unless (null errors) $ do
      hPutStrLn @String stderr "Errors during conversion:"
      traverse_ (pHPrint stderr) errors
    pure (stmts, typeHints)

instance XrefImporter GhidraImporter where
  getXrefsTo = Xref.getXrefsTo

