module Blaze.Import.Source.Ghidra (
  module Blaze.Import.Source.Ghidra,
  module Exports,
) where

import Ghidra.State (GhidraState)
import Ghidra.Core (runGhidra)
import qualified Ghidra.State as GState
import Blaze.Import.CallGraph (CallGraphImporter (getCallSites, getFunction, getFunctions))
import Blaze.Import.Cfg (CfgImporter (..))
import Blaze.Import.Pil (PilImporter (..))
import qualified Blaze.Import.Source.Ghidra.CallGraph as CallGraph
import qualified Blaze.Import.Source.Ghidra.Cfg as Cfg
import qualified Blaze.Import.Source.Ghidra.Pil as PilImp
-- import qualified Blaze.Import.Source.Ghidra.Pil.Path as Path
import Blaze.Import.Source.Ghidra.Types as Exports
import qualified Blaze.Pil as Pil
import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Cfg (PilNode)

newtype GhidraImporter = GhidraImporter
  { ghidraState :: GhidraState
  }
  deriving (Eq, Ord, Show, Generic)

getImporter :: FilePath -> IO GhidraImporter
getImporter fp = runGhidra $ GState.openDatabase fp >>= \case
  Left err -> error $ "Could not open binary: " <> show err
  Right gs -> do
    GState.analyze gs
    return $ GhidraImporter gs
    

-- {- |This type is used to provide an alternative instance of the
--  'CfgImporter'.
-- -}
-- newtype GhidraImporterAlt = GhidraImporterAlt
--   { ghidraImporter :: GhidraImporter
--   }
--   deriving (Eq, Ord, Show, Generic)

instance CallGraphImporter GhidraImporter where
  getFunction imp = CallGraph.getFunction (imp ^. #ghidraState)

  getFunctions = CallGraph.getFunctions . view #ghidraState

  getCallSites imp = CallGraph.getCallSites (imp ^. #ghidraState)

-- instance CfgImporter GhidraImporterAlt where
--   type NodeDataType GhidraImporterAlt = MlilSsaCfNode
--   type NodeMapType GhidraImporterAlt = MlilNodeRefMap
--   getCfg imp = Cfg.getCfgAlt (imp ^. #bnImporter . #ghidraState)

-- instance CfgImporter GhidraImporter where
--   type NodeDataType GhidraImporter = PilNode
--   type NodeMapType GhidraImporter = PilMlilNodeMap
--   getCfg imp = Cfg.getCfg imp (imp ^. #ghidraState)

-- instance PilImporter GhidraImporter where
--   type IndexType GhidraImporter = MlilSsaInstructionIndex
--   getFuncStatements imp =
--     PilImp.getFuncStatements (imp ^. #ghidraState)

--   getCodeRefStatements imp ctxIndex' codeRef = do
--     let fn = codeRef ^. #function
--         funcAddr = fn ^. #address
--     mBnFunc <- BnFunc.getFunctionStartingAt bv Nothing funcAddr
--     case mBnFunc of
--       Nothing -> error $ "No function found at " <> show funcAddr
--       Just bnFunc -> do
--         mlilSsaFunc <- BnFunc.getMLILSSAFunction bnFunc
--         let start = fromIntegral $ codeRef ^. #startIndex
--             end = fromIntegral $ codeRef ^. #endIndex
--         nodeInstrs <- traverse (Mlil.instruction mlilSsaFunc) $ Bn.InstructionIndex <$> [start .. end]
--         addrWidth <- Bn.getViewAddressSize bv
--         let convSt =
--               PilImp.mkConverterState
--                 bv
--                 ctxIndex'
--                 Pil.knownFuncDefs
--                 addrWidth
--                 fn
--         PilImp.convert convSt $ PilImp.convertInstrsSplitPhi nodeInstrs
--    where
--     bv :: GhidraState
--     bv = imp ^. #ghidraState
