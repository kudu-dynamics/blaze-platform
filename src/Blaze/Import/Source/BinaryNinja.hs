module Blaze.Import.Source.BinaryNinja (
  module Blaze.Import.Source.BinaryNinja,
  module Exports,
) where

import Binja.Core (BNBinaryView)
import qualified Binja.Core as Bn
import qualified Binja.Function as BnFunc
import qualified Binja.MLIL as Mlil
import Blaze.Cfg (NodeRefMap)
import Blaze.Import.CallGraph (CallGraphImporter (getCallSites, getFunction, getFunctions))
import Blaze.Import.Cfg (CfgImporter (..))
import Blaze.Import.Pil (PilImporter (..))
import qualified Blaze.Import.Source.BinaryNinja.CallGraph as CallGraph
import qualified Blaze.Import.Source.BinaryNinja.Cfg as Cfg
import qualified Blaze.Import.Source.BinaryNinja.Pil as PilImp
import qualified Blaze.Import.Source.BinaryNinja.Pil.Path as Path
import Blaze.Import.Source.BinaryNinja.Types as Exports
import qualified Blaze.Pil as Pil
import qualified Blaze.Types.Path.AlgaPath as AlgaPath
import Blaze.Prelude hiding (Symbol)

newtype BNImporter = BNImporter
  { binaryView :: BNBinaryView
  }
  deriving (Eq, Ord, Show, Generic)

instance CallGraphImporter BNImporter where
  getFunction imp = CallGraph.getFunction (imp ^. #binaryView)

  getFunctions = CallGraph.getFunctions . view #binaryView

  getCallSites imp = CallGraph.getCallSites (imp ^. #binaryView)

instance CfgImporter BNImporter MlilSsaInstructionIndex (NodeRefMap MlilSsaInstructionIndex MlilSsaInstructionIndex) where
  getCfg imp = Cfg.getCfg (imp ^. #binaryView)

instance PilImporter BNImporter MlilSsaInstructionIndex MlilSsaInstructionIndex where
  getFuncStatements imp =
    PilImp.getFuncStatements (imp ^. #binaryView)

  getPathStatements imp =
    Path.convertPath (imp ^. #binaryView)
  
  getNodeStatements imp (_, codeRef) = do
    let fn = codeRef ^. #function
        funcAddr = fn ^. #address
    mBnFunc <- BnFunc.getFunctionStartingAt bv Nothing funcAddr
    case mBnFunc of
      Nothing -> error $ "No function found at " <> show funcAddr
      Just bnFunc -> do 
        mlilSsaFunc <- BnFunc.getMLILSSAFunction bnFunc
        let start = fromIntegral $ codeRef ^. #startIndex
            end = fromIntegral $ codeRef ^. #endIndex
        nodeInstrs <- traverse (Mlil.instruction mlilSsaFunc) $ Bn.InstructionIndex <$> [start..end]
        addrWidth <- Bn.getViewAddressSize bv
        let convSt = PilImp.mkConverterState 
                      bv
                      Pil.knownFuncDefs
                      addrWidth
                      fn
                      AlgaPath.empty
        PilImp.convert convSt $ PilImp.convertInstrsSplitPhi nodeInstrs
   where
    bv :: BNBinaryView
    bv = imp ^. #binaryView