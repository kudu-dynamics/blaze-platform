{-# LANGUAGE TemplateHaskell #-}

module Blaze.Import.Source.BinaryNinja (
  module Blaze.Import.Source.BinaryNinja,
--  module Exports,
) where
  
import qualified Binja.Core as Binja
import Binja.Core (BNBinaryView)
import Blaze.Import.CallGraph (CallGraphImporter (getCallSites, getFunctions, getFunction))
import Blaze.Import.Cfg (CfgImporter (getCfg))
import Blaze.Import.Pil (PilImporter (getFuncStatements, getPathStatements))
import Blaze.Prelude hiding (Symbol)
import Blaze.Import.Source.BinaryNinja.Types
import qualified Blaze.Import.Source.BinaryNinja.CallGraph as CallGraph
import qualified Blaze.Import.Source.BinaryNinja.Cfg as Cfg
import qualified Blaze.Import.Source.BinaryNinja.Pil as Pil
import qualified Blaze.Import.Source.BinaryNinja.Pil.Path as Path


newtype BNImporter
  = BNImporter
      { binaryView :: BNBinaryView
      }
  deriving (Eq, Ord, Show, Generic)

instance CallGraphImporter BNImporter where
  getFunction imp = CallGraph.getFunction (imp ^. #binaryView)
  
  getFunctions = CallGraph.getFunctions . view #binaryView

  getCallSites imp = CallGraph.getCallSites (imp ^. #binaryView)

instance CfgImporter BNImporter NodeMap where
  getCfg imp = Cfg.getCfg (imp ^. #binaryView)


instance PilImporter BNImporter where
  getFuncStatements imp =
    Pil.getFuncStatements (imp ^. #binaryView)

  getPathStatements imp =
    Path.convertPath (imp ^. #binaryView)

