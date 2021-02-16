module Blaze.Import.Cfg where

import Blaze.Prelude
import Blaze.Types.CallGraph (Function)
import Blaze.Types.Cfg
import Blaze.Types.Import (ImportResult)

class CfgImporter a where
  type NodeDataType a
  type NodeMapType a
  getCfg :: a -> Function -> IO (Maybe (ImportResult (Cfg (NodeDataType a)) (NodeMapType a)))
