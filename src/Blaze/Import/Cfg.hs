module Blaze.Import.Cfg where

import Blaze.Prelude
import Blaze.Types.Cfg
import Blaze.Types.Function (Function)
import Blaze.Types.Import (ImportResult)
import Blaze.Types.Pil (CtxId)

class CfgImporter a where
  type NodeDataType a
  type NodeMapType a
  getCfg
    :: a
    -> CtxId
    -> Function
    -> IO (Maybe (ImportResult (Cfg (NodeDataType a)) (NodeMapType a)))
