module Blaze.Import.Cfg where

import Blaze.Prelude
import Blaze.Types.Cfg
import Blaze.Types.Function (Function)
import Blaze.Types.Import (ImportResult)
import Blaze.Types.Pil (CtxIndex)

class CfgImporter a where
  type NodeDataType a
  type NodeMapType a
  getCfg
    :: a
    -> CtxIndex
    -> Function
    -> IO (Maybe (ImportResult (Cfg (NodeDataType a)) (NodeMapType a)))
