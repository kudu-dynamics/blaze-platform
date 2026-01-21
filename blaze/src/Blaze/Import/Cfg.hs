module Blaze.Import.Cfg where

import Blaze.Prelude
import Blaze.Types.Cfg
import Blaze.Types.Import (ImportResult, TypeHints)
import Blaze.Types.Function (Function)
import Blaze.Types.Pil (CtxId)


-- | Use associated types to specify the import results.
class CfgImporter a where
  type NodeDataType a
  type NodeMapType a
  getCfg
    :: a
    -> Function
    -> CtxId
    -> IO (Maybe (ImportResult (NodeMapType a) (Cfg (NodeDataType a))))
  getCfgWithTypeHints
    :: a
    -> Function
    -> CtxId
    -> IO (Maybe (ImportResult (NodeMapType a) (Cfg (NodeDataType a))), TypeHints)

getCfg_
  :: CfgImporter a
  => a
  -> Function
  -> CtxId
  -> IO (Maybe (Cfg (NodeDataType a)))
getCfg_ imp func = fmap (fmap $ view #result) . getCfg imp func

getCfgAndTypeHints_
  :: CfgImporter a
  => a
  -> Function
  -> CtxId
  -> IO (Maybe (Cfg (NodeDataType a)), TypeHints)
getCfgAndTypeHints_ imp func ctxId = do
  (mCfg, typeHints) <- getCfgWithTypeHints imp func ctxId
  return (fmap (view #result) mCfg, typeHints)

