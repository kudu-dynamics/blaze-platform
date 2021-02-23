module Blaze.Types.Cfg.Interprocedural where

import Blaze.Prelude
import Blaze.Types.Cfg (PilCfg)
import Blaze.Types.Pil (Stmt)
import Blaze.Function (Function)
import Blaze.Types.Import (ImportResult)
import Blaze.Import.Cfg (CfgImporter (NodeMapType, NodeDataType))
import qualified Blaze.Import.Cfg as CfgImp
import Data.HashMap.Strict as HMap
import Blaze.Types.Pil.Common (CtxIndex)
import Control.Monad (MonadFail)

newtype InterCfg = InterCfg {unInterCfg :: PilCfg}
  deriving (Eq, Show, Generic)

newtype Builder a b = Builder
  {_runBuilder :: StateT (BuilderState a) IO b}
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad, 
    MonadState (BuilderState a), MonadIO, MonadFail)

-- TODO: Consider replacing CtxIndex with UUID
data BuilderState a = BuilderState
  {
  -- | Track an ID used for creating contexts
  -- (and use for PIL statement contexts?)
    nextId :: CtxIndex
  -- | Presumably using a CfgImporter instance, provide a import result with a
  -- PIL CFG given a function
  , getCfg :: CtxIndex -> Function -> IO (Maybe (ImportResult PilCfg a))
  -- |A mapping from UUIDs to a map that maps CFG nodes to import source locations
  , importMap :: HashMap CtxIndex a
  }
  deriving (Generic)

mkBuilderState ::
  ( CfgImporter a
  , NodeDataType a ~ [Stmt]
  , NodeMapType a ~ b) =>
  a ->
  CtxIndex ->
  BuilderState b
mkBuilderState imp startId =
  BuilderState 
    { nextId = startId
    , getCfg = CfgImp.getCfg imp
    , importMap = HMap.empty}

runBuilder :: Builder a b -> BuilderState a -> IO (b, BuilderState a)
runBuilder m = runStateT $ _runBuilder m

build :: BuilderState a -> Builder a b -> IO b
build s m = fst <$> runBuilder m s