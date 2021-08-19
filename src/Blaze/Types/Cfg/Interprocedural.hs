module Blaze.Types.Cfg.Interprocedural where

import Blaze.Prelude
import Blaze.Types.Cfg (PilCfg)
import Blaze.Types.Function (Function)
import Blaze.Types.Pil (Stmt)
import Blaze.Types.Import (ImportResult)
import Blaze.Import.Cfg (CfgImporter (NodeMapType, NodeDataType))
import qualified Blaze.Import.Cfg as CfgImp
import Data.HashMap.Strict as HMap
import Blaze.Types.Pil.Common (CtxId)

-- TODO : Instead of wrapping PilCfg to create an InterCfg, assume
--        every Cfg could be an InterCfg and instead introduce a type
--        to indicate a CFG is for a single function? Possibly a different
--        type if it's contained in a function but not all blocks?
newtype InterCfg = InterCfg {unInterCfg :: PilCfg}
  deriving (Eq, Show, Generic)

newtype Builder a b = Builder
  {_runBuilder :: StateT (BuilderState a) IO b}
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad, 
    MonadState (BuilderState a), MonadIO, MonadFail)

-- TODO: Consider replacing CtxId with UUID
data BuilderState a = BuilderState
  {
  -- | Presumably using a CfgImporter instance, provide a import result with a
  -- PIL CFG given a function
    getCfg :: Function -> IO (Maybe (ImportResult PilCfg a))
  -- |A mapping from UUIDs to a map that maps CFG nodes to import source locations
  , importMap :: HashMap CtxId a
  }
  deriving (Generic)

mkBuilderState ::
  ( CfgImporter a
  , NodeDataType a ~ [Stmt]
  , NodeMapType a ~ b) =>
  a ->
  BuilderState b
mkBuilderState imp =
  BuilderState 
    { getCfg = CfgImp.getCfg imp
    , importMap = HMap.empty}

runBuilder :: Builder a b -> BuilderState a -> IO (b, BuilderState a)
runBuilder m = runStateT $ _runBuilder m

build :: BuilderState a -> Builder a b -> IO b
build s m = fst <$> runBuilder m s
