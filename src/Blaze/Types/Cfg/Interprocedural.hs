module Blaze.Types.Cfg.Interprocedural where

import Blaze.Prelude
import Blaze.Types.Cfg (PilCfg)
import Blaze.Types.Pil (Stmt)
import Blaze.CallGraph (Function)
import Blaze.Types.Import (ImportResult)
import Blaze.Import.Cfg (CfgImporter (NodeMapType, NodeDataType))
import qualified Blaze.Import.Cfg as CfgImp
import Data.HashMap.Strict as HMap

newtype InterCfg = InterCfg {unInterCfg :: PilCfg}

newtype Builder a b = Builder
  {_runBuilder :: StateT (BuilderState a) IO b}
  deriving (Functor)
  deriving newtype (Applicative, Monad, MonadState (BuilderState a), MonadIO)

data BuilderState a = BuilderState
  {
  -- | Get a UUID in order to create unique CFG nodes 
  -- (and use for PIL statement contexts?)
    getId :: IO UUID
  -- | Presumably using a CfgImporter instance, provide a import result with a
  -- PIL CFG given a function
  , getCfg :: Function -> IO (Maybe (ImportResult PilCfg a))
  -- |A mapping from UUIDs to a map that maps CFG nodes to import source locations
  , importMap :: HashMap UUID a
  }

mkBuilderState ::
  ( CfgImporter a
  , NodeDataType a ~ [Stmt]
  , NodeMapType a ~ b) =>
  a ->
  IO UUID ->
  BuilderState b
mkBuilderState imp getUuid =
  BuilderState 
    { getId = getUuid
    , getCfg = CfgImp.getCfg imp
    , importMap = HMap.empty}

runBuilder :: Builder a b -> BuilderState a -> IO (b, BuilderState a)
runBuilder m = runStateT $ _runBuilder m

build :: BuilderState a -> Builder a b -> IO b
build s m = fst <$> runBuilder m s