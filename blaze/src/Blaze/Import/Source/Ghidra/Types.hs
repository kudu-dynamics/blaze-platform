module Blaze.Import.Source.Ghidra.Types where

import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Cfg (CodeReference)
import Blaze.Types.Pil (PilVar)
import Blaze.Types.CachedMap (CachedMap)

import Ghidra.State (GhidraState)
import Ghidra.Types (HighFunction)
import qualified Ghidra.Types.Address as GAddr


convertAddress :: GAddr.Address -> Address
convertAddress x = fromIntegral
  $ x ^. #offset * fromIntegral (x ^. #space . #addressableUnitSize)

type PcodeReference = CodeReference GAddr.Address

newtype PilPcodeMap a = PilPcodeMap
  { sourceVars :: HashMap PilVar a
  }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

data GhidraImporter = GhidraImporter
  { ghidraState :: GhidraState
  , highFnCache :: CachedMap Address (Maybe HighFunction)
  } deriving (Show, Generic)
