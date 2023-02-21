module Blaze.Import.Source.Ghidra.Types where

import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Cfg (CodeReference)
import Blaze.Types.Pil (PilVar)
import qualified Ghidra.Types.Address as GAddr

convertAddress :: GAddr.Address -> Address
convertAddress x = fromIntegral
  $ x ^. #offset * fromIntegral (x ^. #space . #addressableUnitSize)

type PcodeReference = CodeReference GAddr.Address

newtype PilPcodeMap a = PilPcodeMap
  { sourceVars :: HashMap PilVar a
  } deriving (Eq, Ord, Show, Generic, Hashable)

