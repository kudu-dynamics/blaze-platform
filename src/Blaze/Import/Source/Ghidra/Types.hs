module Blaze.Import.Source.Ghidra.Types where

import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Cfg (CfNode, CodeReference, NodeRefMap, NodeRefMapEntry, PilNode)
import Blaze.Types.Pil (PilVar)
import qualified Blaze.Types.Pil as Pil
import qualified Ghidra.Types.Address as GAddr
import Ghidra.Types.Pcode.Lifted (PcodeOp)

convertAddress :: GAddr.Address -> Address
convertAddress x = fromIntegral
  $ x ^. #offset * (fromIntegral $ x ^. #space . #addressableUnitSize)

type PcodeReference = CodeReference GAddr.Address

data PilPcodeMap a = PilPcodeMap
  { sourceVars :: HashMap PilVar a
  } deriving (Eq, Ord, Show, Generic, Hashable)

