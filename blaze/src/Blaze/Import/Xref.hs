module Blaze.Import.Xref where

import Blaze.Prelude
import Blaze.Types.Function (Function)


-- | A reference to an address from within a function.
data Xref = Xref
  { function :: Function
  , address  :: Address    -- ^ The instruction address where the reference occurs
  } deriving (Eq, Ord, Show, Generic)

class XrefImporter a where
  -- | Get all references to a given address.
  getXrefsTo :: a -> Address -> IO [Xref]
