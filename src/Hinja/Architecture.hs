module Hinja.Architecture
  ( module Exports
  , getSemClasses
  , getSemGroups
  , create
  ) where


import Hinja.Prelude

import Hinja.Types.Architecture
import Hinja.Types.Architecture as Exports
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Hinja.C.Main as BN
import Hinja.C.Pointers


getSemClasses :: BNArchitecture -> IO SemClasses
getSemClasses archPtr = fmap (SemClasses . Map.fromList) $
  BN.getAllArchitectureSemanticFlagClasses archPtr >>= traverse f
  where
    f :: Word32 -> IO (Word32, Text)
    f n = (n,) . Text.pack <$> BN.getArchitectureSemanticFlagClassName archPtr n

getSemGroups :: BNArchitecture -> IO SemGroups
getSemGroups archPtr = fmap (SemGroups . Map.fromList) $
  BN.getAllArchitectureSemanticFlagGroups archPtr >>= traverse f
  where
    f :: Word32 -> IO (Word32, Text)
    f n = (n,) . Text.pack <$> BN.getArchitectureSemanticFlagGroupName archPtr n

create :: BNArchitecture -> IO Architecture
create ptr = Architecture ptr
             <$> (Text.pack <$> BN.getArchitectureName ptr)
             <*> getSemClasses ptr
             <*> getSemGroups ptr
