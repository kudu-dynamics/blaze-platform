module Ghidra.ExternalLocation where

import Ghidra.Prelude hiding (toList)

import qualified Language.Java as Java
import Ghidra.Util (maybeNull)
import qualified Ghidra.Types as J
import Ghidra.Types.Internal (Ghidra, runIO)
import qualified Ghidra.Address as Addr
import qualified Foreign.JNI as JNI


getAddress :: J.ExternalLocationDB -> Ghidra (Maybe Addr.Address)
getAddress loc = do
  maddr <- runIO $ maybeNull <$> Java.call loc "getAddress"
  maybe (return Nothing) (fmap Just . Addr.mkAddress) maddr

getExternalSpaceAddress :: J.ExternalLocationDB -> Ghidra (Maybe Addr.Address)
getExternalSpaceAddress loc = do
  maddr <- runIO $ maybeNull <$> Java.call loc "getExternalSpaceAddress"
  maybe (return Nothing) (fmap Just . Addr.mkAddress) maddr

getLibraryName :: J.ExternalLocationDB -> Ghidra Text
getLibraryName loc = runIO $ Java.call loc "getLibraryName" >>= JNI.newGlobalRef >>= Java.reify

getLabel :: J.ExternalLocationDB -> Ghidra Text
getLabel loc = runIO $ Java.call loc "getLabel" >>= JNI.newGlobalRef >>= Java.reify
