{-# LANGUAGE DataKinds #-}
module Ghidra.Reference
  ( module Ghidra.Reference
  ) where

import Ghidra.Prelude hiding (toList)

import Ghidra.State (GhidraState)
import qualified Ghidra.State as State
import qualified Language.Java as Java
import Ghidra.Types (Addressable, toAddrs)
import qualified Ghidra.Types as J
import qualified Foreign.JNI as JNI


referenceIteratorToList :: J.ReferenceIterator -> IO [J.Reference]
referenceIteratorToList x = do
  hasNext :: Bool <- Java.call x "hasNext"
  if hasNext
    then do
      ref <- Java.call x "next" >>= JNI.newGlobalRef
      (ref:) <$> referenceIteratorToList x
    else return []

getReferencesToAddress :: GhidraState -> J.Address -> IO [J.Reference]
getReferencesToAddress gs addr = do
  prg <- State.getProgram gs
  rm :: J.ReferenceManager <- Java.call prg "getReferenceManager" >>= JNI.newGlobalRef
  Java.call rm "getReferencesTo" addr >>= JNI.newGlobalRef >>= referenceIteratorToList

getReferencesTo :: (Addressable a) => GhidraState -> a -> IO [J.Reference]
getReferencesTo gs x = toAddrs x >>= concatMapM (getReferencesToAddress gs)
