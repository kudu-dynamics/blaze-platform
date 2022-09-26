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
import qualified Ghidra.Address as Addr
import Ghidra.Types.Function (Function)
import qualified Ghidra.Function as Func


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

getFromAddress :: J.Reference -> IO J.Address
getFromAddress ref = Java.call ref "getFromAddress"

getToAddress :: J.Reference -> IO J.Address
getToAddress ref = Java.call ref "getToAddress"

data FuncRef = FuncRef
  { caller :: Function
  , callee :: Function
  , callerAddr :: J.Address
  } deriving (Eq, Ord, Show, Generic)

toFuncReference :: GhidraState -> J.Reference -> IO (Maybe FuncRef)
toFuncReference gs ref = do
  callerAddr <- getFromAddress ref
  calleeAddr <- getToAddress ref
  (,) <$> Func.fromAddr gs callerAddr <*> Func.fromAddr gs calleeAddr >>= \case
    (Just callerFunc, Just calleeFunc) -> fmap Just $
      FuncRef <$> Func.mkFunction callerFunc <*> Func.mkFunction calleeFunc <*> return callerAddr
    _ -> return Nothing

getFunctionRefs :: GhidraState -> J.Function -> IO [FuncRef]
getFunctionRefs gs fn = do
  funcStart <- J.toAddr fn
  refs <- getReferencesTo gs funcStart
  catMaybes <$> traverse (toFuncReference gs) refs
