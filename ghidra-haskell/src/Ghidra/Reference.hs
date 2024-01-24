{-# LANGUAGE DataKinds #-}
module Ghidra.Reference
  ( module Ghidra.Reference
  ) where

import Ghidra.Prelude

import Ghidra.State (GhidraState)
import qualified Ghidra.State as State
import qualified Language.Java as Java
import Ghidra.Types (Addressable, toAddrs)
import Ghidra.Types.Internal (Ghidra, runIO)
import qualified Ghidra.Types as J
import qualified Foreign.JNI as JNI
import qualified Ghidra.Address as Addr
import Ghidra.Address (Address)
import Ghidra.Types.Function (Function)
import qualified Ghidra.Function as Func


referenceIteratorToList :: J.ReferenceIterator -> Ghidra [J.Reference]
referenceIteratorToList x = do
  hasNext :: Bool <- runIO $ Java.call x "hasNext"
  if hasNext
    then do
      ref <- runIO $ Java.call x "next" >>= JNI.newGlobalRef
      (ref:) <$> referenceIteratorToList x
    else return []

getReferencesToAddress :: GhidraState -> J.Address -> Ghidra [J.Reference]
getReferencesToAddress gs addr = do
  prg <- State.getProgram gs
  rm :: J.ReferenceManager <- runIO $ Java.call prg "getReferenceManager" >>= JNI.newGlobalRef
  runIO (Java.call rm "getReferencesTo" addr >>= JNI.newGlobalRef) >>= referenceIteratorToList

getReferencesTo :: (Addressable a) => GhidraState -> a -> Ghidra [J.Reference]
getReferencesTo gs x = toAddrs x >>= concatMapM (getReferencesToAddress gs)

getFromAddress :: J.Reference -> Ghidra J.Address
getFromAddress ref = runIO $ Java.call ref "getFromAddress"

getToAddress :: J.Reference -> Ghidra J.Address
getToAddress ref = runIO $ Java.call ref "getToAddress"

data FuncRef = FuncRef
  { caller :: Function
  , callee :: Function
  , callerAddr :: Address
  } deriving (Eq, Ord, Show, Generic)

toFuncReference :: GhidraState -> J.Reference -> Ghidra (Maybe FuncRef)
toFuncReference gs ref = do
  callerAddr_ <- getFromAddress ref
  calleeAddr <- getToAddress ref
  (,) <$> Func.fromAddr gs callerAddr_ <*> Func.fromAddr gs calleeAddr >>= \case
    (Just callerFunc, Just calleeFunc) -> fmap Just $
      FuncRef <$> Func.mkFunction callerFunc <*> Func.mkFunction calleeFunc <*> Addr.mkAddress callerAddr_
    _ -> return Nothing

getFunctionRefs :: GhidraState -> J.Function -> Ghidra [FuncRef]
getFunctionRefs gs fn = do
  funcStart <- J.toAddr fn
  refs <- getReferencesTo gs funcStart
  catMaybes <$> traverse (toFuncReference gs) refs
