{-# LANGUAGE DataKinds #-}
module Ghidra.Reference
  ( module Ghidra.Reference
  ) where

import Ghidra.Prelude

import qualified Language.Java as Java
import Ghidra.Types (Addressable, toAddrs)
import Ghidra.Types.Internal (Ghidra, runIO)
import qualified Ghidra.Types as J
import qualified Foreign.JNI as JNI
import qualified Ghidra.Address as Addr
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

getReferencesToAddress :: J.ProgramDB -> J.Address -> Ghidra [J.Reference]
getReferencesToAddress prg addr = do
  rm :: J.ReferenceManager <- runIO $ Java.call prg "getReferenceManager" >>= JNI.newGlobalRef
  runIO (Java.call rm "getReferencesTo" addr >>= JNI.newGlobalRef) >>= referenceIteratorToList

getReferencesFromAddress :: J.ProgramDB -> J.Address -> Ghidra [J.Reference]
getReferencesFromAddress prg addr = do
  rm :: J.ReferenceManager <- runIO $ Java.call prg "getReferenceManager" >>= JNI.newGlobalRef
  refs :: J.ReferenceArray <- runIO $ Java.call rm "getReferencesFrom" addr >>= JNI.newGlobalRef
  n <- runIO $ JNI.getArrayLength refs
  forM [0 .. n - 1] $ \i -> do
    obj :: J.Object <- runIO $ JNI.getObjectArrayElement refs i >>= JNI.newGlobalRef
    return $ coerce obj

getReferencesTo :: (Addressable a) => J.ProgramDB -> a -> Ghidra [J.Reference]
getReferencesTo prg x = toAddrs x >>= concatMapM (getReferencesToAddress prg)

getFromAddress :: J.Reference -> Ghidra J.Address
getFromAddress ref = runIO $ Java.call ref "getFromAddress"

getToAddress :: J.Reference -> Ghidra J.Address
getToAddress ref = runIO $ Java.call ref "getToAddress"

data FuncRef = FuncRef
  { caller :: Function
  , callee :: Function
  , callerAddr :: Addr.Address
  } deriving (Eq, Ord, Show, Generic)

toFuncReference :: J.ProgramDB -> J.Reference -> Ghidra (Maybe FuncRef)
toFuncReference prg ref = do
  callerAddr_ <- getFromAddress ref
  calleeAddr <- getToAddress ref
  (,) <$> Func.fromAddr prg callerAddr_ <*> Func.fromAddr prg calleeAddr >>= \case
    (Just callerFunc, Just calleeFunc) -> fmap Just $
      FuncRef <$> Func.mkFunction callerFunc <*> Func.mkFunction calleeFunc <*> Addr.mkAddress callerAddr_
    _ -> return Nothing

getFunctionRefs :: J.ProgramDB -> J.Function -> Ghidra [FuncRef]
getFunctionRefs prg fn = do
  funcStart <- J.toAddr fn
  refs <- getReferencesTo prg funcStart
  catMaybes <$> traverse (toFuncReference prg) refs
