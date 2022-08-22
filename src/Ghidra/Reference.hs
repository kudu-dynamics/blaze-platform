{-# LANGUAGE DataKinds #-}
module Ghidra.Reference
  ( module Ghidra.Reference
  ) where

import Ghidra.Prelude hiding (toList)

import Language.Clojure
import System.IO.Memoize (once)
import Foreign.JNI.Types (JObject)
import qualified Ghidra.Program as Program
import Ghidra.State (GhidraState(GhidraState))
import qualified Ghidra.State as State
import qualified Language.Java as Java
import Ghidra.Util (convertOpt)
import qualified Language.Clojure.Map as ClojureMap
import Language.Java (J, VariadicIO)
import Ghidra.Types (Addressable, Address, toAddrs)


requireModule :: IO ()
requireModule = unsafePerformIO . once $ do
  _ <- readEval "(require (quote [ghidra-clojure.reference]))"
  return ()

type Reference = J ('Java.Class "ghidra.program.model.symbol.Reference")

type ReferenceManager = J ('Java.Class "ghidra.program.model.symbol.ReferenceManager")

type ReferenceIterator = J ('Java.Class "ghidra.program.model.symbol.ReferenceIterator")

referenceIteratorToList :: ReferenceIterator -> IO [Reference]
referenceIteratorToList x = do
  hasNext :: Bool <- Java.call x "hasNext"
  if hasNext
    then do
      ref <- Java.call x "next"
      (ref:) <$> referenceIteratorToList x
    else return []

getReferencesToAddress :: GhidraState -> Address -> IO [Reference]
getReferencesToAddress gs addr = do
  prg <- State.getProgram gs
  rm :: ReferenceManager <- Java.call prg "getReferenceManager"
  Java.call rm "getReferencesTo" addr >>= referenceIteratorToList

getReferencesTo :: (Addressable a) => GhidraState -> a -> IO [Reference]
getReferencesTo gs x = toAddrs x >>= concatMapM (getReferencesToAddress gs)
