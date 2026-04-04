{-# LANGUAGE DataKinds #-}
module Ghidra.Symbol
  ( module Ghidra.Symbol
  ) where

import Ghidra.Prelude

import qualified Language.Java as Java
import qualified Foreign.JNI as JNI
import Ghidra.Types.Internal (Ghidra, runIO)
import qualified Ghidra.Types as J


-- | Get the program's symbol table.
getSymbolTable :: J.ProgramDB -> Ghidra J.SymbolTable
getSymbolTable prg =
  runIO $ Java.call prg "getSymbolTable" >>= JNI.newGlobalRef

-- | Look up global symbols by name.
-- Returns all symbols with the given name in the global namespace.
getGlobalSymbols :: J.SymbolTable -> Text -> Ghidra [J.Symbol]
getGlobalSymbols symTable name = do
  jName <- runIO $ Java.reflect name >>= JNI.newGlobalRef
  jList :: J.SymbolList <- runIO $ Java.call symTable "getGlobalSymbols" jName >>= JNI.newGlobalRef
  listSize :: Int32 <- runIO $ Java.call jList "size"
  forM [0 .. listSize - 1] $ \i -> do
    obj :: J.Object <- runIO $ Java.call jList "get" i >>= JNI.newGlobalRef
    return $ coerce obj

-- | Get the address of a symbol.
getSymbolAddress :: J.Symbol -> Ghidra J.Address
getSymbolAddress s =
  runIO $ Java.call s "getAddress" >>= JNI.newGlobalRef

-- | Get the name of a symbol.
getSymbolName :: J.Symbol -> Ghidra Text
getSymbolName s =
  runIO $ Java.call s "getName" >>= Java.reify

-- | Look up a global symbol by name and return its address.
-- If multiple symbols match, returns the first one.
lookupGlobalAddress :: J.ProgramDB -> Text -> Ghidra (Maybe J.Address)
lookupGlobalAddress prg name = do
  symTable <- getSymbolTable prg
  syms <- getGlobalSymbols symTable name
  case syms of
    [] -> return Nothing
    (s : _) -> Just <$> getSymbolAddress s

-- | Look up a global symbol by name and return all matching addresses.
lookupGlobalAddresses :: J.ProgramDB -> Text -> Ghidra [J.Address]
lookupGlobalAddresses prg name = do
  symTable <- getSymbolTable prg
  syms <- getGlobalSymbols symTable name
  traverse getSymbolAddress syms
