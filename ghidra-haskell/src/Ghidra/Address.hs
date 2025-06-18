{-# LANGUAGE DataKinds #-}
module Ghidra.Address
  ( module Ghidra.Address
  , module Exports
  ) where

import Ghidra.Prelude hiding (toList, Const)

import qualified Ghidra.Types as J
import Ghidra.Types.Address as Exports
import Ghidra.Types.Internal (Ghidra, runIO)
import Ghidra.Util (maybeNull)

import qualified Language.Java as Java
import qualified Foreign.JNI as JNI


readAddressSpaceName :: Text -> AddressSpaceName
readAddressSpaceName t = case t of
  "EXTERNAL" -> EXTERNAL
  "HASH" -> HASH
  "const" -> Const
  "ram" -> Ram
  "register" -> Register
  "stack" -> Stack
  "unique" -> Unique
  _ -> Other t

showAddressSpaceName :: AddressSpaceName -> Text
showAddressSpaceName (Other t) = t
showAddressSpaceName x = show x

mkAddressSpace :: J.AddressSpace -> Ghidra AddressSpace
mkAddressSpace x = runIO $ do
  spaceId :: Int32 <- Java.call x "getSpaceID"
  ptrSize :: Int32 <- Java.call x "getPointerSize"
  addressableUnitSize :: Int32 <- Java.call x "getAddressableUnitSize"
  name :: Text <- Java.call x "getName" >>= JNI.newGlobalRef >>= Java.reify
  return $
    AddressSpace
      (fromIntegral spaceId)
      (fromIntegral ptrSize)
      (fromIntegral addressableUnitSize)
      (readAddressSpaceName name)

getAddressSpace :: J.AddressFactory -> Text -> Ghidra (Maybe J.AddressSpace)
getAddressSpace af spaceName = do
  maybeNull <$> runIO (Java.reflect spaceName >>= Java.call af "getAddressSpace" >>= JNI.newGlobalRef)

getSpaceID :: J.AddressSpace -> Ghidra Int32
getSpaceID aspace = runIO $ Java.call aspace "getSpaceID"

--- | Returns the register `AddressSpace`.
getRegisterSpace :: J.AddressFactory -> Ghidra AddressSpace
getRegisterSpace af = do
  space :: J.AddressSpace <- runIO $ Java.call af "getRegisterSpace"
                             >>= JNI.newGlobalRef
  mkAddressSpace space

mkAddress :: J.Address -> Ghidra Address
mkAddress addr = do
  addrSpace <- runIO (Java.call addr "getAddressSpace") >>= mkAddressSpace
  offset :: Int64 <- runIO $ Java.call addr "getOffset"
  return $ Address addrSpace (fromIntegral offset)
  -- return $ Address addrSpace (Just ".txt") (fromIntegral offset)

-- getExternalAddress :: GhidraState -> Int64 -> Ghidra J.Address
-- getExternalAddress gs offset = do
--   prg <- State.getProgram gs
--   af <- Program.getAddressFactory prg
  

getAddress :: J.AddressFactory -> Int32 -> Int64 -> Ghidra J.Address
getAddress af spaceID offset = runIO $ 
  Java.call af "getAddress" spaceID offset
  >>= JNI.newGlobalRef
  >>= Java.reify

