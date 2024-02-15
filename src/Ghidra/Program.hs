{-# LANGUAGE DataKinds #-}

module Ghidra.Program (
  module Ghidra.Program,
) where

import Ghidra.Prelude hiding (force)

import Data.BinaryAnalysis qualified as BA
import Data.HashMap.Strict qualified as HashMap
import Foreign.JNI qualified as JNI
import Language.Java qualified as Java

import Ghidra.Address (
  Address,
  AddressSpace,
  AddressSpaceId (AddressSpaceId),
  AddressSpaceMap,
  getAddress,
  mkAddress,
  mkAddressSpace,
 )
import Ghidra.Register (Register)
import Ghidra.Register qualified as Reg
import Ghidra.Types qualified as J
import Ghidra.Types.Internal (Ghidra (..), runIO)
import Ghidra.Util (isJNull)

getAddressFactory :: J.ProgramDB -> Ghidra J.AddressFactory
getAddressFactory p = runIO $ Java.call p "getAddressFactory" >>= JNI.newGlobalRef

-- | Returns an address from the constant space of the program.
getConstantAddress :: J.AddressFactory -> BA.Address -> Ghidra J.Address
getConstantAddress gen off = runIO $ Java.call gen "getConstantAddress" (fromIntegral off :: Int64) >>= JNI.newGlobalRef

getMinAddress :: J.ProgramDB -> Ghidra Address
getMinAddress p = runIO (Java.call p "getMinAddress" >>= JNI.newGlobalRef >>= Java.reify) >>= mkAddress

getMaxAddress :: J.ProgramDB -> Ghidra Address
getMaxAddress p = runIO (Java.call p "getMaxAddress" >>= JNI.newGlobalRef >>= Java.reify) >>= mkAddress

getAddressSpaceMap :: J.ProgramDB -> Ghidra AddressSpaceMap
getAddressSpaceMap p = do
  af <- getAddressFactory p
  spaces :: [J.AddressSpace] <-
    runIO $
      Java.call af "getAllAddressSpaces"
        >>= Java.reify
        >>= traverse JNI.newGlobalRef
  HashMap.fromList <$> traverse f spaces
  where
    f :: J.AddressSpace -> Ghidra (AddressSpaceId, AddressSpace)
    f x = do
      id <- runIO $ AddressSpaceId <$> Java.call x "getSpaceID"
      (id,) <$> mkAddressSpace x

getExecutableMD5 :: J.ProgramDB -> Ghidra Text
getExecutableMD5 p =
  runIO $
    Java.call p "getExecutableMD5"
      >>= Java.reify

getExecutablePath :: J.ProgramDB -> Ghidra Text
getExecutablePath p =
  runIO $
    Java.call p "getExecutablePath"
      >>= Java.reify

-- | Given a program DB, address, and size, provide the associated register.
getRegister :: J.ProgramDB -> Int64 -> Int -> Ghidra (Maybe Register)
getRegister p offset size = do
  jAddrFactory <- getAddressFactory p
  jSpace :: J.AddressSpace <-
    runIO $ Java.call jAddrFactory "getRegisterSpace" >>= JNI.newGlobalRef
  spaceId :: Int32 <- runIO $ Java.call jSpace "getSpaceID"
  jAddr <-
    getAddress
      jAddrFactory
      spaceId
      offset
  let size' :: Int32 = fromIntegral size
  reg :: J.Register <-
    runIO $
      Java.call p "getRegister" jAddr size'
        >>= JNI.newLocalRef
  if isJNull reg
    then return Nothing
    else do
      regName <- Reg.getName reg
      regLength <- Reg.getBitLength reg
      return $
        Just
          Reg.Register
            { name = regName
            , length = regLength
            }

setImageBase :: J.ProgramDB -> Int64 -> Bool -> Ghidra ()
setImageBase p baseAddr commit = do
  jAddrFactory <- getAddressFactory p
  jSpace :: J.AddressSpace <-
    runIO $ Java.call jAddrFactory "getDefaultAddressSpace" >>= JNI.newGlobalRef
  spaceId :: Int32 <- runIO $ Java.call jSpace "getSpaceID"
  jAddr <-
    getAddress
      jAddrFactory
      spaceId
      baseAddr
  runIO $
    Java.call p "setImageBase" jAddr commit

withTransaction :: J.ProgramDB -> Text -> Ghidra a -> Ghidra a
withTransaction p description action = do
  tid <- startTransaction
  runIO (try $ _runGhidra action) >>= \case
    Left (err :: SomeException) -> do
      endTransaction tid False
      runIO $ throwIO err
    Right res -> do
      endTransaction tid True
      return res
  where
    startTransaction :: Ghidra Int32
    startTransaction = do
      description' :: J.String <- runIO $ Java.reflect description >>= Java.new >>= JNI.newLocalRef
      runIO $
        Java.call p "startTransaction" description'
    endTransaction :: Int32 -> Bool -> Ghidra ()
    endTransaction id commit = do
      runIO $ Java.call p "endTransaction" id commit
