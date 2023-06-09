{-# LANGUAGE DataKinds #-}
module Ghidra.Program (
  module Ghidra.Program,
) where

import Ghidra.Prelude hiding (force)

import Control.Lens (_Wrapped')
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
  mkAddressSpace,
 )
import Ghidra.Register (Register)
import Ghidra.Register qualified as Reg
import Ghidra.Types qualified as J
import Ghidra.Util (isJNull)

getAddressFactory :: J.ProgramDB -> IO J.AddressFactory
getAddressFactory p = Java.call p "getAddressFactory" >>= JNI.newGlobalRef

-- | Returns an address from the constant space of the program.
getConstantAddress :: J.AddressFactory -> BA.Address -> IO J.Address
getConstantAddress gen off = Java.call gen "getConstantAddress" (fromIntegral off :: Int64) >>= JNI.newGlobalRef

getAddressSpaceMap :: J.ProgramDB -> IO AddressSpaceMap
getAddressSpaceMap p = do
  af <-  getAddressFactory p
  spaces :: [J.AddressSpace] <- Java.call af "getAllAddressSpaces"
                                >>= Java.reify
                                >>= traverse JNI.newGlobalRef
  HashMap.fromList <$> traverse f spaces
  where
    f :: J.AddressSpace -> IO (AddressSpaceId, AddressSpace)
    f x = do
      id <- AddressSpaceId <$> Java.call x "getSpaceID"
      (id,) <$> mkAddressSpace x

getExecutableMD5 :: J.ProgramDB -> IO Text
getExecutableMD5 p = Java.call p "getExecutableMD5"
                     >>= Java.reify

-- | Given a program DB, address, and size, provide the associated register.
getRegister :: J.ProgramDB -> Address -> Int -> IO (Maybe Register)
getRegister p addr size = do
  jAddrFactory <- getAddressFactory p
  jAddr <-
    getAddress
      jAddrFactory
      (addr ^. #space . #id . _Wrapped')
      (addr ^. #offset)
  let size' :: Int32 = fromIntegral size
  reg :: J.Register <-
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
