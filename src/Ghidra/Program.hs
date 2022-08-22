{-# LANGUAGE DataKinds #-}
module Ghidra.Program where

import Ghidra.Prelude hiding (force)

import qualified Language.Java as Java
import qualified Data.BinaryAnalysis as BA
import Language.Java (J)
import Ghidra.Types

type Program = J ('Java.Class "ghidra.program.database.ProgramDB")

type AddressFactory = J ('Java.Class "ghidra.program.model.address.AddressFactory")

getAddressFactory :: Program -> IO AddressFactory
getAddressFactory p = Java.call p "getAddressFactory"

-- | Returns an address from the constant space of the program
getConstantAddress :: AddressFactory -> BA.Address -> IO Address
getConstantAddress gen off = Java.call gen "getConstantAddress" (fromIntegral off :: Int64)
