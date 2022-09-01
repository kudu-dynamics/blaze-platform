{-# LANGUAGE DataKinds #-}
module Ghidra.Types.Pcode.Lifted where

import Ghidra.Prelude hiding (toList)

import qualified Ghidra.Types as J
import Ghidra.Variable (HighVariable, VarNode)
import Ghidra.Types.Address (AddressSpaceId, AddressSpace, Address)

type AddressSpaceMap = HashMap AddressSpaceId AddressSpace

data ParserCtx = ParserCtx
  { addressSpaces :: AddressSpaceMap
  } deriving (Eq, Ord, Show, Generic)
  

-- Lifts pcode into pure Haskell types
newtype Parser a = Parser { runLiftPcode :: ReaderT ParserCtx IO a }
  deriving newtype (Functor, Applicative, Monad)

newtype Output a = Output a
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

data Input a = Input
  { index :: Word64
  , value :: a
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

type Offset = Int64

data Destination
  = Absolute Address
  | Relative Offset
  deriving (Eq, Ord, Show, Generic)

data PcodeOp a
  = BOOL_AND (Output a) (Input a) (Input a)
  | BOOL_NEGATE (Output a) (Input a) 
  | BOOL_OR (Output a) (Input a) (Input a)
  | BOOL_XOR (Output a) (Input a) (Input a)
  | BRANCH (Input Destination)
  | BRANCHIND (Input a)
  | CALL (Input Destination) [Input a]
  | CALLIND (Input a) [Input a]
  | CALLOTHER (Input a) [Input a] -- Can't find this in the docs
  | CAST (Output a) (Input a)
  | CBRANCH (Input Destination) (Input a)
  | COPY (Output a) (Input a) (Input a)
  | CPOOLREF (Output a) (Input a) (Input a) [Input a]
  | EXTRACT (Output a) (Input a) (Input a) -- NOT in docs. guessing
  | FLOAT_ABS (Output a) (Input a)
  | FLOAT_ADD (Output a) (Input a) (Input a)
  | FLOAT_CEIL (Output a) (Input a)
  | FLOAT_DIV (Output a) (Input a) (Input a)
  | FLOAT_EQUAL (Output a) (Input a) (Input a)
  | FLOAT_FLOAT2FLOAT (Output a) (Input a)
  | FLOAT_FLOOR (Output a) (Input a)
  | FLOAT_INT2FLOAT (Output a) (Input a)
  | FLOAT_LESS (Output a) (Input a) (Input a)
  | FLOAT_LESSEQUAL (Output a) (Input a) (Input a)
  | FLOAT_MULT (Output a) (Input a) (Input a)
  | FLOAT_NAN (Output a) (Input a)
  | FLOAT_NEG (Output a) (Input a)
  | FLOAT_NOTEQUAL (Output a) (Input a) (Input a)
  | FLOAT_ROUND (Output a) (Input a)
  | FLOAT_SQRT (Output a) (Input a)
  | FLOAT_SUB (Output a) (Input a) (Input a)
  | FLOAT_TRUNC (Output a) (Input a) (Input a) -- not in docs
  | INDIRECT (Output a) (Input a) (Input a)
  | INSERT -- not in docs
  | INT_2COMP (Output a) (Input a)
  | INT_ADD (Output a) (Input a) (Input a)
  | INT_AND (Output a) (Input a) (Input a)
  | INT_CARRY (Output a) (Input a) (Input a)
  | INT_DIV (Output a) (Input a) (Input a)
  | INT_EQUAL (Output a) (Input a) (Input a)
  | INT_LEFT (Output a) (Input a) (Input a)
  | INT_LESS (Output a) (Input a) (Input a)
  | INT_LESSEQUAL (Output a) (Input a) (Input a)
  | INT_MULT (Output a) (Input a) (Input a)
  | INT_NEGATE (Output a) (Input a)
  | INT_NOTEQUAL (Output a) (Input a) (Input a)
  | INT_OR (Output a) (Input a) (Input a)
  | INT_REM (Output a) (Input a) (Input a)
  | INT_RIGHT (Output a) (Input a) (Input a)
  | INT_SBORROW (Output a) (Input a) (Input a)
  | INT_SCARRY (Output a) (Input a) (Input a)
  | INT_SDIV (Output a) (Input a) (Input a)
  | INT_SEXT (Output a) (Input a)
  | INT_SLESS (Output a) (Input a) (Input a)
  | INT_SLESSEQUAL (Output a) (Input a) (Input a)
  | INT_SREM (Output a) (Input a) (Input a)
  | INT_SRIGHT (Output a) (Input a) (Input a)
  | INT_SUB (Output a) (Input a) (Input a)
  | INT_XOR (Output a) (Input a) (Input a)
  | INT_ZEXT (Output a) (Input a)
  | LOAD (Output a) (Input AddressSpace) (Input a)
  | MULTIEQUAL (Output a) (Input a) (Input a) [Input a]
  | NEW (Output a) (Input a) [Input a]
  | PCODE_MAX -- unknown
  | PIECE (Output a) (Input a) (Input a)
  | POPCOUNT (Output a) (Input a)
  | PTRADD (Output a) (Input a) (Input a) (Input a)
  | PTRSUB (Output a) (Input a) (Input a)
  | RETURN (Input a) [Input a]
  | SEGMENTOP -- unknowng
  | STORE (Input AddressSpace) (Input a) (Input a)
  | SUBPIECE (Output a) (Input Int64) (Input a)
  | UNIMPLEMENTED
  deriving (Eq, Ord, Show, Generic)
