module Haze.Checker where

import Haze.Prelude

newtype Sym = Sym Text
  deriving (Eq, Ord, Show, IsString)

data T = TInteger
       | SInt16
       | SInt32
       | SInt64
       | UInt16
       | UInt32
       | UInt64
       | TVar Sym
       | TPtr PtrLib
       deriving (Eq, Ord, Show)

data VarT t = VarT Text
  deriving (Eq, Ord, Show)

data PtrIndex = PtrIndexConst Integer
              | PtrIndexVar (VarT Integer)
              deriving (Eq, Ord, Show)

data PtrLib = PtrLib
  { ptrMap :: Map PtrIndex T
  , baseIndex :: Maybe PtrIndex
  } deriving (Eq, Ord, Show)
