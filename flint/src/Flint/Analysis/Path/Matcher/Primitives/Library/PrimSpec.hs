module Flint.Analysis.Path.Matcher.Primitives.Library.PrimSpec where

import Flint.Types.Analysis.Path.Matcher.Primitives

import qualified Data.HashSet as HashSet


copyPrim :: PrimSpec
copyPrim = PrimSpec
  { name = "copy"
  , vars = HashSet.fromList ["dest", "src", "len"]
  , locations = HashSet.fromList ["write"]
  }

freeHeap :: PrimSpec
freeHeap = PrimSpec
  { name = "freeHeap"
  , vars = HashSet.fromList ["ptr"]
  , locations = HashSet.fromList ["free"]
  }

allocHeap :: PrimSpec
allocHeap = PrimSpec
  { name = "allocHeap"
  , vars = HashSet.fromList ["ptr", "size"]
  , locations = HashSet.fromList ["alloc"]
  }

doubleFree :: PrimSpec
doubleFree = PrimSpec
  { name = "doubleFree"
  , vars = HashSet.fromList ["ptr"]
  , locations = HashSet.fromList ["free1", "free2"]
  }

controlledFormatString :: PrimSpec
controlledFormatString = PrimSpec
  { name = "ControlledFormatString"
  , vars = HashSet.fromList
    [ "fmt" ]
  , locations = HashSet.fromList
    [ "call" ]
  }

-- | [x] = [x] + 1
-- without previously checking [x]
integerOverflow :: PrimSpec
integerOverflow = PrimSpec
  { name = "IntegerOverflow"
  , vars = HashSet.fromList
    [ "ptr" -- overflowed ptr
    , "n"
    ]
  , locations = HashSet.fromList
    [ "increment store" ]
  }
