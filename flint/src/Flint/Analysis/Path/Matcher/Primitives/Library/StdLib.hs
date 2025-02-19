module Flint.Analysis.Path.Matcher.Primitives.Library.StdLib where

import Flint.Prelude hiding (Location)

import Flint.Analysis.Path.Matcher
import Flint.Types.Analysis.Path.Matcher.Func
import Flint.Types.Analysis.Path.Matcher.Primitives

import Blaze.Pil.Construct hiding (not)

import qualified Data.HashSet as HashSet


controlledFormatString :: PrimType
controlledFormatString = PrimType
  { name = "controlledFormatString"
  , vars = HashSet.fromList
    [ "fmtString" ]
  , locations = HashSet.fromList
    [ "call" ]
  }

copyPrim :: PrimType
copyPrim = PrimType
  { name = "copy"
  , vars = HashSet.fromList ["dest", "src", "len"]
  , locations = HashSet.fromList ["write"]
  }


-----------------------------------

  -- [ Primitive "copy_" copyPrim 
  -- , Assert "copy_len"
  -- , Call Nothing (FuncName "memcpy") [WIld, WIld, (Not Const)]
  -- ]

-- ("dest", C.load (FuncVar $ Arg 0) (C.const 0x24 8) 8)
memcpyPrims :: [StdLibPrimitive]
memcpyPrims =
  [ StdLibPrimitive
    { prim = copyPrim
    , funcName = "memcpy"
    , varMapping = HashMap.fromList
      [ ("dest", FuncVar $ Arg 0)
      , ("src", FuncVar $ Arg 1)
      , ("len", FuncVar $ Arg 2)
      ]
    , constraints = []
    }
  ]
