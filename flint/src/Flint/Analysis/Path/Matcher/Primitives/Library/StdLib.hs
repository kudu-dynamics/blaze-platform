module Flint.Analysis.Path.Matcher.Primitives.Library.StdLib where

import Flint.Prelude hiding (Location)

import Flint.Analysis.Path.Matcher
import Flint.Types.Analysis.Path.Matcher.Func
import Flint.Types.Analysis.Path.Matcher.Primitives

import Blaze.Pil.Construct hiding (not)

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap


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
      , ("n", FuncVar $ Arg 2)
      ]
    , constraints = []
    }
  ]

memmovePrims :: [StdLibPrimitive]
memmovePrims =
  [ StdLibPrimitive
    { prim = copyPrim
    , funcName = "memmove"
    , varMapping = HashMap.fromList
      [ ("dest", FuncVar $ Arg 0)
      , ("src", FuncVar $ Arg 1)
      , ("n", FuncVar $ Arg 2)
      ]
    , constraints = []
    }
  ]


bcopyPrims :: [StdLibPrimitive]
bcopyPrims =
  [ StdLibPrimitive
    { prim = copyPrim
    , funcName = "bcopy"
    , varMapping = HashMap.fromList
      [ ("src", FuncVar $ Arg 0)
      , ("dest", FuncVar $ Arg 1)
      , ("n", FuncVar $ Arg 2)
      ]
    , constraints = []
    }
  ]

strcpyPrims :: [StdLibPrimitive]
strcpyPrims =
  [ StdLibPrimitive
    { prim = copyPrim
    , funcName = "strcpy"
    , varMapping = HashMap.fromList
      [ ("dest", FuncVar $ Arg 0)
      , ("src", FuncVar $ Arg 1)
      ]
    , constraints = []
    }
  ]

strncpyPrims :: [StdLibPrimitive]
strncpyPrims =
  [ StdLibPrimitive
    { prim = copyPrim
    , funcName = "strncpy"
    , varMapping = HashMap.fromList
      [ ("dest", FuncVar $ Arg 0)
      , ("src", FuncVar $ Arg 1)
      , ("n", FuncVar $ Arg 2)
      ]
    , constraints = []
    }
  ]


stpcpyPrims :: [StdLibPrimitive]
stpcpyPrims =
  [ StdLibPrimitive
    { prim = copyPrim
    , funcName = "stpcpy"
    , varMapping = HashMap.fromList
      [ ("dest", FuncVar $ Arg 0)
      , ("src", FuncVar $ Arg 1)
      ]
    , constraints = []
    }
  ]

stpncpyPrims :: [StdLibPrimitive]
stpncpyPrims =
  [ StdLibPrimitive
    { prim = copyPrim
    , funcName = "stpncpy"
    , varMapping = HashMap.fromList
      [ ("dest", FuncVar $ Arg 0)
      , ("src", FuncVar $ Arg 1)
      , ("n", FuncVar $ Arg 2)
      ]
    , constraints = []
    }
  ]

strdupPrims :: [StdLibPrimitive]
strdupPrims =
  [ StdLibPrimitive
    { prim = copyPrim
    , funcName = "strdup"
    , varMapping = HashMap.fromList
      [ ("dest", FuncVar Ret)
      , ("src", FuncVar $ Arg 0)
      ]
    , constraints = []
    }
  ]

strndupPrims :: [StdLibPrimitive]
strndupPrims =
  [ StdLibPrimitive
    { prim = copyPrim
    , funcName = "strndup"
    , varMapping = HashMap.fromList
      [ ("dest", FuncVar Ret)
      , ("src", FuncVar $ Arg 0)
      , ("n", FuncVar $ Arg 1)
      ]
    , constraints = []
    }
  ]


wmemcpyPrims :: [StdLibPrimitive]
wmemcpyPrims =
  [ StdLibPrimitive
    { prim = copyPrim
    , funcName = "wmemcpy"
    , varMapping = HashMap.fromList
      [ ("dest", FuncVar $ Arg 0)
      , ("src", FuncVar $ Arg 1)
      , ("n", FuncVar $ Arg 2)
      ]
    , constraints = []
    }
  ]

wmemmovePrims :: [StdLibPrimitive]
wmemmovePrims =
  [ StdLibPrimitive
    { prim = copyPrim
    , funcName = "wmemmove"
    , varMapping = HashMap.fromList
      [ ("dest", FuncVar $ Arg 0)
      , ("src", FuncVar $ Arg 1)
      , ("n", FuncVar $ Arg 2)
      ]
    , constraints = []
    }
  ]

wcscpyPrims :: [StdLibPrimitive]
wcscpyPrims =
  [ StdLibPrimitive
    { prim = copyPrim
    , funcName = "wcscpy"
    , varMapping = HashMap.fromList
      [ ("dest", FuncVar $ Arg 0)
      , ("src", FuncVar $ Arg 1)
      ]
    , constraints = []
    }
  ]

wcsncpyPrims :: [StdLibPrimitive]
wcsncpyPrims =
  [ StdLibPrimitive
    { prim = copyPrim
    , funcName = "wcsncpy"
    , varMapping = HashMap.fromList
      [ ("dest", FuncVar $ Arg 0)
      , ("src", FuncVar $ Arg 1)
      , ("n", FuncVar $ Arg 2)

      ]
    , constraints = []
    }
  ]

strlcpyPrims :: [StdLibPrimitive]
strlcpyPrims =
  [ StdLibPrimitive
    { prim = copyPrim
    , funcName = "strlcpy"
    , varMapping = HashMap.fromList
      [ ("dest", FuncVar $ Arg 0)
      , ("src", FuncVar $ Arg 1)
      , ("size", FuncVar $ Arg 2)

      ]
    , constraints = []
    }
  ]


wcslcpyPrims :: [StdLibPrimitive]
wcslcpyPrims =
  [ StdLibPrimitive
    { prim = copyPrim
    , funcName = "wcslcpy"
    , varMapping = HashMap.fromList
      [ ("dest", FuncVar $ Arg 0)
      , ("src", FuncVar $ Arg 1)
      , ("size", FuncVar $ Arg 2)

      ]
    , constraints = []
    }
  ]

memccpyPrims :: [StdLibPrimitive]
memccpyPrims =
  [ StdLibPrimitive
    { prim = copyPrim
    , funcName = "memccpy"
    , varMapping = HashMap.fromList
      [ ("dest", FuncVar $ Arg 0)
      , ("src", FuncVar $ Arg 1)
      , ("c", FuncVar $ Arg 2)
      , ("n", FuncVar $ Arg 3)
      ]
    , constraints = []
    }
  ]

