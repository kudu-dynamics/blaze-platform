module Flint.Analysis.Path.Matcher.Primitives.Library.StdLib where

import Flint.Prelude hiding (Location)

import Flint.Types.Analysis.Path.Matcher.Primitives
import qualified Flint.Types.Analysis.Path.Matcher.Primitives as Prim
import qualified Flint.Analysis.Path.Matcher.Primitives.Library as PrimsLib

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap

-----------------------------------

controlledFormatStringPrims :: [StdLibPrimitive]
controlledFormatStringPrims = fmtStringFuncs >>= \(funcName, argNo) -> return $
  StdLibPrimitive
    { prim = PrimsLib.controlledFormatString
    , funcName = funcName
    , varMapping = HashMap.fromList
      [ ("fmt", FuncVar $ Arg argNo)
      ]
    , constraints = []
    }
  where
    fmtStringFuncs =
      [ ("printf", 0)
      , ("vprintf", 0)
      , ("scanf", 0)
      , ("vscanf", 0)
      , ("fprintf", 1)
      , ("sprintf", 1)
      , ("vfprintf", 1)
      , ("vsprintf", 1)
      , ("fscanf", 1)
      , ("sscanf", 1)
      , ("vfscanf", 1)
      , ("vsscanf", 1)
      , ("asprintf", 1)
      , ("vasprintf", 1)
      , ("snprintf", 2)
      , ("vsnprintf", 2)
      ]




-- -- ("dest", C.load (FuncVar $ Arg 0) (C.const 0x24 8) 8)
-- memcpyPrims :: [StdLibPrimitive]
-- memcpyPrims =
--   [ StdLibPrimitive
--     { prim = copyPrim
--     , funcName = "memcpy"
--     , varMapping = HashMap.fromList
--       [ ("dest", FuncVar $ Arg 0)
--       , ("src", FuncVar $ Arg 1)
--       , ("len", FuncVar $ Arg 2)
--       ]
--     , constraints = []
--     }
--   ]

-- memmovePrims :: [StdLibPrimitive]
-- memmovePrims =
--   [ StdLibPrimitive
--     { prim = copyPrim
--     , funcName = "memmove"
--     , varMapping = HashMap.fromList
--       [ ("dest", FuncVar $ Arg 0)
--       , ("src", FuncVar $ Arg 1)
--       , ("len", FuncVar $ Arg 2)
--       ]
--     , constraints = []
--     }
--   ]


-- bcopyPrims :: [StdLibPrimitive]
-- bcopyPrims =
--   [ StdLibPrimitive
--     { prim = copyPrim
--     , funcName = "bcopy"
--     , varMapping = HashMap.fromList
--       [ ("src", FuncVar $ Arg 0)
--       , ("dest", FuncVar $ Arg 1)
--       , ("n", FuncVar $ Arg 2)
--       ]
--     , constraints = []
--     }
--   ]

-- strcpyPrims :: [StdLibPrimitive]
-- strcpyPrims =
--   [ StdLibPrimitive
--     { prim = copyPrim
--     , funcName = "strcpy"
--     , varMapping = HashMap.fromList
--       [ ("dest", FuncVar $ Arg 0)
--       , ("src", FuncVar $ Arg 1)
--       ]
--     , constraints = []
--     }
--   ]

-- strncpyPrims :: [StdLibPrimitive]
-- strncpyPrims =
--   [ StdLibPrimitive
--     { prim = copyPrim
--     , funcName = "strncpy"
--     , varMapping = HashMap.fromList
--       [ ("dest", FuncVar $ Arg 0)
--       , ("src", FuncVar $ Arg 1)
--       , ("n", FuncVar $ Arg 2)
--       ]
--     , constraints = []
--     }
--   ]


-- stpcpyPrims :: [StdLibPrimitive]
-- stpcpyPrims =
--   [ StdLibPrimitive
--     { prim = copyPrim
--     , funcName = "stpcpy"
--     , varMapping = HashMap.fromList
--       [ ("dest", FuncVar $ Arg 0)
--       , ("src", FuncVar $ Arg 1)
--       ]
--     , constraints = []
--     }
--   ]

-- stpncpyPrims :: [StdLibPrimitive]
-- stpncpyPrims =
--   [ StdLibPrimitive
--     { prim = copyPrim
--     , funcName = "stpncpy"
--     , varMapping = HashMap.fromList
--       [ ("dest", FuncVar $ Arg 0)
--       , ("src", FuncVar $ Arg 1)
--       , ("n", FuncVar $ Arg 2)
--       ]
--     , constraints = []
--     }
--   ]

-- strdupPrims :: [StdLibPrimitive]
-- strdupPrims =
--   [ StdLibPrimitive
--     { prim = copyPrim
--     , funcName = "strdup"
--     , varMapping = HashMap.fromList
--       [ ("dest", FuncVar Prim.Ret)
--       , ("src", FuncVar $ Arg 0)
--       ]
--     , constraints = []
--     }
--   ]

-- strndupPrims :: [StdLibPrimitive]
-- strndupPrims =
--   [ StdLibPrimitive
--     { prim = copyPrim
--     , funcName = "strndup"
--     , varMapping = HashMap.fromList
--       [ ("dest", FuncVar Prim.Ret)
--       , ("src", FuncVar $ Arg 0)
--       , ("n", FuncVar $ Arg 1)
--       ]
--     , constraints = []
--     }
--   ]


-- wmemcpyPrims :: [StdLibPrimitive]
-- wmemcpyPrims =
--   [ StdLibPrimitive
--     { prim = copyPrim
--     , funcName = "wmemcpy"
--     , varMapping = HashMap.fromList
--       [ ("dest", FuncVar $ Arg 0)
--       , ("src", FuncVar $ Arg 1)
--       , ("n", FuncVar $ Arg 2)
--       ]
--     , constraints = []
--     }
--   ]

-- wmemmovePrims :: [StdLibPrimitive]
-- wmemmovePrims =
--   [ StdLibPrimitive
--     { prim = copyPrim
--     , funcName = "wmemmove"
--     , varMapping = HashMap.fromList
--       [ ("dest", FuncVar $ Arg 0)
--       , ("src", FuncVar $ Arg 1)
--       , ("n", FuncVar $ Arg 2)
--       ]
--     , constraints = []
--     }
--   ]

-- wcscpyPrims :: [StdLibPrimitive]
-- wcscpyPrims =
--   [ StdLibPrimitive
--     { prim = copyPrim
--     , funcName = "wcscpy"
--     , varMapping = HashMap.fromList
--       [ ("dest", FuncVar $ Arg 0)
--       , ("src", FuncVar $ Arg 1)
--       ]
--     , constraints = []
--     }
--   ]

-- wcsncpyPrims :: [StdLibPrimitive]
-- wcsncpyPrims =
--   [ StdLibPrimitive
--     { prim = copyPrim
--     , funcName = "wcsncpy"
--     , varMapping = HashMap.fromList
--       [ ("dest", FuncVar $ Arg 0)
--       , ("src", FuncVar $ Arg 1)
--       , ("n", FuncVar $ Arg 2)

--       ]
--     , constraints = []
--     }
--   ]

-- strlcpyPrims :: [StdLibPrimitive]
-- strlcpyPrims =
--   [ StdLibPrimitive
--     { prim = copyPrim
--     , funcName = "strlcpy"
--     , varMapping = HashMap.fromList
--       [ ("dest", FuncVar $ Arg 0)
--       , ("src", FuncVar $ Arg 1)
--       , ("size", FuncVar $ Arg 2)

--       ]
--     , constraints = []
--     }
--   ]


-- wcslcpyPrims :: [StdLibPrimitive]
-- wcslcpyPrims =
--   [ StdLibPrimitive
--     { prim = copyPrim
--     , funcName = "wcslcpy"
--     , varMapping = HashMap.fromList
--       [ ("dest", FuncVar $ Arg 0)
--       , ("src", FuncVar $ Arg 1)
--       , ("size", FuncVar $ Arg 2)

--       ]
--     , constraints = []
--     }
--   ]

-- memccpyPrims :: [StdLibPrimitive]
-- memccpyPrims =
--   [ StdLibPrimitive
--     { prim = copyPrim
--     , funcName = "memccpy"
--     , varMapping = HashMap.fromList
--       [ ("dest", FuncVar $ Arg 0)
--       , ("src", FuncVar $ Arg 1)
--       , ("c", FuncVar $ Arg 2)
--       , ("n", FuncVar $ Arg 3)
--       ]
--     , constraints = []
--     }
--   ]


