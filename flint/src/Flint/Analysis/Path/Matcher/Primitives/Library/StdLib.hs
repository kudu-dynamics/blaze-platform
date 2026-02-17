module Flint.Analysis.Path.Matcher.Primitives.Library.StdLib where

import Flint.Prelude hiding (Location)

import Flint.Types.Analysis.Path.Matcher.Primitives
import qualified Flint.Analysis.Path.Matcher.Primitives.Library.PrimSpec as PrimSpec

import Blaze.Pil.Construct
import qualified Blaze.Types.Function as Func
import qualified Blaze.Types.Pil as Pil

import qualified Data.BinaryAnalysis as BA

import qualified Data.HashMap.Strict as HashMap


allStdLibPrims :: [StdLibPrimitive]
allStdLibPrims =
     controlledFormatStringPrims
  <> freeHeapPrims
  <> allocHeapPrims
  <> copyMemPrims

-----------------------------------

callExtern :: Text -> [FuncVarExpr] -> FuncVarExprSize -> FuncVarExpr
callExtern funcName args sz = FuncVarExpr sz . Pil.CALL $ Pil.CallOp dest args
  where
    dest = Pil.CallExtern $ Func.ExternFunction
      { symbol = Nothing
      , name = funcName
      , library = Nothing
      , address = Address (AddressSpace 8 1 BA.EXTERNAL) 0
      , params = fmap mkParam [0..(numArgs - 1)]
      }
    numArgs = length args
    mkParam n = Func.FuncParamInfo (Func.ParamInfo ("arg" <> show (n + 1)) (Just 8) Func.Unknown)

copyMemPrims :: [StdLibPrimitive]
copyMemPrims = 
  [ StdLibPrimitive
    { prim = PrimSpec.copyMemSpec
    , funcName = "strncpy"
    , varMapping = HashMap.fromList
      [ ("dest_ptr", FuncVar $ Arg 0)
      , ("src_ptr", FuncVar $ Arg 1)
      , ("len", let sz = SizeOf $ Arg 2 in
            callExtern
              "min"
              [ callExtern "strlen" [ FuncVar $ Arg 1 ] sz
              , FuncVar $ Arg 2 ]
              sz
        )
      ]
    , constraints = []
    }
  , StdLibPrimitive
    { prim = PrimSpec.copyMemSpec
    , funcName = "memcpy"
    , varMapping = HashMap.fromList
      [ ("dest_ptr", FuncVar $ Arg 0)
      , ("src_ptr", FuncVar $ Arg 1)
      , ("len", FuncVar $ Arg 2)
      ]
    , constraints = []
    }
  ]


freeHeapPrims :: [StdLibPrimitive]
freeHeapPrims = freeFuncs >>= \(funcName, argNo) -> return $
  StdLibPrimitive
    { prim = PrimSpec.freeHeapSpec
    , funcName = funcName
    , varMapping = HashMap.fromList
      [ ("ptr", FuncVar $ Arg argNo)
      ]
    , constraints = []
    }
  where
    freeFuncs =
      [ ("free", 0)
      , ("cfree", 0)      -- legacy, but still does heap stuff
      , ("kfree", 0)      -- kernel free
      , ("realloc", 0)    -- when called with a freed pointer, boom! TODO: look into
      , ("g_free", 0)     -- GLib wrapper around free()
      , ("sqlite3_free", 0) -- SQLite-managed heap, sometimes same as system free()
      , ("XFree", 0)      -- X11 heap-ish, platform-allocator dependent
      , ("xmlFree", 0)    -- if xmlMalloc == malloc, then same danger
      ]

allocHeapPrims :: [StdLibPrimitive]
allocHeapPrims =
  [ StdLibPrimitive
    { prim = PrimSpec.allocHeapSpec
    , funcName = "malloc"
    , varMapping = HashMap.fromList
      [ ("ptr", FuncVar Ret)
      , ("size", FuncVar $ Arg 0)
      ]
    , constraints = []
    }
  , StdLibPrimitive
    { prim = PrimSpec.allocHeapSpec
    , funcName = "calloc"
    , varMapping = HashMap.fromList
      [ ("ptr", FuncVar Ret)
      , ("size", mul (FuncVar $ Arg 0) (FuncVar $ Arg 1) (SizeOf $ Arg 0))
      ]
    , constraints = []
    }
  ]

controlledFormatStringPrims :: [StdLibPrimitive]
controlledFormatStringPrims = fmtStringFuncs >>= \(funcName, argNo) -> return $
  StdLibPrimitive
    { prim = PrimSpec.controlledFormatStringSpec
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
