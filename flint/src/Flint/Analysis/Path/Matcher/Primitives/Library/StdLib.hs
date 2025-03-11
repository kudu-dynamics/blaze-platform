module Flint.Analysis.Path.Matcher.Primitives.Library.StdLib where

import Flint.Prelude hiding (Location)

import Flint.Types.Analysis.Path.Matcher.Primitives
import qualified Flint.Analysis.Path.Matcher.Primitives.Library as PrimsLib

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
