module Flint.Analysis.Path.Matcher.Primitives.Library
  ( module Flint.Types.Analysis.Path.Matcher.Primitives
  , module Flint.Analysis.Path.Matcher.Primitives
  ) where

import Flint.Prelude hiding (Location)

import Flint.Types.Analysis.Path.Matcher
import Flint.Types.Analysis.Path.Matcher.Primitives

import Blaze.Pil.Construct hiding (not)

import qualified Data.HashSet as HashSet


userControlledFormatString :: PrimType
userControlledFormatString = PrimType
  { name = "UserControlledFormatString"
  , vars = HashSet.fromList
    [ "fmtString" ]
  , locations = HashSet.fromList
    [ "call" ]
  }

isArg :: ExprPattern
isArg = Var "arg"

userControlledFormatStringPattern :: StmtPattern
userControlledFormatStringPattern = AnyOne
  [ Stmt $ Call Nothing (CallFunc $ FuncNames firstArgFuncs) firstArg
  , Stmt $ Call Nothing (CallFunc $ FuncNames secondArgFuncs) secondArg
  , Stmt $ Call Nothing (CallFunc $ FuncNames thirdArgFuncs) thirdArg
  ]
  where
    argPat = Bind "arg" isArg
    firstArg = [argPat]
    secondArg = [Wild, argPat]
    thirdArg = [Wild, Wild, argPat]
    firstArgFuncs = HashSet.fromList
      [ "printf"
      , "vprintf"
      , "scanf"
      , "vscanf"
      ]
    secondArgFuncs = HashSet.fromList
      [ "fprintf"
      , "sprintf"
      , "vfprintf"
      , "vsprintf"
      , "fscanf"
      , "sscanf"
      , "vfscanf"
      , "vsscanf"
      , "asprintf"
      , "vasprintf"
      ]
    thirdArgFuncs = HashSet.fromList
      [ "snprintf"
      , "vsnprintf"
      ]
  
writeToKernelGlobal :: PrimType
writeToKernelGlobal = PrimType
  { name = "WriteToKernelGlobal"
  , vars = HashSet.fromList
    [ "src", "dest", "len" ]
  , locations = HashSet.fromList
    [ "write" ]
  }

