{- HLINT ignore "Evaluate" -}

module Helper.Primitives where

import Flint.Prelude hiding (const)

import qualified Flint.Analysis.Path.Matcher.Primitives as Prim
import Flint.Analysis.Path.Matcher.Primitives
import qualified Flint.Types.Analysis.Path.Matcher.Func as M

import Blaze.Pil.Construct
import Blaze.Pil.Summary (CodeSummary)
import qualified Blaze.Pil.Summary as Summary
import Blaze.Types.Function (Function(Function), FuncParamInfo)
import qualified Blaze.Types.Function as Func
import Blaze.Types.Pil (Stmt)
import qualified Blaze.Types.Pil as Pil

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet


memcpy :: Function
memcpy = Function Nothing "memcpy" 0x111 []

sscanf :: Function
sscanf = Function Nothing "sscanf" 0x222 []


fooParams :: [FuncParamInfo]
fooParams = [ Func.FuncParamInfo $ Func.ParamInfo "arg1" Func.Unknown
            , Func.FuncParamInfo $ Func.ParamInfo "arg2" Func.Unknown
            ]
            
foo :: Function
foo = Function Nothing "foo" 0x999 fooParams

fooPath1 :: [Stmt]
fooPath1 =
  [ defCall "r" (Pil.CallFunc sscanf)
    [ constPtr 0x3234 8
    , load (add (var "arg1" 8) (const 4 8) 8) 8
    ]
    8
  , ret $ var "r" 8
  ]

fooPath2 :: [Stmt]
fooPath2 =
  [ defCall "r" (Pil.CallFunc sscanf)
    [ constPtr 0x3234 8
    , load (add (var "global1" 8) (const 4 8) 8) 8
    ]
    8
  , ret $ var "r" 8
  ]

fooPath3 :: [Stmt]
fooPath3 =
  [ constraint $ cmpNE (var "arg2" 8) (const 0 8) 8
  , defCall "r" (Pil.CallFunc memcpy)
    [ var "global1" 8
    , load (add (var "arg1" 8) (const 4 8) 8) 8
    ]
    8
  , ret $ var "r" 8
  ]

fooCodeSummary1 :: CodeSummary
fooCodeSummary1 = Summary.fromStmts fooPath1

fooCodeSummary2 :: CodeSummary
fooCodeSummary2 = Summary.fromStmts fooPath2

fooCodeSummary3 :: CodeSummary
fooCodeSummary3 = Summary.fromStmts fooPath3

copyPrim :: PrimType
copyPrim = PrimType
  { name = "copy"
  , vars = HashSet.fromList ["dest", "src"]
  , locations = HashSet.fromList ["write"]
  }

globalFuncVar :: FuncVar
globalFuncVar = Prim.Global $ var "global1" 8

fooCallablePrimitive3 :: CallablePrimitive
fooCallablePrimitive3 = Prim.CallablePrimitive
  { prim = copyPrim
  , func = foo
  , callDest = M.FuncName "foo"
  , varMapping = HashMap.fromList
    [ ( "src"
      , ( load (add (FuncVar $ Prim.Arg 0) (const 4 8) 8) 8
        , HashSet.fromList [Prim.Arg 0]
        )
      )
    , ("dest"
      , ( FuncVar globalFuncVar
        , HashSet.fromList [globalFuncVar]
        )
      )
    ]
  , constraints =
      [ ( cmpNE (FuncVar $ Prim.Arg 1) (const 0 8) 8
        , HashSet.fromList [Prim.Arg 1]
        )
      ]
  , locations = HashMap.fromList
                [ ("write", HashSet.singleton 0x1234) ]

  , linkedVars = HashSet.fromList
                 [ Prim.Arg 0
                 , Prim.Arg 1
                 , globalFuncVar
                 ]
  }


barParams :: [FuncParamInfo]
barParams = [ Func.FuncParamInfo $ Func.ParamInfo "arg1" Func.Unknown
            , Func.FuncParamInfo $ Func.ParamInfo "arg2" Func.Unknown
            , Func.FuncParamInfo $ Func.ParamInfo "arg3" Func.Unknown
            , Func.FuncParamInfo $ Func.ParamInfo "arg4" Func.Unknown
            ]
            
bar :: Function
bar = Function Nothing "bar" 0x777 barParams

barPath3 :: [Stmt]
barPath3 =
  [ constraint $ cmpSgt (var "arg1" 8) (const 0 8) 8
  , defCall "r" (Pil.CallFunc foo)
    [ (var "arg4" 8)
    , (load (var "arg2" 8) 8)
    ]
    8
  , ret $ var "r" 8
  ]

barCodeSummary3 :: CodeSummary
barCodeSummary3 = Summary.fromStmts barPath3
