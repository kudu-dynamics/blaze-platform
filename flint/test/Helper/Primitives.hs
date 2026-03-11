{- HLINT ignore "Evaluate" -}

module Helper.Primitives where

import Flint.Prelude hiding (const, sym)

import qualified Flint.Analysis.Path.Matcher.Primitives as Prim
import Flint.Analysis.Path.Matcher.Primitives
import qualified Flint.Types.Analysis.Path.Matcher.Func as M

import Blaze.Pil.Construct
import Blaze.Pil.Summary (CodeSummary)
import qualified Blaze.Pil.Summary as Summary
import Blaze.Types.Function (Function(Function), FuncParamInfo(FuncParamInfo), ParamInfo(ParamInfo))
import qualified Blaze.Types.Function as Func
import Blaze.Types.Pil (Stmt)
import qualified Blaze.Types.Pil as Pil

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet


var_
  :: (GetExprSize attrs, ExprConstructor attrs expr)
  => Function
  -> Pil.Symbol
  -> attrs
  -> expr
var_ func sym attrs = mkExpr attrs (Pil.VAR . Pil.VarOp $ pilVar_ (getPilVarSize attrs) (Just ctx) sym)
  where
    ctx = Pil.Ctx func 0

memcpy :: Function
memcpy = Function Nothing "memcpy" (intToAddr 0x111)
  [ FuncParamInfo $ ParamInfo "dest" Nothing Func.Out
  , FuncParamInfo $ ParamInfo "src" Nothing Func.In
  , FuncParamInfo $ ParamInfo "n" Nothing Func.In
  ]

memcpyPrims :: [KnownFunc]
memcpyPrims =
  [ KnownFunc
    { prim = copyPrim
    , funcName = "memcpy"
    , varMapping = HashMap.fromList
      [ ("dest", FuncVar $ Arg 0)
      , ("src", FuncVar $ Arg 1)
      ]
    , constraints = []
    }
  ]

sscanf :: Function
sscanf = Function Nothing "sscanf" (intToAddr 0x222)
  [ FuncParamInfo $ ParamInfo "str" Nothing Func.In
  , FuncParamInfo $ ParamInfo "format" Nothing Func.In
  ]

sscanfPrims :: [KnownFunc]
sscanfPrims =
  [ KnownFunc
    { prim = controlledFormatStringPrim
    , funcName = "sscanf"
    , varMapping = HashMap.fromList
      [ ("fmt", FuncVar $ Arg 1)
      ]
    , constraints = []
    }
  ]

strdupPrims :: [KnownFunc]
strdupPrims =
  [ KnownFunc
    { prim = copyPrim
    , funcName = "strdup"
    , varMapping = HashMap.fromList
      [ ("dest", FuncVar Ret)
      , ("src", FuncVar $ Arg 0)
      ]
    , constraints = []
    }
  ]

printf :: Function
printf = Function Nothing "printf" (intToAddr 0x8888)
  [ FuncParamInfo $ ParamInfo "format" Nothing Func.In
  ]

printfPrims :: [KnownFunc]
printfPrims =
  [ KnownFunc
    { prim = controlledFormatStringPrim
    , funcName = "printf"
    , varMapping = HashMap.fromList
      [ ("fmt", FuncVar $ Arg 0)
      ]
    , constraints = []
    }
  ]


fooParams :: [FuncParamInfo]
fooParams = [ Func.FuncParamInfo $ Func.ParamInfo "arg1" Nothing Func.Unknown
            , Func.FuncParamInfo $ Func.ParamInfo "arg2" Nothing Func.Unknown
            ]
            
foo :: Function
foo = Function Nothing "foo" (intToAddr 0x999) fooParams

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

copyPrim :: PrimSpec
copyPrim = PrimSpec
  { name = "copy"
  , vars = HashSet.fromList ["dest", "src"]
  , locations = HashSet.fromList ["write"]
  }

controlledFormatStringPrim :: PrimSpec
controlledFormatStringPrim = PrimSpec
  { name = "controlledFormatString"
  , vars = HashSet.fromList ["fmt"]
  , locations = HashSet.fromList ["usage"]
  }


globalVar :: FuncVarExpr
globalVar = var' (pilVar 8 "global1") $ ConstSize 8

fooCallableWMI3 :: CallableWMI
fooCallableWMI3 = Prim.CallableWMI
  { prim = copyPrim
  , func = Func.Internal foo
  , callDest = M.FuncName "foo"
  , varMapping = HashMap.fromList
    [ ( "src"
      , ( load (add (FuncVar $ Prim.Arg 0) (const 4 (ConstSize 8)) (ConstSize 8)) (ConstSize 8)
        , HashSet.fromList [Prim.Arg 0]
        )
      )
    , ("dest"
      , ( globalVar
        , HashSet.empty
        )
      )
    ]
  , constraints =
      [ ( cmpNE (FuncVar $ Prim.Arg 1) (const 0 (ConstSize 8)) (ConstSize 8)
        , HashSet.fromList [Prim.Arg 1]
        )
      ]
  , locations = HashMap.fromList
                [ ("write", Right (intToAddr 0x1234)) ]

  , linkedVars = HashSet.fromList
                 [ Prim.Arg 0
                 , Prim.Arg 1
                 ]
  }


barParams :: [FuncParamInfo]
barParams = [ Func.FuncParamInfo $ Func.ParamInfo "arg1" Nothing Func.Unknown
            , Func.FuncParamInfo $ Func.ParamInfo "arg2" Nothing Func.Unknown
            , Func.FuncParamInfo $ Func.ParamInfo "arg3" Nothing Func.Unknown
            , Func.FuncParamInfo $ Func.ParamInfo "arg4" Nothing Func.Unknown
            ]
            
bar :: Function
bar = Function Nothing "bar" (intToAddr 0x777) barParams

barPath3 :: [Stmt]
barPath3 =
  [ constraint $ cmpSgt (var_ bar "arg1" 8) (const 0 8) 8
  , defCall "r" (Pil.CallFunc foo)
    [ var_ bar "arg4" 8
    , load (var_ bar "arg2" 8) 8
    ]
    8
  , ret $ var_ bar "r" 8
  ]

barCodeSummary3 :: CodeSummary
barCodeSummary3 = Summary.fromStmts barPath3
