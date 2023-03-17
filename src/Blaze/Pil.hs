module Blaze.Pil where

import Blaze.Pil.Analysis (getAllSyms)
import Blaze.Types.Pil.Analysis (symbolGenerator)
import Blaze.Prelude hiding
  ( Symbol,
    Type,
  )
import Blaze.Types.Function
  ( Access (In, Out),
    FuncInfo,
    FuncParamInfo (FuncParamInfo, FuncVarArgInfo),
    ParamInfo (ParamInfo),
    ResultInfo (ResultInfo),
    mkFuncInfo,
  )
import qualified Blaze.Types.Function as Func
import Blaze.Types.Pil
  ( CallOp,
    Expression (Expression),
    PilVar (PilVar),
    Stmt
  )

import qualified Blaze.Types.Pil as Pil
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet

addConstToExpr :: Expression -> Int64 -> Expression
addConstToExpr expr@(Expression size _) n = Expression size addOp
  where
    addOp = Pil.ADD $ Pil.AddOp expr const'
    const' = Expression size . Pil.CONST $ Pil.ConstOp n

-- |Find the first candidate var in the list.
-- The list contains vars cons'd on as they are encountered in processing a path.
-- The var last defined is at the head of the list.
-- If none of the candidate vars appear in the list, return 'Nothing'.
getLastDefined :: [PilVar] -> HashSet PilVar -> Maybe PilVar
getLastDefined orderedVars candidateVars =
  headMay [v | v <- orderedVars, HSet.member v candidateVars]

-- |Generate store statements that correspond to any arguments used 
-- for output for a called function.
genCallOutputStores :: [FuncParamInfo] -> [Expression] -> [Stmt]
genCallOutputStores paramInfos params =
  uncurry mkStore <$> zip outArgs exprSyms
  where
    maybeOutParam :: FuncParamInfo -> Expression -> Maybe Expression
    maybeOutParam pInfo expr = do
      access <- pInfo ^? #_FuncParamInfo . #access
      if access == Func.Out
        then return expr
        else Nothing
    outArgs :: [Expression]
    outArgs = mapMaybe (uncurry maybeOutParam) . zip paramInfos $ params
    mkStore :: Expression -> Pil.Symbol -> Stmt
    mkStore argExpr freeVarSym =
      Pil.Store $
        Pil.StoreOp
          argExpr
          (Expression (argExpr ^. #size) (Pil.VAR (Pil.VarOp pv)))
      where
        pv = PilVar (fromByteBased $ argExpr ^. #size) Nothing freeVarSym
    -- TODO: Need to actually find the used defined vars and exclude them
    exprSyms :: [Pil.Symbol]
    exprSyms = symbolGenerator (getAllSyms [])

isDirectCall :: CallOp Expression -> Bool
isDirectCall c = case c ^. #dest of
  (Pil.CallAddr _) -> True
  _ -> False

-- TODO: Move to external file/module of definitions
knownFuncDefs :: HashMap Text FuncInfo
knownFuncDefs =
  HMap.fromList
    [ ( "asprintf",
        mkFuncInfo
          "asprintf"
          "asprintf"
          [ FuncParamInfo $ ParamInfo "ret" Out,
            FuncParamInfo $ ParamInfo "fmt" In,
            FuncVarArgInfo $ ParamInfo "args" In
          ]
          (ResultInfo "result")
      ),
      ( "cgc_sprintf",
        mkFuncInfo
          "cgc_sprintf"
          "cgc_sprintf"
          [ FuncParamInfo $ ParamInfo "ret" Out,
            FuncParamInfo $ ParamInfo "fmt" In,
            FuncVarArgInfo $ ParamInfo "args" In
          ]
          (ResultInfo "result")
      )
    ]
