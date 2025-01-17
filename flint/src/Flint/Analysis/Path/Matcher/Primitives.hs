module Flint.Analysis.Path.Matcher.Primitives where

import Flint.Prelude hiding (Location)

import Flint.Types.Analysis.Path.Matcher (Symbol)
import qualified Flint.Types.Analysis.Path.Matcher as M
import Flint.Types.Analysis.Path.Matcher.Primitives

import Blaze.Types.Cfg.Path (PilPath)
import Blaze.Types.Function (FuncParamInfo)
import qualified Blaze.Types.Function as Func
import Blaze.Types.Pil (PilVar)
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil.Analysis (LoadExpr(LoadExpr))
import Blaze.Types.Pil.Summary (CodeSummary)

import qualified Data.HashSet as HashSet


-- | Function to check if var is argument of hosting func
type IsFuncArgPred = PilVar -> Bool

getParamName :: FuncParamInfo -> Text
getParamName (Func.FuncParamInfo x) = x ^. #name
getParamName (Func.FuncVarArgInfo x) = x ^. #name

getParamIndex :: [FuncParamInfo] -> PilVar -> Maybe Word64
getParamIndex params pv
  = headMay
  . mapMaybe (\(i, pinfo) -> bool Nothing (Just i) $ pv ^. #symbol == getParamName pinfo)
  . zip [0..]
  $ params

wrapVar :: PilVar -> Pil.Expression
wrapVar pv = Pil.Expression (coerce $ pv ^. #size) (Pil.VAR . Pil.VarOp $ pv)

-- | Returns Arg with index if var is param.
-- otherwise returns Global if var is in inputs set.
-- otherwise return Nothing.
getFuncVarFromPilVar
  :: [FuncParamInfo]
  -> HashSet PilVar
  -> PilVar
  -> Maybe FuncVar
getFuncVarFromPilVar params inputs pv = asParam <|> asGlobalVar <|> Nothing
  where
    asParam = Arg <$> getParamIndex params pv
    asGlobalVar = bool Nothing (Just . Global $ wrapVar pv) $ HashSet.member pv inputs
  

-- | If the expr is an arg, ret expr, or a global, convert to a FuncVar
getFuncVar
  :: [FuncParamInfo]
  -> CodeSummary
  -> Pil.Expression
  -> Maybe FuncVar
getFuncVar params codeSum expr@(Pil.Expression _sz exprOp) = case exprOp of
  (Pil.VAR (Pil.VarOp pv)) -> getFuncVarFromPilVar params (codeSum ^. #inputVars) pv
  (Pil.LOAD (Pil.LoadOp _)) -> case HashSet.member (LoadExpr expr) (codeSum ^. #inputLoads) of
    True -> Just . Global $ expr
    False -> Nothing
  -- TODO: do something about rets and output stores. Maybe use Effects from CodeSummary
  _ -> Nothing

-- | This converts any args from func params, globals, and the return expr (FuncVars)
-- an explicitly labels them as such.
-- It returns all the func vars that it found and changed.
toFuncVarExpr
  :: CodeSummary
  -> Pil.Expression
  -> (FuncVarExpr, HashSet FuncVar)
toFuncVarExpr expr = undefined

-- TODO: maybe we need to check to see if all parts of primitive are controllable here?
mkCallablePrimitive
  :: PrimType
  -> M.Func
  -> HashMap (Symbol Pil.Expression) Pil.Expression
  -> HashMap (Symbol Location) (HashSet Address)
  -> PilPath
  -> CallablePrimitive
mkCallablePrimitive = undefined
