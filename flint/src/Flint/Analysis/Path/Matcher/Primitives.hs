module Flint.Analysis.Path.Matcher.Primitives
  ( module Flint.Types.Analysis.Path.Matcher.Primitives
  , module Flint.Analysis.Path.Matcher.Primitives
  ) where

import Flint.Prelude hiding (Location)

import Flint.Types.Analysis.Path.Matcher (addCallablePrimitive_)
import qualified Flint.Types.Analysis.Path.Matcher.Func as MFunc
import Flint.Types.Analysis.Path.Matcher.Primitives
import Flint.Types.Symbol (Symbol)

import Blaze.Types.Function (Function, FuncParamInfo)
import qualified Blaze.Types.Function as Func
import Blaze.Types.Pil (PilVar, Stmt)
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil.Summary (CodeSummary)

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap


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
getFuncVar params codeSum _expr@(Pil.Expression _sz exprOp) = case exprOp of
  (Pil.VAR (Pil.VarOp pv)) -> getFuncVarFromPilVar params (codeSum ^. #inputVars) pv
  -- (Pil.LOAD (Pil.LoadOp _)) -> case HashSet.member (LoadExpr expr) (codeSum ^. #inputLoads) of
  --   True -> Just . Global $ expr
  --   False -> Nothing
  -- TODO: do something about rets and output stores. Maybe use Effects from CodeSummary
  _ -> Nothing

-- | This converts any args from func params, globals, and the return expr (FuncVars)
-- and explicitly labels them as such.
-- It returns all the func vars that it found and changed.
toFuncVarExpr
  :: [FuncParamInfo]
  -> CodeSummary
  -> Pil.Expression
  -> (FuncVarExpr, HashSet FuncVar)
toFuncVarExpr params codeSum expr' = runState (f expr') HashSet.empty
  where
    addFuncVar v = modify (HashSet.insert v)
    
    f :: Pil.Expression -> State (HashSet FuncVar) FuncVarExpr
    f expr@(Pil.Expression sz exprOp) =
      case getFuncVar params codeSum expr of
        Nothing -> FuncVarExpr sz <$> traverse f exprOp
        Just fvar -> do
          addFuncVar fvar
          return $ FuncVar fvar

-- | Makes a CallablePrimitive from a path that goes through a func in the binary.
-- TODO: maybe we need to check to see if all parts of primitive are controllable here?
mkCallablePrimitive
  :: Function
  -> CodeSummary
  -> PrimType
  -> HashMap (Symbol Pil.Expression) Pil.Expression
  -> HashMap (Symbol Address) (HashSet Address)
  -> [Stmt] -- whole path
  -> CallablePrimitive
mkCallablePrimitive func codeSum primType boundExprs boundLocations path = CallablePrimitive
  { prim = primType
  , func = func
  , callDest = MFunc.FuncName $ func ^. #name
  , varMapping = varMapping'
  -- TODO: derive from path.
  -- could get all constraints out of path, or just up until last boundLocation is reached
  , constraints = funcVarExprConstraints
  , locations = boundLocations
  , linkedVars =
      foldMap snd funcVarExprConstraints
      <>
      (foldMap snd . HashMap.elems $ varMapping')
  }
  where
    funcVarExprConstraints = toFuncVarExpr' <$> allConstraints
    allConstraints = mapMaybe extractConstraint path
    
    extractConstraint :: Stmt -> Maybe Pil.Expression
    extractConstraint stmt = case stmt ^. #statement of
      (Pil.Constraint (Pil.ConstraintOp x)) -> Just x
      (Pil.BranchCond (Pil.BranchCondOp x)) -> Just x
      _ -> Nothing

    params = func ^. #params
    toFuncVarExpr' = toFuncVarExpr params codeSum
    varMapping' :: HashMap (Symbol Pil.Expression) (FuncVarExpr, HashSet FuncVar)
    varMapping' = toFuncVarExpr' <$> boundExprs


fromStdLibPrimitive :: StdLibPrimitive -> Function -> CallablePrimitive
fromStdLibPrimitive x func = CallablePrimitive
  { prim = x ^. #prim
  , func = func
  , callDest = MFunc.FuncName $ func ^. #name
  , varMapping = varMapping'
  , constraints = constraints'
  , locations = HashMap.fromList
                . fmap (toSnd . const . HashSet.singleton $ func ^. #address)
                . HashSet.toList
                $ x ^. #prim . #locations
  , linkedVars = HashSet.unions
    $ fmap snd constraints'
    <> fmap (snd . snd) (HashMap.toList varMapping')
  }
  where
    varMapping' = toSnd extractFuncVars <$> x ^. #varMapping
    constraints' = toSnd extractFuncVars <$> x ^. #constraints


getInitialPrimitivesForFunc :: Function -> [StdLibPrimitive] -> [CallablePrimitive]
getInitialPrimitivesForFunc func = mapMaybe f
  where
    f sprim = if (func ^. #name == sprim ^. #funcName)
      then Just $ fromStdLibPrimitive sprim func
      else Nothing
      
getInitialPrimitives
  :: [StdLibPrimitive]
  -> [Function]
  -> HashMap PrimType (HashSet CallablePrimitive)
getInitialPrimitives sprims
  = foldr addCallablePrimitive_ HashMap.empty
  . foldMap (`getInitialPrimitivesForFunc` sprims)
