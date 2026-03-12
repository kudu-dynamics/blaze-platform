module Flint.Analysis.Path.Matcher.Primitives
  ( module Flint.Types.Analysis.Path.Matcher.Primitives
  , module Flint.Analysis.Path.Matcher.Primitives
  ) where

import Flint.Prelude hiding (Location)

import Flint.Types.Analysis.Path.Matcher (addCallableWMI_)
import qualified Flint.Types.Analysis.Path.Matcher.Func as MFunc
import Flint.Types.Analysis.Path.Matcher.Primitives
import Flint.Types.Symbol (Symbol)

import Blaze.Types.Function (ExternFunction, FuncParamInfo, Func, _name, _params)
import qualified Blaze.Types.Function as Func
import Blaze.Types.Pil (PilVar, Stmt)
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil.Summary (CodeSummary)

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Data.List (foldl1')

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
-- otherwise return Nothing.
getFuncVarFromPilVar :: [FuncParamInfo] -> PilVar -> Maybe FuncVar
getFuncVarFromPilVar params pv = Arg <$> getParamIndex params pv

-- | If the expr is an arg, ret expr, or a global, convert to a FuncVar
getFuncVar
  :: [FuncParamInfo]
  -> CodeSummary
  -> Pil.Expression
  -> Maybe FuncVar
getFuncVar params codeSum expr@(Pil.Expression _ exprOp) = isArgOrGlobal <|> isRet
  where
    isArgOrGlobal = case exprOp of
      (Pil.VAR (Pil.VarOp pv)) -> getFuncVarFromPilVar params pv
      (Pil.GLOBAL_PTR _) -> Just $ Global expr
      _ -> Nothing
    isRet = bool Nothing (Just Ret) . HashSet.member expr $ codeSum ^. #results

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
        Nothing -> FuncVarExpr (ConstSize sz) <$> traverse f exprOp
        Just fvar -> do
          addFuncVar fvar
          return $ FuncVar fvar

-- | Makes a CallableWMI from a path that goes through a func in the binary.
-- TODO: maybe we need to check to see if all parts of primitive are controllable here?
mkCallableWMI
  :: Func
  -> CodeSummary
  -> PrimSpec
  -> HashMap (Symbol Pil.Expression) Pil.Expression
  -> HashMap (Symbol Address) (Either ExternFunction Address)
  -> [Stmt] -- whole path
  -> Either MkCallableWMIError CallableWMI
mkCallableWMI func codeSum primSpec boundExprs' boundLocations path = do
  let boundExprs = HashMap.filterWithKey (\k _ -> HashSet.member k $ primSpec ^. #vars) boundExprs'
      keySet = HashSet.fromList . HashMap.keys
      missingVarKeys = HashSet.difference (primSpec ^. #vars) $ keySet boundExprs
      missingLocationKeys = HashSet.difference (primSpec ^. #locations) $ keySet boundLocations
  case not (HashSet.null missingVarKeys) || not (HashSet.null missingLocationKeys) of
    True -> Left $ MkCallableWMIError primSpec missingVarKeys missingLocationKeys
    False -> Right $ CallableWMI
      { prim = primSpec
      , func = func
      , callDest = MFunc.FuncName $ func ^. _name
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

        params = func ^. _params
        toFuncVarExpr' = toFuncVarExpr params codeSum
        varMapping' :: HashMap (Symbol Pil.Expression) (FuncVarExpr, HashSet FuncVar)
        varMapping' = toFuncVarExpr' <$> boundExprs

-- | Squash together CallableWMIs that share the same location and varMapping
squashCallableWMIs :: HashSet CallableWMI -> HashSet CallableWMI
squashCallableWMIs wmis = HashSet.fromList . concatMap squashGroup . HashMap.elems $ grouped
  where
    grouped = HashMap.fromListWith (++)
      . fmap (\wmi -> ((wmi ^. #locations, wmi ^. #varMapping), [wmi]))
      $ HashSet.toList wmis

    squashGroup [] = []
    squashGroup [x] = [x]
    squashGroup xs@(firstWMI:_) =
      let commonConstraints = foldl1' HashSet.intersection
            $ fmap (HashSet.fromList . view #constraints) xs
      in [firstWMI & #constraints .~ HashSet.toList commonConstraints]

locationFromFunc :: Func -> Either ExternFunction Address
locationFromFunc (Func.External x) = Left x
locationFromFunc (Func.Internal x) = Right $ x ^. #address

fromKnownFunc :: KnownFunc -> Func -> CallableWMI
fromKnownFunc x func = CallableWMI
  { prim = x ^. #prim
  , func = func
  , callDest = MFunc.FuncName $ func ^. _name
  , varMapping = varMapping'
  , constraints = constraints'
  , locations = HashMap.fromList
                . fmap (toSnd . const . locationFromFunc $ func)
                . HashSet.toList
                $ x ^. #prim . #locations
  , linkedVars = HashSet.unions
    $ fmap snd constraints'
    <> fmap (snd . snd) (HashMap.toList varMapping')
  }
  where
    varMapping' = toSnd extractFuncVars <$> x ^. #varMapping
    constraints' = toSnd extractFuncVars <$> x ^. #constraints


getInitialWMIsForFunc :: Func -> [KnownFunc] -> [CallableWMI]
getInitialWMIsForFunc func = mapMaybe f
  where
    f sprim = if cleanFuncName (func ^. _name) == sprim ^. #funcName
      then Just $ fromKnownFunc sprim func
      else Nothing

cleanFuncName :: Text -> Text
cleanFuncName = Text.dropWhile (== '_')

getInitialWMIs
  :: [KnownFunc]
  -> [Func]
  -> HashMap (PrimSpec, Func) (HashSet CallableWMI)
getInitialWMIs sprims
  = foldr addCallableWMI_ HashMap.empty
  . foldMap (`getInitialWMIsForFunc` sprims)
