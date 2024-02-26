{-# LANGUAGE ViewPatterns #-}

module Blaze.Pil.Summary (
  module Blaze.Pil.Summary,
  module Blaze.Types.Pil.Summary,
) where

import Blaze.Pil (
  ConstOp (ConstOp),
  ExprOp (CONST, LOAD, VAR),
  Expression (Expression),
  LoadOp (LoadOp),
  PilVar,
  RetOp (RetOp),
  Statement (Ret, Store),
  Stmt,
  StoreOp (StoreOp),
  VarOp (VarOp),
  mkCallStatement,
 )
import Blaze.Pil.Analysis (DefLoadStmt (DefLoadStmt), LoadExpr (LoadExpr), LoadStmt (LoadStmt), findLoads, getFreeVars, mkDefLoadStmt, mkStoreStmt)
import Blaze.Prelude
import Blaze.Types.Pil.Summary
import Data.HashSet qualified as HashSet

-- TODO: Names of specific allocation and deallocation functions should be
--       relocated to the application using this summarization.
writeFuncs :: HashSet Text
writeFuncs =
  HashSet.fromList
    [ "CopyMem"
    , "InternalMemCopyMem"
    , "memcpy"
    , "SetMem"
    , "SetMem.part.0"

    ]

allocFuncs :: HashSet Text
allocFuncs =
  HashSet.fromList
    [ "AllocatePool"
    , "malloc"
    ]

deallocFuncs :: HashSet Text
deallocFuncs =
  HashSet.fromList
    [ "free"
    , "FreePool"
    ]

isArgLoad :: HashSet PilVar -> LoadExpr -> Bool
isArgLoad inputArgs (LoadExpr (Expression _ (LOAD (LoadOp (Expression _ (VAR (VarOp var))))))) =
  HashSet.member var inputArgs
isArgLoad _ _ = False

isStackLocalLoad :: LoadExpr -> Bool
isStackLocalLoad load =
  isJust $
    load ^? #expr . #op . _Ctor @"LOAD" . #src . #op . _Ctor @"STACK_LOCAL_ADDR"

getRetVal :: Stmt -> Maybe Expression
getRetVal (Ret (RetOp x)) = Just x
getRetVal _ = Nothing

-- | Given a list of function parameters, a list of returned values, and a
-- trace, find all copies the trace performs. A copy is a read plus a write. A
-- read is from a user-supplied pointer, a fixed address, or a pure expression.
-- A write is to a user-supplied pointer, a fixed address, or a function return
-- argument. The degenerate case of a pure expression input plus a return
-- parameter output is excluded
findCopies :: [PilVar] -> [Expression] -> [Stmt] -> [Capability]
findCopies inputVars retVals stmts = do
  -- [(y, x) | stmts = [...; y = LOAD x; ...]] ++ [Nothing (representing the input arg case)]
  mayYX <- (Just <$> getInputs stmts) <> [Nothing]
  -- [(z, y) | stmts = [...; STORE z y; ...] OR stmts = [...; RET y]]
  (mayZ, yExpr) <- (getStores stmts <&> \case (StoreOp z y') -> (Just z, y')) <> ((Nothing,) <$> retVals)
  guard $ case mayYX of
    Just (y, _) -> yExpr ^? #op . #_VAR . #src == Just y
    _ -> True
  inputLocation <-
    toList $ case mayYX of
      Just (_, Expression _ (CONST (ConstOp (fromIntegral -> x :: Address)))) -> Just $ ConcreteInputLocation x
      Just (_, Expression _ (VAR (VarOp x))) -> Just $ SymbolicInputLocation x
      Nothing ->
        -- if yExpr is a VAR, skip it, because it should be handled by the other two input types
        case yExpr of
          Expression _ (VAR _) -> Nothing
          _ -> Just $ PureExpression yExpr
      _ -> Nothing
  outputLocation <-
    toList $ case mayZ of
      Just (Expression _ (CONST (ConstOp (fromIntegral -> zAddr :: Address)))) ->
        Just $ ConcreteOutputLocation zAddr
      Just (Expression _ (VAR (VarOp zVar))) | zVar `elem` inputVars ->
        Just $ SymbolicOutputLocation zVar
      Nothing | yExpr `elem` retVals -> Just Returned
      _ -> Nothing
  -- remove degenerate case that just says we return a pure (non-inputted) value
  guard $ case (inputLocation, outputLocation) of
    (PureExpression _, Returned) -> False
    _ -> True
  pure $ CopyCapability outputLocation inputLocation
  where
    getInputs :: [Stmt] -> [(PilVar, Expression)]
    getInputs =
      fmap (\(DefLoadStmt pv (LoadStmt _ (LoadExpr e) _ _)) -> (pv, e ^?! #op . #_LOAD . #src)) . mapMaybe (uncurry mkDefLoadStmt) . zip [0..]
    getStores :: [Stmt] -> [StoreOp Expression]
    getStores = fmap (view #op) . mapMaybe (uncurry mkStoreStmt) . zip [0..]


extractCapabilities :: [PilVar] -> [Expression] -> [Stmt] -> [Capability]
extractCapabilities inputVars retVals stmts =
  [ findCopies inputVars retVals
  ]
  >>= ($ stmts)

mkEffect ::
  (Stmt -> Maybe Effect) ->
  (Stmt -> Maybe Effect) ->
  (Stmt -> Maybe Effect) ->
  (Stmt -> Maybe Effect) ->
  Stmt ->
  Maybe Effect
mkEffect parseWrite parseAlloc parseDealloc parseCall stmt =
  msum $ fmap ($ stmt) [parseWrite, parseAlloc, parseDealloc, parseCall]

mkWrite :: Stmt -> Maybe Effect
mkWrite stmt@(Store _) = Just $ EffectWrite stmt
mkWrite stmt = do
  callStmt <- mkCallStatement stmt
  funcName <- callStmt ^. #callOp . #name
  if HashSet.member funcName writeFuncs
    then Just $ EffectWrite stmt
    else Nothing

mkAlloc :: Stmt -> Maybe Effect
mkAlloc stmt = do
  callStmt <- mkCallStatement stmt
  funcName <- callStmt ^. #callOp . #name
  if HashSet.member funcName allocFuncs
    then Just $ EffectAlloc stmt
    else Nothing

mkDealloc :: Stmt -> Maybe Effect
mkDealloc stmt = do
  callStmt <- mkCallStatement stmt
  funcName <- callStmt ^. #callOp . #name
  if HashSet.member funcName deallocFuncs
    then Just $ EffectDealloc stmt
    else Nothing

mkCall :: Stmt -> Maybe Effect
mkCall stmt = do
  callStmt <- mkCallStatement stmt
  return $ EffectCall $ callStmt ^. #stmt

fromStmts :: [Stmt] -> CodeSummary
fromStmts stmts =
  let inputVars = HashSet.toList $ getFreeVars stmts
      mkEffect' = mkEffect mkWrite mkAlloc mkDealloc mkCall
      results = mapMaybe getRetVal stmts
      effects = mapMaybe mkEffect' stmts
   in CodeSummary
        { inputVars = inputVars
        , inputLoads =
            concatMap
              ( filter
                  ( \x ->
                      not
                        ( isArgLoad (HashSet.fromList inputVars) x
                            || isStackLocalLoad x
                        )
                  )
                  . findLoads
              )
              stmts
        , results = results
        , effects = effects
        , capabilities = extractCapabilities inputVars results stmts
        }

data RemoveKilledResult = RemoveKilledResult
  { effects :: [Effect]
  , seenStoreAddr :: HashSet Expression
  }
  deriving (Eq, Ord, Show, Generic)

-- | Remove write effects that are killed by later writes
removeKilledWrites :: CodeSummary -> CodeSummary
removeKilledWrites codeSum =
  let initResult :: RemoveKilledResult
      initResult = RemoveKilledResult [] HashSet.empty
      f :: Effect -> RemoveKilledResult -> RemoveKilledResult
      f x prevResult@(RemoveKilledResult effects seenStoreAddr) =
        case x of
          EffectWrite (Store (StoreOp addr _)) ->
            if HashSet.member addr seenStoreAddr
              then prevResult
              else RemoveKilledResult (x : effects) (HashSet.insert addr seenStoreAddr)
          _ -> RemoveKilledResult (x : effects) seenStoreAddr
      effects' :: [Effect]
      -- The effects need to be processed in reverse order.
      -- Using `foldr` meets this requirement.
      effects' = foldr f initResult (codeSum ^. #effects) ^. #effects
   in codeSum & #effects .~ effects'
