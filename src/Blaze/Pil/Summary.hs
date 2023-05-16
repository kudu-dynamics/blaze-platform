module Blaze.Pil.Summary (
  module Blaze.Pil.Summary,
  module Blaze.Types.Pil.Summary,
) where

import Blaze.Pil (
  ExprOp (LOAD, VAR),
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
import Blaze.Pil.Analysis (LoadExpr (LoadExpr), findLoads, getFreeVars)
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
   in CodeSummary
        { inputVars = inputVars
        , inputLoads =
            filter
              ( \x ->
                  not
                    ( isArgLoad (HashSet.fromList inputVars) x
                        || isStackLocalLoad x
                    )
              )
              (concatMap findLoads stmts)
        , results = mapMaybe getRetVal stmts
        , effects = mapMaybe mkEffect' stmts
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
