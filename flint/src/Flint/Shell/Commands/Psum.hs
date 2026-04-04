module Flint.Shell.Commands.Psum
  ( psumCommand
  , includeInPsum
  ) where

import Flint.Prelude

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))

import Blaze.Pretty (pretty', PStmts(PStmts))
import Blaze.Types.Function (FuncParamInfo(..))
import qualified Blaze.Types.Pil as Pil
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text


psumCommand :: ShellCommand
psumCommand = ShellCommand
  { cmdName = "psum"
  , cmdAliases = []
  , cmdHelp = "Show path summary: only calls and non-stack-local memory ops"
  , cmdUsage = "psum [path_id ...]  (no args = all paths)"
  , cmdAction = psumPaths
  }

psumPaths :: ShellState -> [Text] -> IO CommandResult
psumPaths st args = do
  refs <- case args of
    [] -> do
      cache <- allPaths st
      return . fmap (`PathRef` False) . sort $ HashMap.keys cache
    _ -> resolvePathRefs st args
  case refs of
    [] -> return $ ResultOk "No paths loaded."
    _ -> do
      results <- forM refs $ \(PathRef pid raw) -> do
        mPath <- lookupPath st pid
        mTag <- lookupTag st pid
        case mPath of
          Nothing -> return $ "Path " <> show pid <> ": not found"
          Just cp -> do
            let stmts = resolveStmts cp raw
                filtered = filter includeInPsum stmts
                rawTag = if raw then " [raw]" else ""
                tagLabel = maybe "" (\t -> " \"" <> t <> "\"") mTag
                funcName = cp ^. #sourceFunc . #name
                paramNames = fmap getParamName $ cp ^. #sourceFunc . #params
                funcSig = funcName <> "(" <> Text.intercalate ", " paramNames <> ")"
                header = "=== Path " <> show pid <> tagLabel <> rawTag
                  <> " (func: " <> funcSig
                  <> ", " <> show (length filtered) <> "/" <> show (length stmts) <> " stmts) ==="
            return $ header <> "\n" <> pretty' (PStmts filtered)
      return . ResultText $ Text.intercalate "\n\n" results

getParamName :: FuncParamInfo -> Text
getParamName (FuncParamInfo p) = p ^. #name
getParamName (FuncVarArgInfo p) = p ^. #name

------------------------------------------------------------------------
-- PIL statement filtering for path summaries
------------------------------------------------------------------------

-- | Should this statement be included in the path summary?
--
-- A statement is included if it contains:
--   1. A call expression (not __stack_chk_fail)
--   2. A LOAD or STORE whose address does NOT root at STACK_LOCAL_ADDR
--
-- Excluded unconditionally: Ret, NoRet, Exit, Nop, Undef,
-- and any statement referencing in_FS_OFFSET (stack canary boilerplate).
includeInPsum :: Pil.Stmt -> Bool
includeInPsum stmt =
  let s = stmt ^. #statement
  in not (isAlwaysExcluded s)
     && not (stmtReferencesFSOffset s)
     && isInteresting s

-- | Statements that never carry interesting external-state information.
isAlwaysExcluded :: Pil.Statement Pil.Expression -> Bool
isAlwaysExcluded = \case
  Pil.Ret _ -> True
  Pil.NoRet -> True
  Pil.Exit -> True
  Pil.Nop -> True
  Pil.Undef -> True
  _ -> False

-- | A statement is interesting if it contains calls or non-stack memory ops.
isInteresting :: Pil.Statement Pil.Expression -> Bool
isInteresting = \case
  -- Statement-level calls: include unless targeting __stack_chk_fail
  Pil.Call (Pil.CallOp dest _) -> not (isStackChkFail dest)
  Pil.TailCall (Pil.TailCallOp dest _ _) -> not (isStackChkFail dest)
  -- Store with non-stack-local destination address
  Pil.Store (Pil.StoreOp addr _) | not (addrRootsAtStackLocal addr) -> True
  -- For all other statements: check sub-expressions for calls or non-stack loads
  s -> anyExprInteresting s

-- | Check if any expression within a statement (recursively) is a call or non-stack load.
anyExprInteresting :: Pil.Statement Pil.Expression -> Bool
anyExprInteresting s = any (any isInterestingExpr . allExprs) (F.toList s)

-- | An expression is interesting if it's a non-__stack_chk_fail call
--   or a load from non-stack-local memory.
isInterestingExpr :: Pil.Expression -> Bool
isInterestingExpr expr = case expr ^. #op of
  Pil.CALL (Pil.CallOp dest _) -> not (isStackChkFail dest)
  Pil.LOAD (Pil.LoadOp src) -> not (addrRootsAtStackLocal src)
  _ -> False

-- | Check if the root of an address computation is STACK_LOCAL_ADDR.
-- Walks the left/base spine of address arithmetic (ADD, FIELD_ADDR, ARRAY_ADDR).
addrRootsAtStackLocal :: Pil.Expression -> Bool
addrRootsAtStackLocal expr = case expr ^. #op of
  Pil.STACK_LOCAL_ADDR _ -> True
  Pil.ADD (Pil.AddOp l _) -> addrRootsAtStackLocal l
  Pil.FIELD_ADDR (Pil.FieldAddrOp base _) -> addrRootsAtStackLocal base
  Pil.ARRAY_ADDR (Pil.ArrayAddrOp base _ _) -> addrRootsAtStackLocal base
  _ -> False

-- | Check if a CallDest targets __stack_chk_fail.
isStackChkFail :: Pil.CallDest Pil.Expression -> Bool
isStackChkFail = \case
  Pil.CallExtern ef -> ef ^. #name == "__stack_chk_fail"
  Pil.CallFunc f -> f ^. #name == "__stack_chk_fail"
  Pil.CallAddr (Pil.ConstFuncPtrOp _ (Just s)) -> s == "__stack_chk_fail"
  _ -> False

-- | Check if an expression references the in_FS_OFFSET variable (stack canary TLS slot).
referencesFSOffset :: Pil.Expression -> Bool
referencesFSOffset expr = case expr ^. #op of
  Pil.VAR (Pil.VarOp v) -> v ^. #symbol == "in_FS_OFFSET"
  _ -> any referencesFSOffset (F.toList (expr ^. #op))

-- | Check if any expression in a statement references in_FS_OFFSET.
stmtReferencesFSOffset :: Pil.Statement Pil.Expression -> Bool
stmtReferencesFSOffset = any (any referencesFSOffset . allExprs) . F.toList

-- | All sub-expressions of an expression (self + all descendants).
allExprs :: Pil.Expression -> [Pil.Expression]
allExprs expr = expr : concatMap allExprs (F.toList (expr ^. #op))
