module Flint.Shell.Commands.TypeCheck
  ( typecheckCommand
  ) where

import Flint.Prelude hiding (sym)

import Flint.Shell.Types
import Flint.Shell.Command (ShellCommand(..))
import Flint.Types.Analysis.Path.Matcher.PathPrep (toTypedStmts)
import Flint.Types.Analysis.Path.Matcher (TypedStmt)

import Blaze.Types.Pil.Checker (InfoExpression(..), DeepSymType(..), BitWidth)
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Pil.Analysis.Path as PA

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Numeric (showHex)


typecheckCommand :: ShellCommand
typecheckCommand = ShellCommand
  { cmdName = "typecheck"
  , cmdAliases = ["tc"]
  , cmdHelp = "Type-check paths. Use N! to type-check WITHOUT aggressiveExpand (raw stmts)."
  , cmdUsage = "typecheck <path_ids>  (e.g. tc 0, tc 0! for raw/unexpanded)"
  , cmdAction = typecheckPaths
  }

typecheckPaths :: ShellState -> [Text] -> IO CommandResult
typecheckPaths _st [] = return $ ResultError "Usage: typecheck <path_ids>  (use N! to skip aggressiveExpand)"
typecheckPaths st args = do
  let refs = parsePathRefs args
  results <- forM refs $ \(PathRef pid mode) -> do
    mPath <- lookupPath st pid
    case mPath of
      Nothing -> return $ "Path " <> show pid <> ": not found"
      Just cp -> do
        let pilStmts = cp ^. #pilPath
            nStmts = length pilStmts
        if mode == ViewRaw
          then do
            t0 <- getCurrentTime
            let typedStmts = toTypedStmts HashMap.empty pilStmts
                !_ = length typedStmts
            t1 <- getCurrentTime
            let dt = diffUTCTime t1 t0
                header = "=== Path " <> show pid <> " [raw/unexpanded]"
                  <> " (func: " <> (cp ^. #sourceFunc . #name)
                  <> ", " <> show nStmts <> " stmts"
                  <> ", typecheck: " <> show dt <> ") ==="
                rendered = Text.unlines $ fmap renderTypedStmt typedStmts
            return $ header <> "\n" <> rendered
          else do
            t0 <- getCurrentTime
            let expanded = PA.aggressiveExpand pilStmts
                !_ = length expanded
            t1 <- getCurrentTime
            let typedStmts = toTypedStmts HashMap.empty expanded
                !_ = length typedStmts
            t2 <- getCurrentTime
            let expandTime = diffUTCTime t1 t0
                tcTime = diffUTCTime t2 t1
                header = "=== Path " <> show pid
                  <> " (func: " <> (cp ^. #sourceFunc . #name)
                  <> ", " <> show nStmts <> " stmts"
                  <> ", expand: " <> show expandTime
                  <> ", typecheck: " <> show tcTime <> ") ==="
                rendered = Text.unlines $ fmap renderTypedStmt typedStmts
            return $ header <> "\n" <> rendered
  return $ ResultText $ Text.intercalate "\n\n" results

renderTypedStmt :: TypedStmt -> Text
renderTypedStmt stmt =
  let addr = "0x" <> Text.pack (showHex (addrToInt $ stmt ^. #addr) "")
      exprs = collectExprs (stmt ^. #statement)
      typeAnnotations = Text.intercalate ", " $ fmap renderExprType exprs
  in addr <> ": " <> typeAnnotations

renderExprType :: InfoExpression (BitWidth, Maybe DeepSymType) -> Text
renderExprType (InfoExpression (bw, mType) _op) =
  show bw <> "b:" <> renderDeepSymType mType

renderDeepSymType :: Maybe DeepSymType -> Text
renderDeepSymType Nothing = "?"
renderDeepSymType (Just (DSVar s)) = "v" <> show s
renderDeepSymType (Just (DSType pt)) = show pt
renderDeepSymType (Just (DSRecursive s _)) = "rec(" <> show s <> ")"

collectExprs :: Pil.Statement (InfoExpression (BitWidth, Maybe DeepSymType))
             -> [InfoExpression (BitWidth, Maybe DeepSymType)]
collectExprs = foldr (:) []
