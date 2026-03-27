module Flint.Types.Analysis.Path.Matcher.PathPrep where

import Flint.Prelude

import Flint.Analysis.Path.Matcher.Taint (mkTaintSet)
import Flint.Types.Analysis.Path.Matcher ( TypedStmt )
import Flint.Types.Analysis (Taint(..), TaintPropagator(..))

import Blaze.Cfg.Path (PilPath)
import qualified Blaze.Cfg.Path as Path
import qualified Blaze.Pil.Analysis.Path as PA
import Blaze.Pil.Checker (checkStmtsWithTypeHints)
import qualified Blaze.Types.Pil.Checker as Ch
import qualified Blaze.Pil.Summary as Summary
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil.Summary (CodeSummary)
import Blaze.Types.Import (TypeHints)
import qualified Data.HashMap.Strict as HM


data PathPrep stmt = PathPrep
  { untouchedStmts :: [stmt]
  , stmts :: [stmt]
  , taintSet :: HashSet Taint
  , codeSummary :: CodeSummary
  , taintPropagators :: [TaintPropagator]
  } deriving (Eq, Ord, Show, Generic)

class MkPathPrep stmt a where
  mkPathPrep :: [TaintPropagator] -> a -> PathPrep stmt
  mkPathPrepWithTypeHints :: TypeHints -> [TaintPropagator] -> a -> PathPrep stmt

instance MkPathPrep Pil.Stmt [Pil.Stmt] where
  mkPathPrep props stmts = PathPrep stmts stmts' (mkTaintSet props stmts) codeSummary props
    where
      stmts' = PA.aggressiveExpand stmts
      codeSummary = Summary.fromStmts stmts
  mkPathPrepWithTypeHints _ = mkPathPrep

instance MkPathPrep Pil.Stmt PilPath where
  mkPathPrep mprops p = PathPrep stmts stmts' (mkTaintSet mprops stmts) codeSummary mprops
    where
      stmts = Path.toStmts p
      stmts' = PA.aggressiveExpand stmts
      -- TODO: `resolveCalls` is a uefi thing. make this optional
      -- stmts' = resolveCalls . PA.aggressiveExpand $ stmts
      codeSummary = Summary.fromStmts stmts'
  mkPathPrepWithTypeHints _ = mkPathPrep

stripSymInfo :: Ch.SymTypedStmt -> TypedStmt
stripSymInfo = fmap stripExpr
  where
    stripExpr
      :: Ch.InfoExpression (Ch.SymInfo, Maybe Ch.DeepSymType)
      -> Ch.InfoExpression (Ch.BitWidth, Maybe Ch.DeepSymType)
    stripExpr (Ch.InfoExpression (Ch.SymInfo bw _, dst) op)
      = Ch.InfoExpression (bw, dst) $ fmap stripExpr op

toTypedStmts :: TypeHints -> [Pil.Stmt] -> [TypedStmt]
toTypedStmts typeHints stmts = case checkStmtsWithTypeHints typeHints Nothing stmts of
  Left err -> error $ "ConstraintGenError: " <> show err
  Right tr -> stripSymInfo . snd <$> tr ^. #symTypedStmts

-- | Type-check BEFORE aggressiveExpand to avoid O(2^n) blowup.
-- The raw stmts have VAR references (O(1) for the type checker).
-- Then aggressiveExpand runs on the already-typed statements.
instance MkPathPrep TypedStmt [Pil.Stmt] where
  mkPathPrep = mkPathPrepWithTypeHints HM.empty
  mkPathPrepWithTypeHints typeHints props stmts =
    PathPrep typed expanded (mkTaintSet props stmts) codeSummary props
    where
      typed = toTypedStmts typeHints stmts
      expanded = PA.aggressiveExpand_ typed
      codeSummary = Summary.fromStmts stmts

instance MkPathPrep TypedStmt PilPath where
  mkPathPrep = mkPathPrepWithTypeHints HM.empty
  mkPathPrepWithTypeHints typeHints props p =
    PathPrep typed expanded (mkTaintSet props stmts) codeSummary props
    where
      stmts = Path.toStmts p
      typed = toTypedStmts typeHints stmts
      expanded = PA.aggressiveExpand_ typed
      codeSummary = Summary.fromStmts stmts

