module Flint.Types.Analysis.Path.Matcher.PathPrep where

import Flint.Prelude

import Flint.Analysis.Path.Matcher.Taint (mkTaintSet)
import Flint.Types.Analysis.Path.Matcher ( TypedStmt )
import Flint.Types.Analysis (Taint(..), TaintPropagator(..))

import Blaze.Cfg.Path (PilPath)
import qualified Blaze.Cfg.Path as Path
import qualified Blaze.Pil.Analysis.Path as PA
import Blaze.Pil.Checker (checkStmts)
import qualified Blaze.Types.Pil.Checker as Ch
import qualified Blaze.Pil.Summary as Summary
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil.Summary (CodeSummary)


data PathPrep stmt = PathPrep
  { untouchedStmts :: [stmt]
  , stmts :: [stmt]
  , taintSet :: HashSet Taint
  , codeSummary :: CodeSummary
  } deriving (Eq, Ord, Show, Generic)

class MkPathPrep stmt a where
  mkPathPrep :: [TaintPropagator] -> a -> PathPrep stmt

instance MkPathPrep Pil.Stmt [Pil.Stmt] where
  mkPathPrep props stmts = PathPrep stmts stmts' (mkTaintSet props stmts) codeSummary
    where
      stmts' = PA.aggressiveExpand stmts
      codeSummary = Summary.fromStmts stmts

instance MkPathPrep Pil.Stmt PilPath where
  mkPathPrep mprops p = PathPrep stmts stmts' (mkTaintSet mprops stmts) codeSummary
    where
      stmts = Path.toStmts p
      stmts' = PA.aggressiveExpand stmts
      -- TODO: `resolveCalls` is a uefi thing. make this optional
      -- stmts' = resolveCalls . PA.aggressiveExpand $ stmts
      codeSummary = Summary.fromStmts stmts'

stripSymInfo :: Ch.SymTypedStmt -> TypedStmt
stripSymInfo = fmap stripExpr
  where
    stripExpr
      :: Ch.InfoExpression (Ch.SymInfo, Maybe Ch.DeepSymType)
      -> Ch.InfoExpression (Ch.BitWidth, Maybe Ch.DeepSymType)
    stripExpr (Ch.InfoExpression (Ch.SymInfo bw _, dst) op)
      = Ch.InfoExpression (bw, dst) $ fmap stripExpr op

toTypedStmts :: [Pil.Stmt] -> [TypedStmt]
toTypedStmts stmts = case checkStmts Nothing stmts of
  Left err -> error $ "ConstraintGenError: " <> show err
  Right tr -> stripSymInfo . snd <$> tr ^. #symTypedStmts

instance MkPathPrep TypedStmt [Pil.Stmt] where
  mkPathPrep props stmts = tr
    { untouchedStmts = toTypedStmts $ tr ^. #untouchedStmts
    , stmts = toTypedStmts $ tr ^. #stmts
    }
    where
      tr = mkPathPrep props stmts

instance MkPathPrep TypedStmt PilPath where
  mkPathPrep props stmts = tr
    { untouchedStmts = toTypedStmts $ tr ^. #untouchedStmts
    , stmts = toTypedStmts $ tr ^. #stmts
    }
    where
      tr = mkPathPrep props stmts

