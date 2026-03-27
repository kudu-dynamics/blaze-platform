-- | Typeclass for PIL expression types, enabling functions like
-- @substExprInExpr@ and @aggressiveExpand@ to work on both plain
-- @Expression@ and @InfoExpression@ (typed expressions).
module Blaze.Types.Pil.Expression
  ( IsExpression(..)
  ) where

import Blaze.Prelude hiding (Symbol)
import Blaze.Types.Pil.Common (PilVar, Size)
import qualified Blaze.Types.Pil as Pil


-- | Minimal interface for expression types that wrap @ExprOp@.
-- Both @Pil.Expression@ and @InfoExpression a@ satisfy this.
class (Eq expr, Hashable expr) => IsExpression expr where
  -- | Extract the operator (and recursive children) from an expression.
  getExprOp :: expr -> Pil.ExprOp expr

  -- | Rebuild an expression with the same metadata but a different operator.
  mkExprLike :: expr -> Pil.ExprOp expr -> expr

  -- | Create an expression from a size and operator (uses default metadata).
  mkExprWithSize :: Size Pil.Expression -> Pil.ExprOp expr -> expr

  -- | Create a variable reference expression.
  liftVar :: PilVar -> expr

  -- | Get the expression size.
  getExprSize :: expr -> Size Pil.Expression

instance IsExpression Pil.Expression where
  getExprOp = view #op
  mkExprLike x op = x & #op .~ op
  mkExprWithSize = Pil.Expression
  liftVar pv = Pil.Expression (coerce $ pv ^. #size) . Pil.VAR . Pil.VarOp $ pv
  getExprSize = view #size
