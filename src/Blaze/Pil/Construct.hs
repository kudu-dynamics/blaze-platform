module Blaze.Pil.Construct where

import Blaze.Prelude hiding (Symbol, const, sym)
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil
  ( ExprOp,
    Expression (Expression),
    OperationSize,
    PilVar (PilVar),
    Stmt,
    Symbol,
  )
import qualified Data.HashSet as HSet

pilVar :: Symbol -> PilVar
pilVar s =
  PilVar
    { _symbol = s,
      _func = Nothing,
      _ctxIndex = Nothing,
      _mapsTo = HSet.empty
    }

mkExpr :: OperationSize -> ExprOp Expression -> Expression
mkExpr size op =
  Expression
    { _size = size,
      _op = op
    }

binOp ::
  (a -> ExprOp Expression) ->
  (Expression -> Expression -> a) ->
  Expression ->
  Expression ->
  OperationSize ->
  Expression
binOp f g x y size =
  Expression
    { _size = size,
      _op = f (g x y)
    }

---- Expressions
const :: Int64 -> OperationSize -> Expression
const x size = mkExpr size (Pil.CONST (Pil.ConstOp x))

var :: Symbol -> OperationSize -> Expression
var sym size = mkExpr size (Pil.VAR $ Pil.VarOp $ pilVar sym)

add :: Expression -> Expression -> OperationSize -> Expression
add = binOp Pil.ADD Pil.AddOp

sub :: Expression -> Expression -> OperationSize -> Expression
sub = binOp Pil.SUB Pil.SubOp

cmpE :: Expression -> Expression -> OperationSize -> Expression
cmpE = binOp Pil.CMP_E Pil.CmpEOp

-- TODO: Change to just Load. PIL is being updated to drop versioned memory.
load :: Expression -> OperationSize -> Expression
load addr size = mkExpr size (Pil.LOAD (Pil.LoadOp addr))

---- Statements
def :: Symbol -> Expression -> Stmt
def sym val = Pil.Def (Pil.DefOp (pilVar sym) val)

store :: Expression -> Expression -> Stmt
store addr val = Pil.Store (Pil.StoreOp addr val)

constraint :: Expression -> Stmt
constraint e = Pil.Constraint (Pil.ConstraintOp e)
