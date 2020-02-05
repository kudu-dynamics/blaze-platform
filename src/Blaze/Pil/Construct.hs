module Blaze.Pil.Construct where

import Blaze.Prelude hiding (const, sym)

import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil ( ExprOp
                       , Expression(Expression)
                       , OperationSize)
import qualified Data.HashSet as HSet

pilVar :: Pil.Symbol -> Pil.PilVar
pilVar s = Pil.PilVar { _symbol   = s
                       , _func     = Nothing
                       , _ctxIndex = Nothing
                       , _mapsTo   = HSet.empty
                       }

const :: Int64 -> Pil.OperationSize -> Pil.Expression
const x size = Pil.Expression { _size = size 
                               , _op = Pil.CONST (Pil.ConstOp x) }

var :: Pil.Symbol -> Pil.OperationSize -> Pil.Expression
var sym size = Pil.Expression { _size = size 
                               , _op = Pil.VAR $ Pil.VarOp $ pilVar sym}

def :: Pil.Symbol -> Pil.Expression -> Pil.Stmt
def sym expr = Pil.Def (Pil.DefOp (pilVar sym) expr)

binOp :: (a -> ExprOp Expression)
  -> (Expression -> Expression -> a)
  -> OperationSize -> Expression -> Expression -> Expression
binOp f g size x y = Expression { _size = size
                                , _op = f (g x y)
                                }

add :: OperationSize -> Expression -> Expression -> Expression
add = binOp Pil.ADD Pil.AddOp

sub :: OperationSize -> Expression -> Expression -> Expression
sub = binOp Pil.SUB Pil.SubOp

cmpE :: OperationSize -> Expression -> Expression -> Expression
cmpE = binOp Pil.CMP_E Pil.CmpEOp


-- add :: Expression -> Expression -> OperationSize -> Expression
-- add x y size = Expression { _size = size
--                           , _op = Pil.ADD (Pil.AddOp x y)}

