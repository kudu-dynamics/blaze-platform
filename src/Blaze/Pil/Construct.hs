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

var :: Pil.Symbol -> Pil.OperationSize -> Pil.Expression
var sym size = Pil.Expression { _size = size 
                               , _op = Pil.VAR $ Pil.VarOp $ pilVar sym}

def :: Pil.Symbol -> Pil.Expression -> Pil.Stmt
def sym expr = Pil.Def (Pil.DefOp (pilVar sym) expr)

binOp :: (a -> ExprOp Expression)
  -> (Expression -> Expression -> a)
  -> Expression -> Expression -> OperationSize -> Expression
binOp f g x y size = Expression { _size = size
                                , _op = f (g x y)
                                }

add :: Expression -> Expression -> OperationSize -> Expression
add = binOp Pil.ADD Pil.AddOp

sub :: Expression -> Expression -> OperationSize -> Expression
sub = binOp Pil.SUB Pil.SubOp

cmpE :: Expression -> Expression -> OperationSize -> Expression
cmpE = binOp Pil.CMP_E Pil.CmpEOp

const :: Int64 -> OperationSize -> Expression
const n size = Expression { _size = size
                          , _op = Pil.CONST . Pil.ConstOp $ n
                          }


