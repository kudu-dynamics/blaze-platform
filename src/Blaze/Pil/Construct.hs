module Blaze.Pil.Construct where

import Blaze.Prelude hiding (const, sym)

import qualified Blaze.Types.Pil as TPil

import qualified Data.HashSet as HSet

pilVar :: TPil.Symbol -> TPil.PilVar
pilVar s = TPil.PilVar { _symbol   = s
                       , _func     = Nothing
                       , _ctxIndex = Nothing
                       , _mapsTo   = HSet.empty
                       }

const :: Int64 -> TPil.OperationSize -> TPil.Expression
const x size = TPil.Expression { _size = size 
                               , _op = TPil.CONST (TPil.ConstOp x) }

var :: TPil.Symbol -> TPil.OperationSize -> TPil.Expression
var sym size = TPil.Expression { _size = size 
                               , _op = TPil.VAR $ TPil.VarOp $ pilVar sym}

def :: TPil.Symbol -> TPil.Expression -> TPil.Stmt
def sym expr = TPil.Def (TPil.DefOp (pilVar sym) expr)

add :: TPil.Expression -> TPil.Expression -> TPil.OperationSize -> TPil.Expression
add x y size = TPil.Expression { _size = size
                               , _op = TPil.ADD (TPil.AddOp x y)}

