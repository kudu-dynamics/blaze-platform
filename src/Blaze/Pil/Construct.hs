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

pilVar :: Symbol -> PilVar
pilVar s =
  PilVar
    { symbol = s
    , ctx = Nothing
    }

mkExpr :: OperationSize -> ExprOp Expression -> Expression
mkExpr size op =
  Expression
    { size = size
    , op = op
    }

binOp
  :: (a -> ExprOp Expression)
  -> (Expression -> Expression -> a)
  -> Expression
  -> Expression
  -> OperationSize
  -> Expression
binOp f g x y size =
  Expression
    { size = size
    , op = f (g x y)
    }

unOp
  :: (a -> ExprOp Expression)
  -> (Expression -> a)
  -> Expression
  -> OperationSize
  -> Expression
unOp f g x size =
  Expression
    { size = size
    , op = f (g x)
    }

---- Expressions
const :: Int64 -> OperationSize -> Expression
const x size = mkExpr size (Pil.CONST (Pil.ConstOp x))

constPtr :: Word64 -> OperationSize -> Expression
constPtr addr size = mkExpr size (Pil.CONST_PTR (Pil.ConstPtrOp (fromIntegral addr)))

constStr :: Text -> OperationSize -> Expression
constStr str size = mkExpr size (Pil.ConstStr (Pil.ConstStrOp str))

var' :: PilVar -> OperationSize -> Expression
var' pv size = mkExpr size (Pil.VAR $ Pil.VarOp pv)

var :: Symbol -> OperationSize -> Expression
var sym size = mkExpr size (Pil.VAR $ Pil.VarOp $ pilVar sym)

add :: Expression -> Expression -> OperationSize -> Expression
add = binOp Pil.ADD Pil.AddOp

sub :: Expression -> Expression -> OperationSize -> Expression
sub = binOp Pil.SUB Pil.SubOp

cmpE :: Expression -> Expression -> OperationSize -> Expression
cmpE = binOp Pil.CMP_E Pil.CmpEOp

cmpNE :: Expression -> Expression -> OperationSize -> Expression
cmpNE = binOp Pil.CMP_NE Pil.CmpNeOp

cmpSgt :: Expression -> Expression -> OperationSize -> Expression
cmpSgt = binOp Pil.CMP_SGT Pil.CmpSgtOp

sx :: Expression -> OperationSize -> Expression
sx = unOp Pil.SX Pil.SxOp

zx :: Expression -> OperationSize -> Expression
zx = unOp Pil.ZX Pil.ZxOp

strcmp :: Expression -> Expression -> OperationSize -> Expression
strcmp = binOp Pil.StrCmp Pil.StrCmpOp

-- TODO: Change to just Load. PIL is being updated to drop versioned memory.
load :: Expression -> OperationSize -> Expression
load addr size = mkExpr size (Pil.LOAD (Pil.LoadOp addr))

varField :: Pil.Symbol -> ByteOffset -> OperationSize -> Expression
varField sym offset size =
  mkExpr size (Pil.VAR_FIELD $ Pil.VarFieldOp (pilVar sym) offset)

fieldAddr :: Pil.Expression -> ByteOffset -> OperationSize -> Expression
fieldAddr base offset size = 
  mkExpr size . Pil.FIELD_ADDR $ Pil.FieldAddrOp base offset

stackLocalAddr :: Expression -> ByteOffset -> OperationSize -> Expression
stackLocalAddr base offset size = 
  mkExpr size . Pil.FIELD_ADDR $ Pil.FieldAddrOp base offset

---- Statements
def :: Symbol -> Expression -> Stmt
def sym val = Pil.Def (Pil.DefOp (pilVar sym) val)

def' :: PilVar -> Expression -> Stmt
def' pv val = Pil.Def (Pil.DefOp pv val)

-- TODO: This helper assumes the only output of the call operation
--       is the variable being defined.
defCall :: Symbol -> Pil.CallDest Expression -> [Expression] -> OperationSize -> Stmt
defCall sym dest args size = def sym callExpr
  where
    callExpr :: Expression
    callExpr = mkExpr size $ Pil.CALL $ Pil.CallOp dest Nothing args

defPhi :: Symbol -> [Symbol] -> Stmt
defPhi sym = Pil.DefPhi . Pil.DefPhiOp (pilVar sym) . fmap pilVar

store :: Expression -> Expression -> Stmt
store addr val = Pil.Store (Pil.StoreOp addr val)

constraint :: Expression -> Stmt
constraint e = Pil.Constraint (Pil.ConstraintOp e)

ret :: Expression -> Stmt
ret = Pil.Ret . Pil.RetOp
