module Blaze.Pil.Construct where

import Blaze.Prelude hiding (Symbol, const, sym)
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Function (Function(Function))
import Blaze.Types.Pil
  ( ExprOp,
    Expression (Expression),
    OperationSize,
    PilVar (PilVar),
    Stmt,
    Symbol,
  )

pilVar' :: Symbol -> Pil.Ctx -> PilVar
pilVar' s ctx =
  PilVar
    { symbol = s
    , ctx = Just ctx
    }

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

mul :: Expression -> Expression -> OperationSize -> Expression
mul = binOp Pil.MUL Pil.MulOp

cmpE :: Expression -> Expression -> OperationSize -> Expression
cmpE = binOp Pil.CMP_E Pil.CmpEOp

cmpNE :: Expression -> Expression -> OperationSize -> Expression
cmpNE = binOp Pil.CMP_NE Pil.CmpNeOp

cmpSge :: Expression -> Expression -> OperationSize -> Expression
cmpSge = binOp Pil.CMP_SGE Pil.CmpSgeOp

cmpSgt :: Expression -> Expression -> OperationSize -> Expression
cmpSgt = binOp Pil.CMP_SGT Pil.CmpSgtOp

cmpSle :: Expression -> Expression -> OperationSize -> Expression
cmpSle = binOp Pil.CMP_SLE Pil.CmpSleOp

cmpSlt :: Expression -> Expression -> OperationSize -> Expression
cmpSlt = binOp Pil.CMP_SLT Pil.CmpSltOp

cmpUge :: Expression -> Expression -> OperationSize -> Expression
cmpUge = binOp Pil.CMP_UGE Pil.CmpUgeOp

cmpUgt :: Expression -> Expression -> OperationSize -> Expression
cmpUgt = binOp Pil.CMP_UGT Pil.CmpUgtOp

cmpUle :: Expression -> Expression -> OperationSize -> Expression
cmpUle = binOp Pil.CMP_ULE Pil.CmpUleOp

cmpUlt :: Expression -> Expression -> OperationSize -> Expression
cmpUlt = binOp Pil.CMP_ULT Pil.CmpUltOp

sx :: Expression -> OperationSize -> Expression
sx = unOp Pil.SX Pil.SxOp

zx :: Expression -> OperationSize -> Expression
zx = unOp Pil.ZX Pil.ZxOp

strcmp :: Expression -> Expression -> OperationSize -> Expression
strcmp = binOp Pil.StrCmp Pil.StrCmpOp

or :: Expression -> Expression -> OperationSize -> Expression
or = binOp Pil.OR Pil.OrOp

and :: Expression -> Expression -> OperationSize -> Expression
and = binOp Pil.AND Pil.AndOp

not :: Expression -> OperationSize -> Expression
not = unOp Pil.NOT Pil.NotOp

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
def sym = def' $ pilVar sym

def' :: PilVar -> Expression -> Stmt
def' pv val = Pil.Def (Pil.DefOp pv val)

-- TODO: This helper assumes the only output of the call operation
--       is the variable being defined.
defCall' :: PilVar -> Pil.CallDest Expression -> [Expression] -> OperationSize -> Stmt
defCall' pv dest args size = def' pv callExpr
  where
    mname :: Maybe Text
    mname = case dest of
      Pil.CallFunc (Function _ nm _ _) -> Just nm
      _ -> Nothing
    callExpr :: Expression
    callExpr = mkExpr size $ Pil.CALL $ Pil.CallOp dest mname args

-- TODO: This helper assumes the only output of the call operation
--       is the variable being defined.
defCall :: Symbol -> Pil.CallDest Expression -> [Expression] -> OperationSize -> Stmt
defCall sym = defCall' $ pilVar sym

defPhi :: Symbol -> [Symbol] -> Stmt
defPhi sym = Pil.DefPhi . Pil.DefPhiOp (pilVar sym) . fmap pilVar

store :: Expression -> Expression -> Stmt
store addr val = Pil.Store (Pil.StoreOp addr val)

constraint :: Expression -> Stmt
constraint e = Pil.Constraint (Pil.ConstraintOp e)

branchCond :: Expression -> Stmt
branchCond e = Pil.BranchCond (Pil.BranchCondOp e)

ret :: Expression -> Stmt
ret = Pil.Ret . Pil.RetOp

nop :: Stmt
nop = Pil.Nop
