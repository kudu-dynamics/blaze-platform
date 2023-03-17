module Blaze.Pil.Construct where

import Blaze.Prelude hiding (Symbol, const, sym)
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Function (Function(Function))
import Blaze.Types.Pil
  ( ExprOp,
    Expression (Expression),
    Size,
    PilVar (PilVar),
    Stmt,
    Symbol,
  )


defaultSize :: forall a. Size a
defaultSize = 8

pilVar_ :: Size PilVar -> Maybe Pil.Ctx -> Symbol -> PilVar
pilVar_ = PilVar

pilVar' :: Pil.Ctx -> Symbol -> PilVar
pilVar' ctx = pilVar_ defaultSize (Just ctx)

pilVar :: Symbol -> PilVar
pilVar = pilVar_ defaultSize Nothing

mkExpr :: Size Expression -> ExprOp Expression -> Expression
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
  -> Size Expression
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
  -> Size Expression
  -> Expression
unOp f g x size =
  Expression
    { size = size
    , op = f (g x)
    }

---- Expressions
const :: Int64 -> Size Expression -> Expression
const x size = mkExpr size (Pil.CONST (Pil.ConstOp x))

unit :: Expression
unit = Pil.Expression 0 Pil.UNIT

constPtr :: Word64 -> Size Expression -> Expression
constPtr addr size = mkExpr size (Pil.CONST_PTR (Pil.ConstPtrOp (fromIntegral addr :: Int64)))

externPtr :: Address -> ByteOffset -> Maybe Symbol -> Size Expression -> Expression
externPtr addr off sym size = mkExpr size (Pil.ExternPtr (Pil.ExternPtrOp addr off sym))

constStr :: Text -> Size Expression -> Expression
constStr str size = mkExpr size (Pil.ConstStr (Pil.ConstStrOp str))

var' :: PilVar -> Size Expression -> Expression
var' pv size = mkExpr size (Pil.VAR $ Pil.VarOp pv)

var :: Symbol -> Size Expression -> Expression
var sym size = mkExpr size (Pil.VAR $ Pil.VarOp $ pilVar sym)

add :: Expression -> Expression -> Size Expression -> Expression
add = binOp Pil.ADD Pil.AddOp

addWillCarry :: Expression -> Expression -> Size Expression -> Expression
addWillCarry = binOp Pil.ADD_WILL_CARRY Pil.AddWillCarryOp

addWillOverflow :: Expression -> Expression -> Size Expression -> Expression
addWillOverflow = binOp Pil.ADD_WILL_OVERFLOW Pil.AddWillOverflowOp

sub :: Expression -> Expression -> Size Expression -> Expression
sub = binOp Pil.SUB Pil.SubOp

subWillOverflow :: Expression -> Expression -> Size Expression -> Expression
subWillOverflow = binOp Pil.SUB_WILL_OVERFLOW Pil.SubWillOverflowOp

mul :: Expression -> Expression -> Size Expression -> Expression
mul = binOp Pil.MUL Pil.MulOp

cmpE :: Expression -> Expression -> Size Expression -> Expression
cmpE = binOp Pil.CMP_E Pil.CmpEOp

cmpNE :: Expression -> Expression -> Size Expression -> Expression
cmpNE = binOp Pil.CMP_NE Pil.CmpNeOp

cmpSge :: Expression -> Expression -> Size Expression -> Expression
cmpSge = binOp Pil.CMP_SGE Pil.CmpSgeOp

cmpSgt :: Expression -> Expression -> Size Expression -> Expression
cmpSgt = binOp Pil.CMP_SGT Pil.CmpSgtOp

cmpSle :: Expression -> Expression -> Size Expression -> Expression
cmpSle = binOp Pil.CMP_SLE Pil.CmpSleOp

cmpSlt :: Expression -> Expression -> Size Expression -> Expression
cmpSlt = binOp Pil.CMP_SLT Pil.CmpSltOp

cmpUge :: Expression -> Expression -> Size Expression -> Expression
cmpUge = binOp Pil.CMP_UGE Pil.CmpUgeOp

cmpUgt :: Expression -> Expression -> Size Expression -> Expression
cmpUgt = binOp Pil.CMP_UGT Pil.CmpUgtOp

cmpUle :: Expression -> Expression -> Size Expression -> Expression
cmpUle = binOp Pil.CMP_ULE Pil.CmpUleOp

cmpUlt :: Expression -> Expression -> Size Expression -> Expression
cmpUlt = binOp Pil.CMP_ULT Pil.CmpUltOp

sx :: Expression -> Size Expression -> Expression
sx = unOp Pil.SX Pil.SxOp

zx :: Expression -> Size Expression -> Expression
zx = unOp Pil.ZX Pil.ZxOp

strcmp :: Expression -> Expression -> Size Expression -> Expression
strcmp = binOp Pil.StrCmp Pil.StrCmpOp

or :: Expression -> Expression -> Size Expression -> Expression
or = binOp Pil.OR Pil.OrOp

and :: Expression -> Expression -> Size Expression -> Expression
and = binOp Pil.AND Pil.AndOp

not :: Expression -> Size Expression -> Expression
not = unOp Pil.NOT Pil.NotOp

-- TODO: Change to just Load. PIL is being updated to drop versioned memory.
load :: Expression -> Size Expression -> Expression
load addr size = mkExpr size (Pil.LOAD (Pil.LoadOp addr))

varField :: Pil.Symbol -> ByteOffset -> Size Expression -> Expression
varField sym offset size =
  mkExpr size (Pil.VAR_FIELD $ Pil.VarFieldOp (pilVar sym) offset)

fieldAddr :: Pil.Expression -> ByteOffset -> Size Expression -> Expression
fieldAddr base offset size = 
  mkExpr size . Pil.FIELD_ADDR $ Pil.FieldAddrOp base offset

arrayAddr :: Pil.Expression -> Pil.Expression -> Word64 -> Size Expression -> Expression
arrayAddr base index stride size =
  mkExpr size . Pil.ARRAY_ADDR $ Pil.ArrayAddrOp base index stride

stackLocalAddr :: Expression -> ByteOffset -> Size Expression -> Expression
stackLocalAddr base offset size = 
  mkExpr size . Pil.FIELD_ADDR $ Pil.FieldAddrOp base offset

---- Statements
def :: Symbol -> Expression -> Stmt
def sym = def' $ pilVar sym

def' :: PilVar -> Expression -> Stmt
def' pv val = Pil.Def (Pil.DefOp pv val)

-- TODO: This helper assumes the only output of the call operation
--       is the variable being defined.
defCall' :: PilVar -> Pil.CallDest Expression -> [Expression] -> Size Expression -> Stmt
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
defCall :: Symbol -> Pil.CallDest Expression -> [Expression] -> Size Expression -> Stmt
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

callStmt :: Pil.CallDest Expression -> [Expression] -> Stmt
callStmt dest args = Pil.Call $ Pil.CallOp dest mname args
  where
    mname :: Maybe Text
    mname = case dest of
      Pil.CallFunc (Function _ nm _ _) -> Just nm
      _ -> Nothing
