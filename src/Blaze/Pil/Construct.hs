module Blaze.Pil.Construct where

import Blaze.Prelude hiding (Symbol, const, sym)
import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Function (Function(Function))
import Blaze.Types.Pil
  ( ExprOp,
    Expression (Expression),
    Size,
    PilVar (PilVar),
    Statement,
    Symbol,
  )

class ExprConstructor attrs expr | expr -> attrs where
  mkExpr :: attrs -> Pil.ExprOp expr -> expr

instance ExprConstructor (Size Expression) Expression where
  mkExpr = Expression

defaultSize :: forall a. Size a
defaultSize = 8

pilVar_ :: Size PilVar -> Maybe Pil.Ctx -> Symbol -> PilVar
pilVar_ = PilVar

pilVar' :: Pil.Ctx -> Symbol -> PilVar
pilVar' ctx = pilVar_ defaultSize (Just ctx)

pilVar :: Symbol -> PilVar
pilVar = pilVar_ defaultSize Nothing

binOp
  :: ExprConstructor attrs expr
  => (a -> ExprOp expr)
  -> (expr -> expr -> a)
  -> expr
  -> expr
  -> attrs
  -> expr
binOp f g x y size = mkExpr size $ f (g x y)

unOp
  :: ExprConstructor attrs expr
  => (a -> ExprOp expr)
  -> (expr -> a)
  -> expr
  -> attrs
  -> expr
unOp f g x size = mkExpr size $ f (g x)

---- Expressions
const :: ExprConstructor attrs expr => Int64 -> attrs -> expr
const x size = mkExpr size (Pil.CONST (Pil.ConstOp x))

fconst :: ExprConstructor attrs expr => Double -> attrs -> expr
fconst x size = mkExpr size (Pil.CONST_FLOAT (Pil.ConstFloatOp x))

unit :: ExprConstructor attrs expr => attrs -> expr
unit attrs = mkExpr attrs Pil.UNIT

constPtr :: ExprConstructor attrs expr => Word64 -> attrs -> expr
constPtr addr size = mkExpr size (Pil.CONST_PTR (Pil.ConstPtrOp (fromIntegral addr :: Int64)))

externPtr :: ExprConstructor attrs expr => Address -> ByteOffset -> Maybe Symbol -> attrs -> expr
externPtr addr off sym size = mkExpr size (Pil.ExternPtr (Pil.ExternPtrOp addr off sym))

constStr :: ExprConstructor attrs expr => Text -> attrs -> expr
constStr str size = mkExpr size (Pil.ConstStr (Pil.ConstStrOp str))

var' :: ExprConstructor attrs expr => PilVar -> attrs -> expr
var' pv size = mkExpr size (Pil.VAR $ Pil.VarOp pv)

var :: ExprConstructor attrs expr => Symbol -> attrs -> expr
var sym size = mkExpr size (Pil.VAR $ Pil.VarOp $ pilVar sym)

add :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
add = binOp Pil.ADD Pil.AddOp

addWillCarry :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
addWillCarry = binOp Pil.ADD_WILL_CARRY Pil.AddWillCarryOp

addWillOverflow :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
addWillOverflow = binOp Pil.ADD_WILL_OVERFLOW Pil.AddWillOverflowOp

sub :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
sub = binOp Pil.SUB Pil.SubOp

subWillOverflow :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
subWillOverflow = binOp Pil.SUB_WILL_OVERFLOW Pil.SubWillOverflowOp

mul :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
mul = binOp Pil.MUL Pil.MulOp

cmpE :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
cmpE = binOp Pil.CMP_E Pil.CmpEOp

cmpNE :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
cmpNE = binOp Pil.CMP_NE Pil.CmpNeOp

cmpSge :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
cmpSge = binOp Pil.CMP_SGE Pil.CmpSgeOp

cmpSgt :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
cmpSgt = binOp Pil.CMP_SGT Pil.CmpSgtOp

cmpSle :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
cmpSle = binOp Pil.CMP_SLE Pil.CmpSleOp

cmpSlt :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
cmpSlt = binOp Pil.CMP_SLT Pil.CmpSltOp

cmpUge :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
cmpUge = binOp Pil.CMP_UGE Pil.CmpUgeOp

cmpUgt :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
cmpUgt = binOp Pil.CMP_UGT Pil.CmpUgtOp

cmpUle :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
cmpUle = binOp Pil.CMP_ULE Pil.CmpUleOp

cmpUlt :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
cmpUlt = binOp Pil.CMP_ULT Pil.CmpUltOp

-- float cmp
fcmpE :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
fcmpE = binOp Pil.FCMP_E Pil.FcmpEOp

fcmpNE :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
fcmpNE = binOp Pil.FCMP_NE Pil.FcmpNeOp

fcmpGe :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
fcmpGe = binOp Pil.FCMP_GE Pil.FcmpGeOp

fcmpGt :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
fcmpGt = binOp Pil.FCMP_GT Pil.FcmpGtOp

fcmpLe :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
fcmpLe = binOp Pil.FCMP_LE Pil.FcmpLeOp

fcmpLt :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
fcmpLt = binOp Pil.FCMP_LT Pil.FcmpLtOp

sx :: ExprConstructor attrs expr => expr -> attrs -> expr
sx = unOp Pil.SX Pil.SxOp

zx :: ExprConstructor attrs expr => expr -> attrs -> expr
zx = unOp Pil.ZX Pil.ZxOp

strcmp :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
strcmp = binOp Pil.StrCmp Pil.StrCmpOp

or :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
or = binOp Pil.OR Pil.OrOp

and :: ExprConstructor attrs expr => expr -> expr -> attrs -> expr
and = binOp Pil.AND Pil.AndOp

not :: ExprConstructor attrs expr => expr -> attrs -> expr
not = unOp Pil.NOT Pil.NotOp

-- TODO: Change to just Load. PIL is being updated to drop versioned memory.
load :: ExprConstructor attrs expr => expr -> attrs -> expr
load addr size = mkExpr size (Pil.LOAD (Pil.LoadOp addr))

varField :: ExprConstructor attrs expr => Pil.Symbol -> ByteOffset -> attrs -> expr
varField sym offset size =
  mkExpr size (Pil.VAR_FIELD $ Pil.VarFieldOp (pilVar sym) offset)

fieldAddr :: ExprConstructor attrs expr => expr -> ByteOffset -> attrs -> expr
fieldAddr base offset size = 
  mkExpr size . Pil.FIELD_ADDR $ Pil.FieldAddrOp base offset

arrayAddr :: ExprConstructor attrs expr => expr -> expr -> Word64 -> attrs -> expr
arrayAddr base index stride size =
  mkExpr size . Pil.ARRAY_ADDR $ Pil.ArrayAddrOp base index stride

stackLocalAddr :: ExprConstructor attrs expr => expr -> ByteOffset -> attrs -> expr
stackLocalAddr base offset size = 
  mkExpr size . Pil.FIELD_ADDR $ Pil.FieldAddrOp base offset

---- Statements

-- | Constructs a 'Pil.Def' that assigns the 'expr' to a 'PilVar' with symbol
-- 'sym' and size 'defaultSize'
def :: Symbol -> expr -> Statement expr
def sym = def' $ pilVar sym

-- | Constructs a 'Pil.Def' that assigns 'val' to 'pv'
def' :: PilVar -> expr -> Statement expr
def' pv val = Pil.Def (Pil.DefOp pv val)

-- TODO: This helper assumes the only output of the call operation
--       is the variable being defined.
defCall'
  :: forall attrs expr. ExprConstructor attrs expr
  => PilVar
  -> Pil.CallDest expr
  -> [expr]
  -> attrs
  -> Statement expr
defCall' pv dest args size = def' pv callExpr
  where
    mname :: Maybe Text
    mname = case dest of
      Pil.CallFunc (Function _ nm _ _) -> Just nm
      _ -> Nothing
    callExpr :: expr
    callExpr = mkExpr size $ Pil.CALL $ Pil.CallOp dest mname args

-- TODO: This helper assumes the only output of the call operation
--       is the variable being defined.
defCall
  :: forall attrs expr. ExprConstructor attrs expr
  => Symbol
  -> Pil.CallDest expr
  -> [expr]
  -> attrs
  -> Statement expr
defCall sym = defCall' $ pilVar sym

defPhi :: Symbol -> [Symbol] -> Statement expr
defPhi sym = Pil.DefPhi . Pil.DefPhiOp (pilVar sym) . fmap pilVar

store :: expr -> expr -> Statement expr
store addr val = Pil.Store (Pil.StoreOp addr val)

constraint :: expr -> Statement expr
constraint e = Pil.Constraint (Pil.ConstraintOp e)

branchCond :: expr -> Statement expr
branchCond e = Pil.BranchCond (Pil.BranchCondOp e)

ret :: expr -> Statement expr
ret = Pil.Ret . Pil.RetOp

nop :: Statement expr
nop = Pil.Nop

callStmt :: Pil.CallDest expr -> [expr] -> Statement expr
callStmt dest args = Pil.Call $ Pil.CallOp dest mname args
  where
    mname :: Maybe Text
    mname = case dest of
      Pil.CallFunc (Function _ nm _ _) -> Just nm
      _ -> Nothing
