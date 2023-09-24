module Flint.Analysis.Path.Matcher where

import Flint.Prelude hiding (Symbol)

import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil (Expression, Stmt, PilVar, Size)


type Symbol = Text

data Func expr = FuncName Text
               | FuncAddress Address
  deriving (Eq, Ord, Show, Generic)

data CallDest expr = CallFunc (Func expr)
                   | CallIndirect expr
  deriving (Eq, Ord, Show, Generic)

data Statement expr
  = Def expr expr -- Def dst src; dst is always (Var PilVar) expr
  | Constraint expr
  | Store expr expr
  | EnterFunc (Func expr) -- need one that lets you access args of expanded call?
  | Call (CallDest expr) [expr]
  | BranchCond expr
  | Jump expr
  | Ret expr
  | NoRet
  | TailCall (CallDest expr) [expr]
  deriving (Eq, Ord, Show, Generic)

data StmtPattern
  = Stmt (Statement ExprPattern)
  | Never StmtPattern
  | AnyOne [StmtPattern]  -- One match succeeds. [] immediately succeeeds
  | Unordered [StmtPattern] -- matches all, but in no particular order
  | Ordered [StmtPattern] -- matches all. Good for grouping and scoping Where bounds
  | Where BoundExpr -- Add a boolean expr constraint, using bound variables
  deriving (Eq, Ord, Show, Generic)

data BoundExprSize
  = ConstSize (Size Pil.Expression)
  | SizeOf Symbol  -- looks up symbol to get size of expr
  deriving (Eq, Ord, Show, Generic)
  
data BoundExpr
  = Bound Symbol -- gets expression that has been bound with Bind
  | BoundExpr BoundExprSize (Pil.ExprOp BoundExpr)
  deriving (Eq, Ord, Show, Generic)

data ExprPattern
  = Expr (Pil.ExprOp ExprPattern)
  -- binds expr to Sym if pattern matches, or if Sym already exists, sees if equal to old sym val
  -- This allows you to nest more binds within ExprPattern
  | Bind Symbol ExprPattern 
  | Var Symbol -- matches prefix of var name, like "arg4" will match "arg4-7#1"
  | Contains ExprPattern -- matches if ExprPattern matches somewhere inside expr
  | Wild
  deriving (Eq, Ord, Show, Generic)

-- This example shows a failed check where they check the commbuffer size (arg4)
-- but later access outside its range. Pseudo-pattern:
--
-- bind 'y' = [arg4]
-- if (bind 'y' < bind 'length')
-- call SmmIsBufferOutsideSmmValid(arg3, bind 'y')
-- [arg3 + (bind 'n')] = (bind "store_val") _ where (bind 'n' > bind 'length')

exampleCommBufferOobUsage :: [StmtPattern]
exampleCommBufferOobUsage =
  [ Stmt $ Def (Bind "y" Wild) (Expr . Pil.LOAD . Pil.LoadOp $ Var "arg4")
  , Unordered
    [ AnyOne
      [ Stmt $ BranchCond . Expr . Pil.CMP_ULT $ Pil.CmpUltOp (Bind "y" Wild) (Bind "max_length" Wild)
      , Stmt $ BranchCond . Expr . Pil.CMP_UGE $ Pil.CmpUgeOp (Bind "max_length" Wild) (Bind "y" Wild)
      ]
    , Stmt $ Call (CallFunc (FuncName "SmmIsBufferOutsideSmmValid"))
      [ Var "arg3"
      , Bind "y" Wild
      ]
    ]
  , Stmt $ Store (Expr . Pil.ADD $ Pil.AddOp (Var "arg3") (Bind "n" Wild)) (Bind "stored_val" Wild)
  , Where . BoundExpr (ConstSize 8) . Pil.CMP_UGT $ Pil.CmpUgtOp (Bound "n") (Bound "max_length")
  ]
                            


-- type Expr = Expression

-- -- data PilParserError = PilParserError

-- data MatcherState input = MatcherState
--   { input :: input
--   , boundSyms :: HashMap Symbol Pil.Expression
--   , extraConstraints :: [Pil.Stmt]
--   } deriving Generic

-- newtype MatcherMonad input a = MatcherMonad {
--   _runMatcher :: ExceptT () (StateT (MatcherState input) Identity) a
--   }
--   deriving newtype
--     ( Functor
--     , Applicative
--     , Monad
--     , MonadError ()
--     , MonadState (MatcherState input)
--     , Alternative
--     )

-- getInput :: Matcher a a
-- getInput 

-- matchDef :: Stmt -> (PilVer -> Matcher ()) -> (Expr -> Matcher ()) -> Matcher ()
-- matchDef (Pil.Def (Pil.DefOp destVar srcExpr)) = 
  
-- def :: Matcher PilVar () -> Matcher Expr () -> Matcher Stmt ()
-- def a b = view #input >>= \case
--   Pil.Def (Pil.DefOp destVar srcExpr) -> 

-- load :: Matcher Expr () -> Matcher Expr ()
-- load = undefined

-- var :: Text -> Matcher Expr
-- var = undefined

-- bind :: Text -> Matcher Expr
-- bind = undefined

-- stmt :: Matcher Stmt -> Matcher [Stmt]
-- stmt = undefined

-- never :: Matcher Stmt -> Matcher [Stmt]
-- never = undefined

-- q :: [Matcher Stmt]

-- jackson = do
--   stmt $ def (bind "y") (load (var "arg4"))

