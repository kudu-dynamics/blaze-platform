module Flint.Types.Analysis.CAst.Matcher
  ( CExprPat(..)
  , CStmtPat(..)
  , ForPat(..)
  , CAstCheck(..)
  , Severity(..)
  ) where

import Flint.Prelude

import Flint.Types.Analysis.CAst.Finding (Severity(..))


-- | Pattern for matching C AST expressions.
-- Designed around loop analysis: patterns can bind expressions by name,
-- reference previously bound names, and recursively search expression trees.
data CExprPat
  = CWild                                  -- ^ Match any expression
  | CBind Text CExprPat                    -- ^ Bind matched expression to a name
  | CBound Text                            -- ^ Match only if equal to previously bound expr
  | CUses Text                             -- ^ Expression references a bound name anywhere in tree
  | CNamePat Text                          -- ^ Match CIdent by exact name
  | CNameRegex Text                        -- ^ Match CIdent by regex
  | CLitIntPat                             -- ^ Match any CLitInt
  | CLitStringPat                          -- ^ Match any CLitString
  | CCallPat Text [CExprPat]              -- ^ Match CFuncall by exact name + arg patterns
  | CCallNamesPat (HashSet Text) [CExprPat]-- ^ Match CFuncall with name in set
  | CIndexPat CExprPat CExprPat           -- ^ Match CIndex arr[idx]
  | CArrowPat CExprPat Text               -- ^ Match CArrow ptr->field
  | CDotPat CExprPat Text                 -- ^ Match CDot expr.field
  | CBinOpPat Text CExprPat CExprPat      -- ^ Match CBinaryOp by operator text
  | CAnyCmpPat CExprPat CExprPat          -- ^ Match any comparison operator
  | CAssignPat CExprPat CExprPat          -- ^ Match CAssign (any op) lhs rhs
  | CUnaryPat Text CExprPat               -- ^ Match CUnaryOp by operator text
  | CCastPat Text CExprPat                -- ^ Match CCast by type name (regex)
  | CContains CExprPat                    -- ^ Recursive search: match if subexpr matches
  | CNotPat CExprPat                      -- ^ Negation: succeed if inner fails
  | COrPat CExprPat CExprPat             -- ^ Alternative: try first, then second
  | CAndPat CExprPat CExprPat            -- ^ Both must match the same expression
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | Pattern for matching C AST statements.
-- Supports matching into nested blocks (loop bodies, if branches)
-- and sequencing with CStar for skipping.
data CStmtPat
  = CStmtExprPat CExprPat                 -- ^ Match CExprStmt containing expr
  | CVarDeclPat Text Text (Maybe CExprPat) -- ^ Match CVarDecl: type regex, name regex, init
  | CIfPat CExprPat [CStmtPat]            -- ^ Match CIf: cond pattern, body patterns
  | CForPat ForPat                         -- ^ Match CFor with structured sub-patterns
  | CWhilePat CExprPat [CStmtPat]         -- ^ Match CWhile: cond, body patterns
  | CDoWhilePat [CStmtPat] CExprPat       -- ^ Match CDoWhile: body patterns, cond
  | CAnyLoopPat [CStmtPat]                -- ^ Match any loop kind, search body
  | CReturnPat CExprPat                   -- ^ Match CReturn with expr pattern
  | CStar                                  -- ^ Skip 0+ statements at current block level
  | CSequence [CStmtPat]                  -- ^ Ordered sequence of patterns
  | CBodyContains CStmtPat                -- ^ Recursive search into nested blocks
  | CStmtOr CStmtPat CStmtPat            -- ^ Alternative
  | CStmtNot CStmtPat                    -- ^ Negation: succeed if inner fails on remaining stmts
  | CAvoidUntil CStmtPat CStmtPat        -- ^ Avoid first pattern while scanning until second matches
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | Dedicated for-loop pattern with access to all components.
-- This is the key addition over a generic DSL: it lets patterns directly
-- inspect the loop variable, bound, and increment, then search the body
-- for dangerous operations using the loop variable.
data ForPat = ForPat
  { initPat :: Maybe CExprPat    -- ^ Match init expression (bind loop var here)
  , condPat :: Maybe CExprPat    -- ^ Match condition (bind bound here)
  , incrPat :: Maybe CExprPat    -- ^ Match increment
  , bodyPat :: [CStmtPat]        -- ^ Patterns to find in body
  } deriving (Eq, Ord, Show, Generic, Hashable)

-- | A named vulnerability check: pattern + metadata.
-- Explanation and remediation are co-located with the check so that
-- adding a new check cannot forget to provide user-facing text.
data CAstCheck = CAstCheck
  { checkName   :: Text
  , severity    :: Severity
  , description :: Text
  , explanation :: Text       -- ^ Why this is a problem (shown in output)
  , remediation :: Text       -- ^ How to fix it (shown in output)
  , cwe         :: Maybe Int  -- ^ CWE identifier (e.g. 787 for CWE-787)
  , pattern_    :: CStmtPat
  } deriving (Eq, Ord, Show, Generic, Hashable)
