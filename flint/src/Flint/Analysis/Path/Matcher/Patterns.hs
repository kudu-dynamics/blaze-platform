module Flint.Analysis.Path.Matcher.Patterns where

import Flint.Prelude

import Flint.Analysis.Path.Matcher
import Flint.Types.Query (BugMatch(..))

import Blaze.Pil.Construct hiding (not)


incrementWithoutCheck :: BugMatch
incrementWithoutCheck = BugMatch
  { pathPattern =
      [ AvoidUntil $ AvoidSpec
        { avoid = Stmt . Constraint
                  $   (Contains (load (Bind "ptr" Wild) ()) .< Wild)
                  .|| (Contains (load (Bind "ptr" Wild) ()) .<= Wild)
                  .|| (Contains (load (Bind "ptr" Wild) ()) .> Wild)
                  .|| (Contains (load (Bind "ptr" Wild) ()) .>= Wild)
                  .|| (Contains (load (Bind "ptr" Wild) ()) .== Wild)
                  .|| (Contains (load (Bind "ptr" Wild) ()) ./= Wild)
        , until = Ordered
          [ Stmt $ Store (Bind "ptr" Wild) (add (load (Bind "ptr" Wild) ()) (Bind "n" Wild) ())
          ]
        }
      ]
      
  , bugName = "Increment Without Check"
  , bugDescription =
    "This path shows an increment of " <> TextExpr "n" <> " to the memory location `" <> TextExpr "ptr" <> "` without a bounds check. This could lead to an integer overflow."
  , mitigationAdvice = "Add a bounds check."
  }

oobWrite :: Func -> BugMatch
oobWrite mallocFunc = BugMatch
  { pathPattern =
      [ Stmt $ Call (Just $ Bind "ptr" Wild) (CallFunc mallocFunc) [Bind "sz" Wild]
      , Stmt $ Store (Bind "ptr" Wild) (add (load (Bind "ptr" Wild) ()) (Bind "n" Wild) ())
      ]
      
  , bugName = "Increment Without Check"
  , bugDescription =
    "This path shows an increment of " <> TextExpr "n" <> " to the memory location `" <> TextExpr "ptr" <> "` without a bounds check. This could lead to an integer overflow."
  , mitigationAdvice = "Add a bounds check."
  }

---------------------------------

isArg :: ExprPattern
isArg = Var "arg"

overFlowCopyingFunc :: Symbol -> Symbol -> ExprPattern -> StmtPattern
overFlowCopyingFunc destArgBindName srcArgBindName srcArgCheck = AnyOne
  [ Stmt $ Call Nothing (CallFunc $ FuncName "strcpy")
    [ Bind destArgBindName Wild, Bind srcArgBindName srcArgCheck ] 
  , Stmt $ Call Nothing (CallFunc $ FuncName "strcat")
    [ Bind destArgBindName Wild, Bind srcArgBindName srcArgCheck ] 
  -- TODO: scanf with %s in fmt string
  ]

bufferOverflow :: BugMatch
bufferOverflow = BugMatch
  { pathPattern =
      -- TODO: make this more general than just strcpy
      [ overFlowCopyingFunc "dest" "src" isArg
      ]
  , bugName = "Buffer Overflow"
  , bugDescription =
    "The buffer at `" <> TextExpr "dest" <> "` can be overflowed by `" <> TextExpr "src" <> "` because the copy function has no bounds limit and `" <> TextExpr "src" <> "` could be user-controlled."
  , mitigationAdvice = "Limit the length of the copy."
  }

formatStringCallPattern :: Symbol -> ExprPattern -> StmtPattern
formatStringCallPattern formatStringArgBindName formatStringArgCheck = AnyOne
  . fmap (\(name, args) -> Stmt $ Call Nothing (CallFunc $ FuncName name) args)
  $ [ ("printf"   , firstArg)
    , ("fprintf"  , secondArg)
    , ("sprintf"  , secondArg)
    , ("snprintf" , thirdArg)
    , ("vprintf"  , firstArg)
    , ("vfprintf" , secondArg)
    , ("vsprintf" , secondArg)
    , ("vsnprintf", thirdArg)
    , ("scanf"    , firstArg)
    , ("fscanf"   , secondArg)
    , ("sscanf"   , secondArg)
    , ("vscanf"   , firstArg)
    , ("vfscanf"  , secondArg)
    , ("vsscanf"  , secondArg)
    , ("asprintf" , secondArg)
    , ("vasprintf", secondArg)
    ]
  where
    arg = Bind formatStringArgBindName formatStringArgCheck
    firstArg = [arg]
    secondArg = [Wild, arg]
    thirdArg = [Wild, Wild, arg]

formatStringVulnerability :: BugMatch
formatStringVulnerability = BugMatch
  { pathPattern =
      [ formatStringCallPattern "arg" isArg
      ]
  , bugName = "Potentially User Controlled Format String"
  , bugDescription =
    "The format string for printf is controlled by `" <> TextExpr "arg" <> "`, which could be user controlled."
  , mitigationAdvice = "Don't do it."
  }


-- This is really stupid...
-- We have no way to match "any arg" at the moment
anyArg :: ExprPattern -> ([ExprPattern] -> Statement ExprPattern) -> StmtPattern
anyArg argPat mkCallDest = AnyOne
  [ stmt [argPat]
  , stmt [Wild, argPat]
  , stmt [Wild, Wild, argPat]
  , stmt [Wild, Wild, Wild, argPat]
  , stmt [Wild, Wild, Wild, Wild, argPat]
  , stmt [Wild, Wild, Wild, Wild, Wild, argPat]
  , stmt [Wild, Wild, Wild, Wild, Wild, Wild, argPat]
  ]
  where
    stmt = Stmt . mkCallDest 

memIsUsed :: Symbol -> StmtPattern
memIsUsed boundMemSymbol = AnyOne
  [ anyArg v $ Call Nothing (CallFunc $ FuncNameRegex ".*")
  , anyArg v $ EnterContext AnyCtx
  , Stmt $ Store v Wild
  , Stmt $ Store Wild v
  , Stmt $ Def Wild v
  , Stmt $ BranchCond v
  , Stmt $ Jump v
  , Stmt $ Ret v
  , Stmt $ Constraint v
  ]
  where
    v = Contains $ Bind boundMemSymbol Wild

useAfterFree :: BugMatch
useAfterFree = BugMatch
  { pathPattern =
      -- TODO: make this more general to match any stdlib func that takes a format str
      [ Stmt $ Call Nothing (CallFunc $ FuncName "free") [Bind "ptr" Wild]
      , memIsUsed "ptr"
      ]
  , bugName = "Use after free"
  , bugDescription =
    "The pointer `" <> TextExpr "ptr" <> "` if freed and is later used."
  , mitigationAdvice = "Don't."
  }




