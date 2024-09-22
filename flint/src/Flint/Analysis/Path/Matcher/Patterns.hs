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

isArg :: ExprPattern
isArg = Var "arg"

bufferOverflow :: BugMatch
bufferOverflow = BugMatch
  { pathPattern =
      -- TODO: make this more general than just strcpy
      [ Stmt $ Call Nothing (CallFunc $ FuncName "strcpy") [Bind "dest" Wild, Bind "src" isArg]
      ]
  , bugName = "Buffer Overflow"
  , bugDescription =
    "The buffer at `" <> TextExpr "dest" <> "` can be overflowed by `" <> TextExpr "src" <> "` because the copy function has no bounds limit and `" <> TextExpr "src" <> "` could be user-controlled."
  , mitigationAdvice = "Use strncpy"
  }

formatStringVulnerability :: BugMatch
formatStringVulnerability = BugMatch
  { pathPattern =
      -- TODO: make this more general to match any stdlib func that takes a format str
      [ Stmt $ Call Nothing (CallFunc $ FuncName "printf") [Bind "arg" isArg]
      ]
  , bugName = "Potentially User Controlled Format String"
  , bugDescription =
    "The format string for printf is controlled by `" <> TextExpr "arg" <> "`, which could be user controlled."
  , mitigationAdvice = "Don't do it."
  }

memIsUsed :: Symbol -> StmtPattern
memIsUsed boundMemSymbol = AnyOne
  [ Stmt $ Call Nothing (CallFunc $ FuncName "strcpy") [v]
  -- TODO: other calls
  , Stmt $ Store v Wild
  -- TODO: look for statements that contain loads
  ]
  where
    -- maybe should be wrapped with Contains?
    v = Bind boundMemSymbol Wild

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


