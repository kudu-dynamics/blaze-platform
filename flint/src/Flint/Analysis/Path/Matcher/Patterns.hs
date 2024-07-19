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
