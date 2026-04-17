module Flint.Analysis.CAst.Patterns
  ( allChecks
  , loopArrayOOB
  , loopOffByOne
  , loopAccumulatingWrite
  , loopUseAfterFree
  , loopDoubleFree
  , loopMemoryLeak
  , loopUnboundedInput
  , loopMissingBoundsCheck
  , dangerousFunction
  , uncheckedAlloc
  , formatStringVuln
  ) where

import Flint.Prelude

import Flint.Types.Analysis.CAst.Matcher
import qualified Data.HashSet as HashSet


allChecks :: [CAstCheck]
allChecks =
  [ loopArrayOOB
  , loopOffByOne
  , loopAccumulatingWrite
  , loopUseAfterFree
  , loopDoubleFree
  , loopMemoryLeak
  , loopUnboundedInput
  , loopMissingBoundsCheck
  , dangerousFunction
  , uncheckedAlloc
  , formatStringVuln
  ]


-- ---------------------------------------------------------------------------
-- Loop-based patterns (primary: these exploit the C AST's loop structure)
-- ---------------------------------------------------------------------------

-- | Array index in loop may exceed bounds.
-- Matches: for(i=...; i<bound; ...) { ... arr[i] ... }
-- The loop variable is used as an array index in the body.
loopArrayOOB :: CAstCheck
loopArrayOOB = CAstCheck
  { checkName = "loop-array-oob"
  , severity = High
  , description = "Array index in loop may exceed allocation bounds"
  , explanation = "Loop variable used as array index may exceed allocation bounds."
  , remediation = "Verify loop bound <= array size, or add an explicit bounds check."
  , cwe = Just 787  -- CWE-787: Out-of-bounds Write
  , pattern_ = CSequence
      [ CStar
      , CForPat ForPat
          { initPat = Just (CBind "loopVar" CWild)
          , condPat = Just (CAnyCmpPat (CBound "loopVar") (CBind "bound" CWild))
          , incrPat = Nothing
          , bodyPat =
              [ CBodyContains
                  (CStmtExprPat (CContains (CIndexPat CWild (CUses "loopVar"))))
              ]
          }
      ]
  }

-- | Off-by-one: loop uses <= instead of < with array access.
-- Matches: for(i=0; i<=n; ...) { ... arr[i] ... }
loopOffByOne :: CAstCheck
loopOffByOne = CAstCheck
  { checkName = "loop-off-by-one"
  , severity = High
  , description = "Loop condition uses <= which may cause off-by-one array access"
  , explanation = "Loop uses '<=' instead of '<', writing one element past the array end."
  , remediation = "Change '<=' to '<' or allocate one extra element."
  , cwe = Just 193  -- CWE-193: Off-by-one Error
  , pattern_ = CSequence
      [ CStar
      , CForPat ForPat
          { initPat = Just (CBind "loopVar" CWild)
          , condPat = Just (CBinOpPat "<=" (CBound "loopVar") (CBind "bound" CWild))
          , incrPat = Nothing
          , bodyPat =
              [ CBodyContains
                  (CStmtExprPat (CContains (CIndexPat CWild (CUses "loopVar"))))
              ]
          }
      ]
  }

-- | Accumulating buffer write in loop.
-- Matches: loop { strcat(buf, ...); } or loop { strncat(buf, ...); }
loopAccumulatingWrite :: CAstCheck
loopAccumulatingWrite = CAstCheck
  { checkName = "loop-accumulating-write"
  , severity = High
  , description = "strcat/strncat/memcpy in loop body may overflow destination buffer"
  , explanation = "String append (strcat/memcpy) in loop accumulates data that may overflow buffer."
  , remediation = "Track cumulative length; use snprintf or check remaining buffer space."
  , cwe = Just 120  -- CWE-120: Buffer Copy without Checking Size of Input
  , pattern_ = CSequence
      [ CStar
      , CAnyLoopPat
          [ CBodyContains
              (CStmtExprPat (CContains
                (CCallNamesPat dangerousAppendFuncs [])))
          ]
      ]
  }

-- | Use-after-free in loop: free(ptr) then ptr used again without reassignment.
-- Matches: loop { ... free(ptr) ... use(ptr) ... }
-- The freed pointer is used in the same or next iteration.
loopUseAfterFree :: CAstCheck
loopUseAfterFree = CAstCheck
  { checkName = "loop-use-after-free"
  , severity = Critical
  , description = "free() in loop body followed by use of freed pointer"
  , explanation = "Freed pointer is used again in the same or next loop iteration."
  , remediation = "Set ptr = NULL after free(), or restructure to avoid reuse."
  , cwe = Just 416  -- CWE-416: Use After Free
  , pattern_ = CSequence
      [ CStar
      , CAnyLoopPat
          [ CStar
          , CStmtExprPat (CContains
              (CCallNamesPat freeFuncs [CBind "ptr" CWild]))
          -- After the free, look for a use of ptr without ptr being reassigned first
          , CAvoidUntil
              (CStmtExprPat (CAssignPat (CBound "ptr") CWild))  -- avoid reassignment
              (CStmtExprPat (CContains (CUses "ptr")))           -- until use of ptr
          ]
      ]
  }

-- | Double free in loop: free(ptr) without ptr=NULL or break.
-- Matches: loop { ... free(ptr) ... } where no null assignment follows.
loopDoubleFree :: CAstCheck
loopDoubleFree = CAstCheck
  { checkName = "loop-double-free"
  , severity = Critical
  , description = "free() in loop body without ptr=NULL or break -- double free on next iteration"
  , explanation = "free() in loop without ptr=NULL guard frees the same pointer on next iteration."
  , remediation = "Set ptr = NULL immediately after free(), or break out of the loop."
  , cwe = Just 415  -- CWE-415: Double Free
  , pattern_ = CSequence
      [ CStar
      , CAnyLoopPat
          [ CStar
          , CStmtExprPat (CContains
              (CCallNamesPat freeFuncs [CBind "ptr" CWild]))
          -- Scan to end of body: fail if ptr=NULL appears (safe), succeed
          -- if we reach end without nulling ptr (double free on next iter).
          , CStmtNot (CBodyContains
              (CStmtExprPat (CAssignPat (CBound "ptr") (CNamePat "NULL"))))
          ]
      ]
  }

-- | Memory leak in loop: alloc in body without free in same body.
-- Matches: loop { ptr = malloc(...); ... } where no free() exists in the loop body.
loopMemoryLeak :: CAstCheck
loopMemoryLeak = CAstCheck
  { checkName = "loop-memory-leak"
  , severity = Medium
  , description = "Allocation in loop body without corresponding free -- leak per iteration"
  , explanation = "Allocation in loop body without corresponding free leaks memory each iteration."
  , remediation = "Free the allocation before next iteration or after the loop."
  , cwe = Just 401  -- CWE-401: Missing Release of Memory after Effective Lifetime
  , pattern_ = CSequence
      [ CStar
      , CAnyLoopPat
          -- Find ptr = alloc() in the loop body, then check that free(ptr)
          -- does NOT appear anywhere in the same body.
          [ CStar
          , CStmtExprPat (CAssignPat
              (CBind "ptr" CWild)
              (CCallNamesPat allocFuncs []))
          , CStmtNot (CBodyContains
              (CStmtExprPat (CContains (CCallNamesPat freeFuncs [CBound "ptr"]))))
          ]
      ]
  }

-- | Unbounded loop with external input as bound.
-- Matches: for/while where condition involves return value of recv/read/strlen
-- without prior validation.
loopUnboundedInput :: CAstCheck
loopUnboundedInput = CAstCheck
  { checkName = "loop-unbounded-input"
  , severity = High
  , description = "Loop bound derived from external input (recv/read/strlen) without cap"
  , explanation = "Loop bound comes from external input without a maximum cap."
  , remediation = "Cap the bound: min(user_value, MAX_ALLOWED)."
  , cwe = Just 606  -- CWE-606: Unchecked Input for Loop Condition
  , pattern_ = CSequence
      [ CStar
      , CForPat ForPat
          { initPat = Nothing
          , condPat = Just (CAnyCmpPat CWild
              (CContains (CCallNamesPat inputFuncs [])))
          , incrPat = Nothing
          , bodyPat = []
          }
      ]
  }

-- | Missing bounds check on array access in while-loop.
-- Matches: while (non-comparison-cond) { ... arr[idx] ... }
-- Flags while-loops where the loop condition is not a comparison (e.g., pointer != NULL)
-- but the body contains array indexing.
loopMissingBoundsCheck :: CAstCheck
loopMissingBoundsCheck = CAstCheck
  { checkName = "loop-missing-bounds-check"
  , severity = Medium
  , description = "Array access in while-loop whose condition is not a bounds comparison"
  , explanation = "Array access in loop whose condition does not check array bounds."
  , remediation = "Add 'if (idx >= size) break;' before the array access."
  , cwe = Just 129  -- CWE-129: Improper Validation of Array Index
  , pattern_ = CSequence
      [ CStar
      , CWhilePat (CNotPat (CAnyCmpPat CWild CWild))
          [ CBodyContains
              (CStmtExprPat (CContains (CIndexPat CWild CWild)))
          ]
      ]
  }


-- ---------------------------------------------------------------------------
-- Secondary (completeness, not loop-specific)
-- ---------------------------------------------------------------------------

-- | Dangerous function calls (gets, strcpy, sprintf, etc.)
dangerousFunction :: CAstCheck
dangerousFunction = CAstCheck
  { checkName = "dangerous-function"
  , severity = Medium
  , description = "Call to function with known buffer overflow risk"
  , explanation = "Function does not check destination buffer size (gets, strcpy, sprintf, etc.)."
  , remediation = "Replace: gets->fgets, strcpy->strncpy, sprintf->snprintf."
  , cwe = Just 676  -- CWE-676: Use of Potentially Dangerous Function
  , pattern_ = CSequence
      [ CStar
      , CStmtExprPat (CContains
          (CCallNamesPat dangerousFuncs []))
      ]
  }

-- | malloc/calloc/realloc without NULL check.
uncheckedAlloc :: CAstCheck
uncheckedAlloc = CAstCheck
  { checkName = "unchecked-alloc"
  , severity = Low
  , description = "Allocation result used without NULL check"
  , explanation = "Allocation result used without NULL check; null deref if allocation fails."
  , remediation = "Check return value for NULL before using the pointer."
  , cwe = Just 252  -- CWE-252: Unchecked Return Value
  , pattern_ = CSequence
      [ CStar
      , CStmtExprPat (CAssignPat
          (CBind "ptr" CWild)
          (CCallNamesPat allocFuncs []))
      -- Scan forward: fail if we find a NULL check for ptr; succeed if ptr
      -- is used without any NULL check in between.
      , CAvoidUntil
          (CIfPat (CAnyCmpPat (CBound "ptr") (CNamePat "NULL")) [])
          (CStmtExprPat (CContains (CUses "ptr")))
      ]
  }


-- | Format string vulnerability: printf-family called with non-literal format.
-- Matches: printf(var) or fprintf(fp, var) where the format arg is not a string literal.
formatStringVuln :: CAstCheck
formatStringVuln = CAstCheck
  { checkName = "format-string"
  , severity = High
  , description = "printf-family called with non-literal format string -- format string attack"
  , explanation = "Non-literal format string allows attacker-controlled format specifiers."
  , remediation = "Use literal format: printf(\"%s\", str) instead of printf(str)."
  , cwe = Just 134  -- CWE-134: Use of Externally-Controlled Format String
  , pattern_ = CSequence
      [ CStar
      , CStmtExprPat (CContains
          (CCallNamesPat printfFuncs [CNotPat CLitStringPat]))
      ]
  }


-- ---------------------------------------------------------------------------
-- Function name sets
-- ---------------------------------------------------------------------------

freeFuncs :: HashSet Text
freeFuncs = HashSet.fromList
  [ "free", "cfree", "cgc_free", "g_free", "kfree"
  , "HeapFree", "GlobalFree", "LocalFree"
  ]

allocFuncs :: HashSet Text
allocFuncs = HashSet.fromList
  [ "malloc", "calloc", "realloc", "cgc_malloc", "cgc_calloc"
  , "g_malloc", "g_malloc0", "kmalloc", "kzalloc"
  , "HeapAlloc", "GlobalAlloc", "LocalAlloc"
  ]

dangerousFuncs :: HashSet Text
dangerousFuncs = HashSet.fromList
  [ "gets", "strcpy", "strcat", "sprintf", "vsprintf"
  , "scanf", "fscanf", "sscanf"
  , "cgc_strcpy", "cgc_strcat"
  ]

dangerousAppendFuncs :: HashSet Text
dangerousAppendFuncs = HashSet.fromList
  [ "strcat", "strncat", "cgc_strcat"
  , "memcpy", "memmove", "cgc_memcpy"
  ]

printfFuncs :: HashSet Text
printfFuncs = HashSet.fromList
  [ "printf", "cgc_printf"
  , "syslog", "err", "warn"
  ]

inputFuncs :: HashSet Text
inputFuncs = HashSet.fromList
  [ "recv", "read", "fread", "gets", "fgets"
  , "strlen", "cgc_strlen"
  , "cgc_receive", "cgc_read"
  ]


