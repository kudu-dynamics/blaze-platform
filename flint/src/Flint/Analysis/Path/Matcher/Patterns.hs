module Flint.Analysis.Path.Matcher.Patterns where

import Flint.Prelude hiding (const)

import Flint.Analysis.Path.Matcher
import Flint.Types.Query (BugMatch(..))

import Blaze.Pil.Construct hiding (not)

import qualified Data.HashSet as HashSet


-- | All the patterns that don't need to use the solver
allPatterns :: [BugMatch]
allPatterns =
  [ incrementWithoutCheck
  , stackSetToExecutable
  , bufferOverflow
  , formatStringVulnerability
  , useAfterFree
  -- , nullPointerDereference
  , stackBasedBufferOverflow
  , inputControlledIndirectCall
  , writeToGlobal
  , inputDataCopiedToStack
  ]
-- allPatterns =
--   [ incrementWithoutCheck
--   , inputControlledIndirectCall
--   , writeToGlobal
--   , inputDataCopiedToStack
--   ]


kernelModulePatterns :: [BugMatch]
kernelModulePatterns =
  [ usbRegisterNotifyImbalance
  , nfHookImbalance
  -- , simple
  ]

usbRegisterNotifyImbalance :: BugMatch
usbRegisterNotifyImbalance = BugMatch
  { pathPattern =
      [ Stmt $ Call Nothing (CallFunc $ FuncName "usb_register_notify") [ Bind "handler" Wild ]
      , AvoidUntil $ AvoidSpec
        { avoid = Stmt $ Call Nothing (CallFunc $ FuncName "usb_unregister_notify")
                  [ Bind "handler" Wild ]
        , until = EndOfPath
        }
      ]
  , bugName = "Dangling Usb Notify Handler in Kernel Module"
  , bugDescription =
      "There is a possible path through the lifecycle of this kernel module where the handler '" <> TextExpr "handler" <> "' is registered with 'usb_register_notify' but is never unregistered, leaving a dangling handler."
  , mitigationAdvice = "Unregister it."
  }

nfHookImbalance :: BugMatch
nfHookImbalance = BugMatch
  { pathPattern =
      [ Stmt $ Call Nothing (CallFunc $ FuncName "nf_register_net_hook") [ Bind "handler" Wild ]
      , AvoidUntil $ AvoidSpec
        { avoid = Stmt $ Call Nothing (CallFunc $ FuncName "nf_unregister_net_hook")
                  [ Bind "handler" Wild ]
        , until = EndOfPath
        }
      ]
  , bugName = "Dangling Nf Hook in Kernel Module"
  , bugDescription =
      "There is a possible path through the lifecycle of this kernel module where the handler '" <> TextExpr "handler" <> "' is registered with 'nf_register_net_hook' but is never unregistered, leaving a dangling handler."
  , mitigationAdvice = "Unregister it."
  }


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

writeToGlobal :: BugMatch
writeToGlobal = BugMatch
  { pathPattern =
      [ Stmt $ Call Nothing (CallFunc $ FuncName "_copy_from_user")
        [ Bind "out" isGlobal
        , Bind "in" (isGlobal .|| isArg)
        ]
      ]
      
  , bugName = "User input writes to kernel global"
  , bugDescription =
    "This path shows that possibly user-controllable memory from " <> TextExpr "in" <> " is written to a global in the kernel module: `" <> TextExpr "out" <> "`."
  , mitigationAdvice = "Try harder."
  }

inputDataCopiedToStack :: BugMatch
inputDataCopiedToStack = BugMatch
  { pathPattern =
      [ AvoidUntil $ AvoidSpec
        { avoid = Stmt $ Call (Just $ Bind "stackVar" Wild) (CallFunc $ FuncNameRegex "alloc") []
        , until = AnyOne
          [ Stmt $ Call Nothing (CallFunc $ FuncName "memcpy")
            [ Bind "stackVar" stackVar
            , Bind "src" (isGlobal .|| isArg)
            ]
            -- TODO: add regular STORE and other stdlib copies
          ]
        }
      ]
  , bugName = "User input writes to stack"
  , bugDescription =
    "This path shows that possibly user-controllable memory from " <> TextExpr "src" <> " is written to memory on the stack: `" <> TextExpr "stackVar" <> "`."
  , mitigationAdvice = "Try harder."
  }
  where
    stackVar = NotPattern (isGlobal .|| Contains isArg)

inputControlledIndirectCall :: BugMatch
inputControlledIndirectCall = BugMatch
  { pathPattern =
      [ AnyOne
        [ Stmt $ Call Nothing (CallIndirect $ Bind "callTarget" Wild) []
        -- , Stmt $ Call Nothing (CallFunc $ FuncNameRegex "^.*$") []
        ]
      ]
  , bugName = "Input Controlled Indirect Call"
  , bugDescription =
    "This path shows an indirect call to `" <> TextExpr "callTarget" <> "` which is controlled by an input to the function."
  , mitigationAdvice = "Be careful."
  }
  where
    inputDest = Contains isArg .|| load isGlobal ()


stackSetToExecutable :: BugMatch
stackSetToExecutable = BugMatch
  { pathPattern =
      [ Stmt $ Call Nothing (CallFunc $ FuncName "fopen") [constStr "/proc/self/maps" ()]
      , Stmt $ Call Nothing (CallFunc $ FuncName "mprotect") [Wild, Wild, const 7 ()]
      ]
  , bugName = "Stack Set to Executable"
  , bugDescription = "The stack might be set to executable in this function."
  , mitigationAdvice = "Be careful."
  }

stackSetToExecutable' :: BugMatch
stackSetToExecutable' = BugMatch
  { pathPattern =
      [ -- Stmt $ Call Nothing (CallFunc $ FuncName "fopen") [constStr "/proc/self/maps" ()]
       Stmt $ Call Nothing (CallFunc $ FuncName "mprotect") [Wild, Wild, const 7 ()]
      ]
  , bugName = "Stack Set to Executable"
  , bugDescription = "The stack might be set to executable in this function."
  , mitigationAdvice = "Be careful."
  }


-----------------------
-- patterns from dirty test binary
---------------------------------

-- | In Binja, I've seen a global be a Const or a ConstPtr.
-- Since this is just matching on an "immediate" the context will be important,
-- like it'll need to be in a place that expects a pointer.
isGlobal :: ExprPattern
isGlobal = Immediate

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

formatStringCallPattern' :: StmtPattern
formatStringCallPattern' = AnyOne
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
    arg = Bind "arg" isArg
    firstArg = [arg]
    secondArg = [Wild, arg]
    thirdArg = [Wild, Wild, arg]

formatStringCallPattern'' :: ExprPattern -> StmtPattern
formatStringCallPattern'' argPat = AnyOne
  [ Stmt $ Call Nothing (CallFunc $ FuncNames firstArgFuncs) firstArg
  , Stmt $ Call Nothing (CallFunc $ FuncNames secondArgFuncs) secondArg
  , Stmt $ Call Nothing (CallFunc $ FuncNames thirdArgFuncs) thirdArg
  ]
  where
    firstArg = [argPat]
    secondArg = [Wild, argPat]
    thirdArg = [Wild, Wild, argPat]
    firstArgFuncs = HashSet.fromList
      [ "printf"
      , "vprintf"
      , "scanf"
      , "vscanf"
      ]
    secondArgFuncs = HashSet.fromList
      [ "fprintf"
      , "sprintf"
      , "vfprintf"
      , "vsprintf"
      , "fscanf"
      , "sscanf"
      , "vfscanf"
      , "vsscanf"
      , "asprintf"
      , "vasprintf"
      ]
    thirdArgFuncs = HashSet.fromList
      [ "snprintf"
      , "vsnprintf"
      ]

formatStringVulnerability :: BugMatch
formatStringVulnerability = BugMatch
  { pathPattern =
      [
        -- Stmt $ Call Nothing (CallFunc $ FuncNames badFuncs) []
        -- formatStringCallPattern "arg" isArg
        -- formatStringCallPattern'
        formatStringCallPattern'' (Bind "arg" isArg)
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


-- This is mainly to be used with the Use After Free pattern.
-- This is looking for a store or load
-- It also flags any call to a func where the arg is passed in, which
-- will produce some false positives until we have a way to ensure it's
-- not passing in &ptr instead of just ptr.
memIsUsed :: ExprPattern -> StmtPattern
memIsUsed memPat = AnyOne
  [ anyArg v $ Call Nothing (CallFunc $ FuncNameRegex ".*")
  , anyArg v $ EnterContext AnyCtx
  , Stmt $ Store v Wild

  -- Do we really care if v is wrapped in a LOAD?
  , Stmt $ Store Wild v'
  , Stmt $ Def Wild v'
  , Stmt $ BranchCond v'
  , Stmt $ Jump v'
  , Stmt $ Ret v'
  , Stmt $ Constraint v'
  ]
  where
    v = Contains memPat
    v' = Contains $ load v ()

pointerAssigned :: ExprPattern -> StmtPattern
pointerAssigned ptrPat = AnyOne
  [ Stmt $ Def ptrPat Wild
  
  -- TODO: handle these calls that take **ptr and set it to newly allocated mem:
  -- [ ("getline", 0)
  -- , ("getdelim", 0)
  -- , ("asprintf", 0)
  -- , ("vasprintf", 0)
  -- , ("getaddrinfo", 3)
  -- ]
  ]
  
useAfterFree :: BugMatch
useAfterFree = BugMatch
  { pathPattern =
      -- TODO: make this more general to match any stdlib func that takes a format str
      [ Stmt $ Call Nothing (CallFunc $ FuncName "free") [Bind "ptr" Wild]
      , AvoidUntil $ AvoidSpec
        { avoid = pointerAssigned $ Bind "ptr" Wild
        , until = memIsUsed $ Bind "ptr" Wild
        }
      ]
  , bugName = "Use after free"
  , bugDescription =
    "The pointer `" <> TextExpr "ptr" <> "` is freed and is later used."
  , mitigationAdvice = "Don't."
  }

badboyz :: HashSet Text
badboyz = HashSet.fromList . fmap fst $ badboys

badboys :: [(Text, Int)]
badboys =
  [ ("strcpy", firstArg)
    , ("strcpy", secondArg)
    , ("strcat", firstArg)
    , ("strcat", secondArg)
    , ("strcmp", firstArg)
    , ("strcmp", secondArg)
    , ("strncmp", firstArg)
    , ("strncmp", secondArg)
    , ("strncpy", firstArg)
    , ("strncpy", secondArg)
    , ("strncat", firstArg)
    , ("strncat", secondArg)
    , ("strlen", firstArg)
    , ("strchr", firstArg)
    , ("strrchr", firstArg)
    , ("strstr", firstArg)
    , ("strstr", secondArg)
    , ("strdup", firstArg)
    , ("strndup", firstArg)
    , ("strtok", firstArg)
    , ("strtok", secondArg)
    , ("memcpy", firstArg)
    , ("memcpy", secondArg)
    , ("memmove", firstArg)
    , ("memmove", secondArg)
    , ("memcmp", firstArg)
    , ("memcmp", secondArg)
    , ("memset", firstArg)
    , ("memchr", firstArg)
    , ("fopen", firstArg)
    , ("fopen", secondArg)
    , ("fclose", firstArg)
    , ("fread", firstArg)
    , ("fread", fourthArg)
    , ("fwrite", firstArg)
    , ("fwrite", fourthArg)
    , ("fprintf", firstArg)
    , ("fprintf", secondArg)
    , ("fscanf", firstArg)
    , ("fscanf", secondArg)
    , ("scanf", firstArg)
    , ("printf", firstArg)
    , ("sprintf", firstArg)
    , ("sprintf", secondArg)
    , ("snprintf", firstArg)
    , ("snprintf", thirdArg)
    , ("fgetc", firstArg)
    , ("fgets", firstArg)
    , ("fgets", thirdArg)
    , ("fputc", secondArg)
    , ("fputs", firstArg)
    , ("fputs", secondArg)
    , ("fseek", firstArg)
    , ("ftell", firstArg)
    , ("rewind", firstArg)
    , ("feof", firstArg)
    , ("ferror", firstArg)
    , ("clearerr", firstArg)
    , ("fgetpos", firstArg)
    , ("fsetpos", firstArg)
    , ("perror", firstArg)
    , ("strerror", firstArg)
    ]
  where
    firstArg = 0
    secondArg = 1
    thirdArg = 2
    fourthArg = 3

shouldn'tGetPassedNullPtr :: ExprPattern -> StmtPattern
shouldn'tGetPassedNullPtr argPat = AnyOne
  . fmap (\(name, args) -> Stmt $ Call Nothing (CallFunc $ FuncName name) args)
  $ [ ("strcpy", firstArg)
    , ("strcpy", secondArg)
    , ("strcat", firstArg)
    , ("strcat", secondArg)
    , ("strcmp", firstArg)
    , ("strcmp", secondArg)
    , ("strncmp", firstArg)
    , ("strncmp", secondArg)
    , ("strncpy", firstArg)
    , ("strncpy", secondArg)
    , ("strncat", firstArg)
    , ("strncat", secondArg)
    , ("strlen", firstArg)
    , ("strchr", firstArg)
    , ("strrchr", firstArg)
    , ("strstr", firstArg)
    , ("strstr", secondArg)
    , ("strdup", firstArg)
    , ("strndup", firstArg)
    , ("strtok", firstArg)
    , ("strtok", secondArg)
    , ("memcpy", firstArg)
    , ("memcpy", secondArg)
    , ("memmove", firstArg)
    , ("memmove", secondArg)
    , ("memcmp", firstArg)
    , ("memcmp", secondArg)
    , ("memset", firstArg)
    , ("memchr", firstArg)
    , ("fopen", firstArg)
    , ("fopen", secondArg)
    , ("fclose", firstArg)
    , ("fread", firstArg)
    , ("fread", fourthArg)
    , ("fwrite", firstArg)
    , ("fwrite", fourthArg)
    , ("fprintf", firstArg)
    , ("fprintf", secondArg)
    , ("fscanf", firstArg)
    , ("fscanf", secondArg)
    , ("scanf", firstArg)
    , ("printf", firstArg)
    , ("sprintf", firstArg)
    , ("sprintf", secondArg)
    , ("snprintf", firstArg)
    , ("snprintf", thirdArg)
    , ("fgetc", firstArg)
    , ("fgets", firstArg)
    , ("fgets", thirdArg)
    , ("fputc", secondArg)
    , ("fputs", firstArg)
    , ("fputs", secondArg)
    , ("fseek", firstArg)
    , ("ftell", firstArg)
    , ("rewind", firstArg)
    , ("feof", firstArg)
    , ("ferror", firstArg)
    , ("clearerr", firstArg)
    , ("fgetpos", firstArg)
    , ("fsetpos", firstArg)
    , ("perror", firstArg)
    , ("strerror", firstArg)
    ]
  where
    firstArg = [argPat]
    secondArg = [Wild, argPat]
    thirdArg = [Wild, Wild, argPat]
    fourthArg = [Wild, Wild, Wild, argPat]

shouldn'tGetPassedNullPtrFirstArgFuncs :: HashSet Text
shouldn'tGetPassedNullPtrFirstArgFuncs = HashSet.fromList
  [ "strcpy"
  , "strcat"
  , "strcmp"
  , "strncmp"
  , "strncpy"
  , "strncat"
  , "strlen"
  , "strchr"
  , "strrchr"
  , "strstr"
  , "strdup"
  , "strndup"
  , "strtok"
  , "memcpy"
  , "memmove"
  , "memcmp"
  , "memset"
  , "memchr"
  , "fopen"
  , "fclose"
  , "fread"
  , "fwrite"
  , "fprintf"
  , "fscanf"
  , "scanf"
  , "printf"
  , "sprintf"
  , "snprintf"
  , "fgetc"
  , "fgets"
  , "fputs"
  , "fseek"
  , "ftell"
  , "rewind"
  , "feof"
  , "ferror"
  , "clearerr"
  , "fgetpos"
  , "fsetpos"
  , "perror"
  , "strerror"
  ]

shouldn'tGetPassedNullPtrSecondArgFuncs :: HashSet Text
shouldn'tGetPassedNullPtrSecondArgFuncs = HashSet.fromList
  [ "strcpy"
  , "strcat"
  , "strcmp"
  , "strncmp"
  , "strncpy"
  , "strncat"
  , "strstr"
  , "strtok"
  , "memcpy"
  , "memmove"
  , "memcmp"
  , "fopen"
  , "fprintf"
  , "fscanf"
  , "sprintf"
  , "fputc"
  , "fputs"
  ]

shouldn'tGetPassedNullPtrThirdArgFuncs :: HashSet Text
shouldn'tGetPassedNullPtrThirdArgFuncs = HashSet.fromList
  [ "snprintf"
  , "fgets"
  ]

shouldn'tGetPassedNullPtrFourthArgFuncs :: HashSet Text
shouldn'tGetPassedNullPtrFourthArgFuncs = HashSet.fromList
  [ "fread"
  , "fwrite"
  ]

-- shouldn'tGetPassedNullPtr' :: ExprPattern -> StmtPattern
-- shouldn'tGetPassedNullPtr' argPat = AnyOne
--   [ Stmt $ Call Nothing (CallFunc $ FuncNames badboyz) fourthArg
--   ]
--   -- [ Stmt $ Call Nothing (CallFunc $ FuncNames shouldn'tGetPassedNullPtrFirstArgFuncs) firstArg
--   -- , Stmt $ Call Nothing (CallFunc $ FuncNames shouldn'tGetPassedNullPtrSecondArgFuncs) secondArg
--   -- , Stmt $ Call Nothing (CallFunc $ FuncNames shouldn'tGetPassedNullPtrThirdArgFuncs) thirdArg
--   -- , Stmt $ Call Nothing (CallFunc $ FuncNames shouldn'tGetPassedNullPtrFourthArgFuncs) fourthArg
--   -- ]
--   where
--     firstArg  = [argPat]
--     secondArg = [Wild, argPat]
--     thirdArg  = [Wild, Wild, argPat]
--     fourthArg = [Wild, Wild, Wild, argPat]

nullPtr :: ExprPattern
nullPtr = const 0 () .|| constPtr 0 ()

-- nullPointerDereference :: BugMatch
-- nullPointerDereference = BugMatch
--   { pathPattern =
--       [ AnyOne
--         [ Ordered
--           [ Stmt $ Def (Bind "ptr" Wild) nullPtr
--           , AvoidUntil $ AvoidSpec
--             { avoid = pointerAssigned $ Bind "ptr" Wild
--             , until = AnyOne
--                       [ shouldn'tGetPassedNullPtr' (Bind "ptr" Wild) ]
--             }
--           ]
--         , shouldn'tGetPassedNullPtr' (Bind "ptr" nullPtr)
--         ]
--       ]
--   , bugName = "Null Pointer Dereference"
--   , bugDescription =
--     "The pointer `" <> TextExpr "ptr" <> "` is null and is dereferenced."
--   , mitigationAdvice = "Don't do it."
--   }

simple :: BugMatch
simple = BugMatch
  { pathPattern =
    [ Stmt $ Def Wild Wild
    ]
  , bugName = "Simple"
  , bugDescription =
    "The pointer `" <> TextExpr "ptr" <> "` is null and is dereferenced."
  , mitigationAdvice = "Don't do it."
  }


-- Here are some funcs that copy between two buffers.
-- (func_name, dest, src, size)
-- TODO: use these in stack based buffer overflow check:
-- [ ("memcpy", 0, 1, 2)        -- void *dest, const void *src, size_t n
-- , ("memmove", 0, 1, 2)       -- void *dest, const void *src, size_t n
-- , ("memccpy", 0, 1, 3)       -- void *dest, const void *src, int c, size_t n
-- , ("strncpy", 0, 1, 2)       -- char *dest, const char *src, size_t n
-- , ("strncat", 0, 1, 2)       -- char *dest, const char *src, size_t n
-- , ("strlcpy", 0, 1, 2)       -- char *dest, const char *src, size_t size
-- , ("strlcat", 0, 1, 2)       -- char *dest, const char *src, size_t size
-- , ("bcopy", 1, 0, 2)         -- void *dest, const void *src, size_t n (Note: some implementations have src and dest swapped)
-- , ("memcpy_s", 0, 2, 3)      -- void *dest, rsize_t destsz, const void *src, rsize_t count
-- , ("memmove_s", 0, 2, 3)     -- void *dest, rsize_t destsz, const void *src, rsize_t count
-- , ("strncpy_s", 0, 2, 3)     -- char *dest, rsize_t destsz, const char *src, rsize_t count
-- , ("strncat_s", 0, 2, 3)     -- char *dest, rsize_t destsz, const char *src, rsize_t count
-- ]

-- TODO: make more general
stackBasedBufferOverflow :: BugMatch
stackBasedBufferOverflow = BugMatch
  { pathPattern =
      [ Stmt $ Call (Just $ Bind "len" Wild) (CallFunc $ FuncName "strlen") [Bind "buf" isArg]
      , Stmt $ Call Nothing (CallFunc $ FuncName "memcpy") [Wild, Bind "buf" isArg, Bind "len" Wild]
      ]
  , bugName = "Stack Based Buffer Overflow"
  , bugDescription =
    "There might be an overflow. Sorry."
  , mitigationAdvice = "Don't do it."
  }
