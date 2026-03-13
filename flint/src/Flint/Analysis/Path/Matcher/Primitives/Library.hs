module Flint.Analysis.Path.Matcher.Primitives.Library where

import Flint.Prelude hiding (Location)

import Flint.Analysis.Path.Matcher
import qualified Flint.Analysis.Path.Matcher as M
import Flint.Analysis.Path.Matcher.Primitives.Library.PrimSpec
import Flint.Types.Analysis.Path.Matcher.Func
import Flint.Types.Analysis.Path.Matcher.Primitives

import Blaze.Pil.Construct hiding (not)

import qualified Data.HashSet as HashSet

allPrims :: [Prim]
allPrims =
  [ controlledFormatStringPrim
  , integerOverflowPrim
  , freeHeapPrim
  , allocHeapPrim
  , doubleFreePrim
  , controlledIndirectCallPrim
  , unboundedCopyFromUserPrim
  , writeToKernelGlobalPrim
  , writeWhatWherePrim
  , -- , escapedDataFromLock
    writePrim
  , returnsFreedPointerPrim
  , copyMemPrim
  , copyPtrPrim
  , copyFromGlobalPrim
  , -- detectStackExecPrim now uses StdLib only
    danglingPtrPrim
  ]

---------------------------

isArg :: ExprPattern
isArg = Param

-- Matches function input parameters and external global addresses
isInput :: ExprPattern
isInput = Param .|| GlobalAddr

{- | This indicates the expr is tainted by function input (args/globals)
TODO: maybe we can use CodeSummary when matching
Matches if an expression is tainted by any src
-}
fromInput :: ExprPattern
fromInput = TaintedBy Wild

------------------------
-- Inline versions (no Star, no isInput, no Ret requirements)
-- These use CallsPrimitive to match via the CallableWMI cache
-- rather than matching on specific function names.

controlledFormatStringPrim_ :: Prim
controlledFormatStringPrim_ = Prim
  { primType = controlledFormatStringSpec
  , stmtPattern =
      Location "call" . CallsPrimitive controlledFormatStringSpec $ 
        [ ("fmt", Bind "fmt" Wild) ]
  }

controlledFormatStringPrim :: Prim
controlledFormatStringPrim = Prim
  { primType = controlledFormatStringSpec
  , stmtPattern = ordered
      [ Star
      , Location "call" . SubPrimitive controlledFormatStringPrim_ $ 
        [ ("fmt", Bind "fmt" (Contains Param)) ]
      ]
  }

freeHeapPrim_ :: Prim
freeHeapPrim_ = Prim
  { primType = freeHeapSpec
  , stmtPattern =
      Location "free" . CallsPrimitive freeHeapSpec $ 
        [ ("ptr", Bind "ptr" Wild) ]
  }

freeHeapPrim :: Prim
freeHeapPrim = Prim
  { primType = freeHeapSpec
  , stmtPattern = ordered
      [ Star
      , Location "free" . SubPrimitive freeHeapPrim_ $ 
        [ ("ptr", Bind "ptr" (Contains isInput)) ]
      ]
  }

allocHeapPrim_ :: Prim
allocHeapPrim_ = Prim
  { primType = allocHeapSpec
  , stmtPattern =
      Location "alloc" . CallsPrimitive allocHeapSpec $ 
        [ ("ptr", Bind "ptr" Wild)
        , ("size", Bind "size" Wild)
        ]
  }

allocHeapPrim :: Prim
allocHeapPrim = Prim
  { primType = allocHeapSpec
  , stmtPattern = ordered
      [ Star
      -- | case 1: allocated ptr is returned
      -- | case 2: allocated ptr is copied to arg or global ptr
      , orr
        [ ordered
          [ Location "alloc" . SubPrimitive allocHeapPrim_ $ 
            [ ("ptr", Bind "ptr" Wild)
            , ("size", Bind "size" Wild)
            ]
          , Star
          , Stmt . M.Ret $ Bound "ptr"
          ]
        , ordered
          [ Location "alloc" . SubPrimitive allocHeapPrim_ $
            [ ("ptr", Bind "ptr_alloc" Wild)
            , ("size", Bind "size" Wild)
            ]
          , Star
          , Stmt $ M.Store (Bind "ptr" $ Contains isInput) (Bound "ptr_alloc")
          ]
        ]
      ]
  }


copyMemPrim_ :: Prim
copyMemPrim_ = Prim
  { primType = copyMemSpec
  , stmtPattern =
      Location "copy" $ orr
        [ CallsPrimitive copyMemSpec
          [ ("dest_ptr", Bind "dest_ptr" Wild)
          , ("src_ptr", Bind "src_ptr" Wild)
          , ("len", Bind "len" Wild)
          ]
        , Stmt $ M.Store (Bind "dest_ptr" Wild) (BindWidth "len" (load (Bind "src_ptr" Wild) ()))
        ]
  }

copyMemPrim :: Prim
copyMemPrim = Prim
  { primType = copyMemSpec
  , stmtPattern = ordered
      [ Star
      -- TODO: handle possibly infinted CopyPtrs
      -- | case 1: func returns dest_ptr or it's copied to input
      -- | case 2: dest and src ptrs are both inputs/globals
      -- | case 3: dest is not global, but ptr gets copied to global
      , orr
        [ ordered
          [ Location "copy" . SubPrimitive copyMemPrim_ $ 
              [ ("dest_ptr", Bind "local_dest_ptr" Wild)
              , ("src_ptr", Bind "src_ptr" (Contains isInput))
              , ("len", Bind "len" Wild)
              ]
          , Star
          , orr
            [ Stmt . M.Ret  . Contains . Bind "dest_ptr" $ Bound "local_dest_ptr"
            -- | local ptr is copied out of func into global/arg ptr
            , Stmt $ Store (Bind "dest_ptr" (Contains isInput)) (Bound "local_dest_ptr")
            ]
          ]

        , ordered
          [ Location "copy" . SubPrimitive copyMemPrim_ $ 
              [ ("dest_ptr", Bind "dest_ptr" (Contains isInput))
              , ("src_ptr", Bind "src_ptr" (Contains isInput))
              , ("len", Bind "len" Wild)
              ]
          ]
        ]
      ]
  }

copyPtrPrim_ :: Prim
copyPtrPrim_ = Prim
  { primType = copyPtrSpec
  , stmtPattern =
      Location "copy" $ orr
        [ CallsPrimitive copyPtrSpec
          [ ("dest", Bind "dest" Wild)
          , ("copied_ptr", Bind "copied_ptr" Wild)
          ]
        , Stmt $ Store (Bind "dest" Wild)
          (OfType
           (PilType $ TPointer AnyBitWidth AnyType)
           (Bind "copied_ptr" Wild))
        ]
  }

copyPtrPrim :: Prim
copyPtrPrim = Prim
  { primType = copyPtrSpec
  , stmtPattern = ordered
      [ Star
      , Location "copy" . SubPrimitive copyPtrPrim_ $ 
          [ ("dest", Bind "dest" (Contains isInput))
          -- TODO: do we care if the ptr is a stack local vs global vs arg?
          -- for now, I'll assume that any ptr copied out might be valuable
          , ("copied_ptr", Bind "copied_ptr" Wild)
          ]
      ]
  }

doubleFreePrim_ :: Prim
doubleFreePrim_ = Prim
  { primType = doubleFreeSpec
  , stmtPattern =
      orr
        [ Location "free1" . Location "free2" . CallsPrimitive doubleFreeSpec $ 
          [ ("ptr", Bind "ptr" Wild) ]
        , ordered
          [ Location "free1" . SubPrimitive freeHeapPrim_ $ 
            [ ("ptr", Bind "ptr" Wild) ]
          , Star
          , AvoidUntil $ AvoidSpec
            { avoid = SubPrimitive allocHeapPrim_
                      [("ptr", Bound "ptr")]
            , until =
              Location "free2" . SubPrimitive freeHeapPrim_ $
              [ ("ptr", Bound "ptr") ]
            }
          ]
        ]
  }

doubleFreePrim :: Prim
doubleFreePrim = Prim
  { primType = doubleFreeSpec
  , stmtPattern =
      Location "free1" . Location "free2" . SubPrimitive doubleFreePrim_ $ 
          [ ("ptr", Bind "ptr" (Contains isInput)) ]
  }

writePrim_ :: Prim
writePrim_ = Prim
  { primType = writeSpec
  , stmtPattern =
      Location "write" $ orr
        [ CallsPrimitive writeSpec
          [ ("ptr", Bind "ptr" Wild)
          , ("value", Bind "value" Wild)
          ]
        , Stmt $ Store (Bind "ptr" Wild) (Bind "value" Wild)
        ]
  }

writePrim :: Prim
writePrim = Prim
  { primType = writeSpec
  , stmtPattern = ordered
      [ Star
      , Location "write" . SubPrimitive writePrim_ $ 
          [ ("ptr", Bind "ptr" (Contains isInput))
          , ("value", Bind "value" Wild)
          ]
      ]
  }

writeWhatWherePrim_ :: Prim
writeWhatWherePrim_ = Prim
  { primType = writeWhatWhereSpec
  , stmtPattern =
      Location "write" $ orr
        [ CallsPrimitive writeWhatWhereSpec
          [ ("dest", Bind "dest" Wild)
          , ("src", Bind "src" Wild)
          ]
        -- TODO: probably should have a `Contains`
        , Stmt $ Store (Bind "dest" Wild)
                       (load (Bind "src" (NotPattern $ Bound "dest")) ())
        ]
  }

writeWhatWherePrim :: Prim
writeWhatWherePrim = Prim
  { primType = writeWhatWhereSpec
  , stmtPattern = ordered
      [ Star
      , Location "write" . SubPrimitive writeWhatWherePrim_ $ 
          [ ("src", Bind "src" (Contains isInput))
          , ("dest", Bind "dest" (NotPattern $ Bound "src"))
          ]
      ]
  }


integerOverflowPrim :: Prim
integerOverflowPrim = Prim
  { primType = integerOverflowSpec
  , stmtPattern = ordered
      [ Star
      , orr
        [ Location "increment store" . CallsPrimitive integerOverflowSpec $ 
          [ ("ptr", Bind "ptr" Wild )
          , ("increment_by", Bind "increment_by" Wild)
          ]
        , AvoidUntil $ AvoidSpec
          { avoid = Stmt . Constraint
                    $   (Contains (load (Bound "ptr") ()) .< Wild)
                    .|| (Contains (load (Bound "ptr") ()) .<= Wild)
                    .|| (Contains (load (Bound "ptr") ()) .> Wild)
                    .|| (Contains (load (Bound "ptr") ()) .>= Wild)
                    .|| (Contains (load (Bound "ptr") ()) .== Wild)
                    .|| (Contains (load (Bound "ptr") ()) ./= Wild)
          , until = Location "increment store"
                    . Stmt $ Store
                           (Bind "ptr" Wild)
                           (add (load (Bound "ptr") ())
                            (Bind "increment_by" Wild) ())
          }
        ]
      ]
    }

returnsFreedPointerPrim_ :: Prim
returnsFreedPointerPrim_ = Prim
  { primType = returnsFreedPointerSpec
  , stmtPattern =
    orr
      [ Location "free" . CallsPrimitive returnsFreedPointerSpec $ 
        [ ("ptr", Bind "ptr" Wild) ]
      , ordered
        [ Location "free" . SubPrimitive freeHeapPrim_ $
          [ ("ptr", Bind "ptr" Wild) ]
        , Star
        , Location "return" . Stmt . M.Ret $ Bound "ptr"
        ]
      ]
  }

returnsFreedPointerPrim :: Prim
returnsFreedPointerPrim = Prim
  { primType = returnsFreedPointerSpec
  , stmtPattern = ordered
    [ Star
    , orr
      [ Location "free" . SubPrimitive freeHeapPrim_ $ 
        [ ("ptr", Bind "ptr" Wild) ]
      , Location "free" . SubPrimitive returnsFreedPointerPrim_ $ 
        [ ("ptr", Bind "ptr" Wild) ]
      ]
    , Star
    -- TODO: Avoid any resetting of the pointer to something else between free and return
    , Location "return" . Stmt . M.Ret $ Bound "ptr"
    ]
  }

-------------------------------------

-- TODO: add recursive CallsPrimitive
writeToKernelGlobalPrim_ :: Prim
writeToKernelGlobalPrim_ = Prim
  { primType = writeToKernelGlobalSpec
  , stmtPattern =
      Location "copy" . Stmt $ Call Nothing (CallFunc
                                . FuncNames
                                . HashSet.fromList
                                $ ["_copy_from_user", "copy_from_user"])
        [ Bind "dest" Wild
        , Bind "src" Wild
        , Bind "len" Wild
        ]
  }

writeToKernelGlobalPrim :: Prim
writeToKernelGlobalPrim = Prim
  { primType = writeToKernelGlobalSpec
  , stmtPattern = ordered
      [ Star
      , Location "copy" . SubPrimitive writeToKernelGlobalPrim_ $ 
          [ ("src", Bind "src" $ Contains isInput)
          , ("dest", Bind "dest" $ Contains GlobalAddr)
          , ("len", Bind "len" Wild)
          ]
      ]
  }

---------------

controlledIndirectCallPrim_ :: Prim
controlledIndirectCallPrim_ = Prim
  { primType = controlledIndirectCallSpec
  , stmtPattern =
      orr
        [ Location "callsite" . CallsPrimitive controlledIndirectCallSpec $ 
          [ ("call_dest", Bind "call_dest" Wild)
          ]
        , Location "callsite" . Stmt $ Call Nothing (CallIndirect $ Bind "call_dest" Wild) []
        ]
  }

controlledIndirectCallPrim :: Prim
controlledIndirectCallPrim = Prim
  { primType = controlledIndirectCallSpec
  , stmtPattern = ordered
      [ Star
      , Location "callsite" . SubPrimitive controlledIndirectCallPrim_ $ 
          [ ("call_dest", Bind "call_dest" (Contains isInput))
          ]
      ]
  }

danglingPtrPrim_ :: Prim
danglingPtrPrim_ = Prim
  { primType = danglingPtrSpec
  , stmtPattern =
      ordered
        [ Location "free" $ orr
          [ CallsPrimitive danglingPtrSpec
            [ ("ptr", Bind "ptr" Wild) ]
          , SubPrimitive freeHeapPrim
            [ ("ptr", load (Bind "ptr" Wild) ()) ]
          ]
        , AvoidUntil $ AvoidSpec
          { avoid = Stmt $ Store (Bound "ptr") Wild
          , until = ordered [Star, Stmt $ M.Ret Wild]
          }
        ]
  }

danglingPtrPrim :: Prim
danglingPtrPrim = Prim
  { primType = danglingPtrSpec
  , stmtPattern = ordered
      [ Star
      , Location "free" . SubPrimitive danglingPtrPrim_ $ 
        [ ("ptr", Bind "ptr" (Contains isInput)) ]
      ]
  }

----------------

kernelModulePrims :: [Prim]
kernelModulePrims = failedToUnregister

thingsYouShouldUnregister :: [((Text, Word64), (Text, Word64))]
thingsYouShouldUnregister =
  [ (("usb_register_notify", 0), ("usb_unregister_notify", 0))
  , (("nf_register_net_hook", 1), ("nf_unregister_net_hook", 1))
  , (("register_netdevice_notifier", 0), ("unregister_netdevice_notifier", 0))
  , (("register_inetaddr_notifier", 0), ("unregister_inetaddr_notifier", 0))
  , (("register_inet6addr_notifier", 0), ("unregister_inet6addr_notifier", 0))
  , (("register_filesystem", 0), ("unregister_filesystem", 0))
  , (("pci_register_driver", 0), ("pci_unregister_driver", 0))
  , (("platform_driver_register", 0), ("platform_driver_unregister", 0))
  , (("register_netdev", 0), ("unregister_netdev", 0))
  , (("dev_add_pack", 0), ("dev_remove_pack", 0))
  , (("register_reboot_notifier", 0), ("unregister_reboot_notifier", 0))
  , (("misc_register", 0), ("misc_deregister", 0))
  , (("register_shrinker", 0), ("unregister_shrinker", 0))
  , (("register_pm_notifier", 0), ("unregister_pm_notifier", 0))
  , (("led_classdev_register", 1), ("led_classdev_unregister", 1))
  , (("class_register", 0), ("class_unregister", 0))
  , (("bus_register", 0), ("bus_unregister", 0))
  , (("register_pernet_subsys", 0), ("unregister_pernet_subsys", 0))
  , (("cdev_add", 0), ("cdev_del", 0))
  , (("input_register_device", 0), ("input_unregister_device", 0))
  , (("register_kretprobe", 0), ("unregister_kretprobe", 0))
  , (("register_jprobe", 0), ("unregister_jprobe", 0))
  , (("register_keyboard_notifier", 0), ("unregister_keyboard_notifier", 0))
  , (("register_die_notifier", 0), ("unregister_die_notifier", 0))
  , (("register_hotcpu_notifier", 0), ("unregister_hotcpu_notifier", 0))
  , (("register_cpu_notifier", 0), ("unregister_cpu_notifier", 0))
  , (("register_module_notifier", 0), ("unregister_module_notifier", 0))
  , (("request_irq", 0), ("free_irq", 0))
  ]

argAt :: Word64 -> ExprPattern -> [ExprPattern]
argAt n x = replicate (fromIntegral n) Wild <> [x]

failedToUnregister :: [Prim]
failedToUnregister = fmap f thingsYouShouldUnregister
  where
    primType =
      PrimSpec
        { name = "FailedToUnregisterKernelHandler"
        , vars = HashSet.fromList ["handler"]
        , locations = HashSet.fromList ["registerHandlerCall"]
        }
    f :: ((Text, Word64), (Text, Word64)) -> Prim
    f ((regName, regArg), (unregName, unregArg)) =
      Prim
        { primType = primType
        , stmtPattern = ordered
            [ Star
            , Location "registerHandlerCall"
              . Stmt . Call Nothing (CallFunc $ FuncName regName) . argAt regArg $ Bind "handler" Wild
            , AvoidUntil $ AvoidSpec
              { avoid = Stmt . Call Nothing (CallFunc $ FuncName unregName) . argAt unregArg
                        $ Bound "handler"
              , until = ordered [Star, EndOfPath]
              }
            ]
        }

unboundedCopyFromUserPrim_ :: Prim
unboundedCopyFromUserPrim_ = Prim
  { primType = unboundedCopyFromUserSpec
  , stmtPattern =
      Stmt $ Call Nothing (CallFunc
                             . FuncNames
                             . HashSet.fromList
                             $ ["_copy_from_user", "copy_from_user"])
        [ Bind "dst" Wild
        , Bind "src" Wild
        , Bind "size" (NotPattern Immediate)
        ]
  }

unboundedCopyFromUserPrim :: Prim
unboundedCopyFromUserPrim = Prim
  { primType = unboundedCopyFromUserSpec
  , stmtPattern =
      AvoidUntil $ AvoidSpec
        { avoid = Stmt . Constraint
                  $   (Contains (Bound "size") .< Wild)
                  .|| (Contains (Bound "size") .<= Wild)
        , until = ordered
                  [ Star
                  , Location "copy" . SubPrimitive unboundedCopyFromUserPrim_ $ 
                    [ ("src", Bind "src" $ Contains isInput)
                    , ("dst", Bind "dst" Wild)
                    , ("size", Bind "size" $ NotPattern Immediate)
                    ]
                  ]
        }
  }

-- TODO: FIX THIS INDENTATION
copyFromGlobalPrim :: Prim
copyFromGlobalPrim =
  Prim
    { primType = copyMemSpec
    , stmtPattern = ordered
        [ Star
        , Location "copy" $
            Stmt $
              Call
                Nothing
                ( CallFunc
                    . FuncNames
                    . HashSet.fromList
                    $ ["memcpy", "_memcpy"] -- NOTE: _memcpy huh why?
                    --                    [ Bind "dst" Wild
                    --                    , Bind "src" (Contains GlobalAddr)
                    --                    , Bind "size" Wild
                    --                    ]
                    --                  , Primitive copyFromGlobalSpec $ 
                    --                    [ ("src", Bind "src" $ Contains GlobalAddr)
                    --                    , ("dst", Bind "dst" Wild)
                    --                    , ("size", Bind "size" $ NotPattern Immediate)
                    --                    ]
                )
                [ Bind "dest_ptr" Wild
                , Bind "src_ptr" (Contains (TaintedBy GlobalAddr))
                , --          , Bind "src" (Contains isInput) -- fixme
                  Bind "len" Wild
                ]
        ]
    }

--

-- Detects calls to make_dynamic_area() which makes stack executable
detectStackExecPrim :: Prim
detectStackExecPrim =
  Prim
    { primType = detectStackExecSpec
    , stmtPattern = ordered
        [ Star
        , orr
            [ Location "call" $
                CallsPrimitive detectStackExecSpec
                    [ ("location", Wild)
                    , ("length", Wild)
                    , ("permissions", Wild)
                    ]
            , Location "call" $ Stmt $ Call Nothing (CallFunc $ FuncName "make_dynamic_area") []
            ]
        ]
    }

escapedDataFromLock :: Prim
escapedDataFromLock =
  Prim
    { primType = escapedDataFromLockSpec
    , stmtPattern = ordered
        [ Location "lock" . Stmt $ Call Nothing (CallFunc $ FuncName "mutex_lock") [Wild]
        , Location "bind" $
            orr
              [ Stmt $
                  Call
                    Nothing
                    (CallFunc $ FuncName "_copy_to_user")
                    [ Bind "ptr_containing_escaped_data" (Contains isInput)
                    , Wild
                    ]
              , Stmt $ Store (Bind "ptr_containing_escaped_data" $ Contains isInput) Wild
              -- TODO: add more copy cases or use Copy primitive
              ]
        , Location "unlock" . Stmt $ Call Nothing (CallFunc $ FuncName "mutex_unlock") [Wild]
        ]
    }
