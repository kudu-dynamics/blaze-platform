module Flint.Analysis.Path.Matcher.Primitives.Library where

import Flint.Prelude hiding (Location)

import Flint.Analysis.Path.Matcher
import qualified Flint.Analysis.Path.Matcher as M
import Flint.Analysis.Path.Matcher.Primitives.Library.PrimSpec
import Flint.Types.Analysis.Path.Matcher.Func
import Flint.Types.Analysis.Path.Matcher.Primitives

import Blaze.Pil.Construct hiding (not)

import qualified Data.HashMap.Strict as HashMap
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
  -- , escapedDataFromLock
  , writePrim
  , returnsFreedPointerPrim
  ]

---------------------------

isArg :: ExprPattern
isArg = Param

-- Matches function input parameters and external global addresses
isInput :: ExprPattern
isInput= Param .|| GlobalAddr

-- | This indicates the expr is tainted by function input (args/globals)
-- TODO: maybe we can use CodeSummary when matching
fromInput :: ExprPattern
fromInput = Wild


------------------------

controlledFormatStringPrim :: Prim
controlledFormatStringPrim = Prim
  { primType = controlledFormatStringSpec
  , stmtPattern =
      [ Star
      , Location "call" . Primitive controlledFormatStringSpec $ HashMap.fromList
        [ ("fmt", Bind "fmt" (Contains Param)) ]
      ]
  }

freeHeapPrim :: Prim
freeHeapPrim = Prim
  { primType = freeHeapSpec
  , stmtPattern =
      [ Star
      , Location "free" . Primitive freeHeapSpec $ HashMap.fromList
        [ ("ptr", Bind "ptr" (Contains isInput)) ]
      ]
  }

allocHeapPrim :: Prim
allocHeapPrim = Prim
  { primType = allocHeapSpec
  , stmtPattern =
      [ Star
      , Location "alloc" . Primitive allocHeapSpec $ HashMap.fromList
        [ ("ptr", Bind "ptr" (Contains isInput))
        , ("size", Bind "size" Wild)
        ]
      ]
  }

doubleFreePrim :: Prim
doubleFreePrim = Prim
  { primType = doubleFreeSpec
  , stmtPattern =
      [ Star
      , orr
        [ Location "free1" . Location "free2" . Primitive doubleFreeSpec $ HashMap.fromList
          [ ("ptr", Bind "ptr" (Contains isInput)) ]
        , ordered
          [ Location "free1" . Primitive freeHeapSpec $ HashMap.fromList
            [ ("ptr", Bind "ptr" (Contains isInput)) ]
          , Star
          -- , Location "free2" . Primitive freeHeap $ HashMap.fromList
          --   [ ("ptr", Bind "ptr" Wild) ]
          , AvoidUntil $ AvoidSpec
            { avoid = Primitive allocHeapSpec $ HashMap.fromList
                      [("ptr", Bind "ptr" Wild)]
            , until =
              Location "free2" . Primitive freeHeapSpec $ HashMap.fromList
              [ ("ptr", Bind "ptr" Wild) ]
            }
          ]
        ]
      ]
  }

writePrim :: Prim
writePrim = Prim
  { primType = writeSpec
  , stmtPattern =
      [ Star
      , orr
        [ Location "write" . Primitive writeSpec $ HashMap.fromList
          [ ("ptr", Bind "ptr" (Contains isInput)) 
          , ("value", Bind "value" Wild)
          ]
        , Location "write" . Stmt $ Store (Bind "ptr" (Contains isInput)) (Bind "value" Wild)
        ]
      ]
  }

writeWhatWherePrim :: Prim
writeWhatWherePrim = Prim
  { primType = writeWhatWhereSpec
  , stmtPattern =
      [ Star
      , orr
        [ Location "write" . Primitive writeWhatWhereSpec $ HashMap.fromList
          [ ("src", Bind "src" (Contains isInput))
          -- TODO: probably can't refer to previously bound var here -- fix!
          , ("dest", Bind "dest" (NotPattern $ Bind "src" (Contains isInput)))
          ]
        , Location "write" . Stmt
          $ Store (Bind "dest" (Contains isInput))
                  (load (Bind "src" (NotPattern $ Bind "dest" (Contains isInput))) ())
        ]
      ]
  }

integerOverflowPrim :: Prim
integerOverflowPrim = Prim
  { primType = integerOverflowSpec
  , stmtPattern =    
      [ Star
      , orr
        [ Location "increment store" . Primitive integerOverflowSpec $ HashMap.fromList
          [ ("ptr", Bind "ptr" Wild )
          , ("increment_by", Bind "increment_by" Wild)
          ]
        , Location "increment store"
          . Stmt $ Store
          (Bind "ptr" Wild)
          (add (load (Bind "ptr" Wild) ())
            (Bind "increment_by" Wild) ())
        , AvoidUntil $ AvoidSpec
          { avoid = Stmt . Constraint
                    $   (Contains (load (Bind "ptr" Wild) ()) .< Wild)
                    .|| (Contains (load (Bind "ptr" Wild) ()) .<= Wild)
                    .|| (Contains (load (Bind "ptr" Wild) ()) .> Wild)
                    .|| (Contains (load (Bind "ptr" Wild) ()) .>= Wild)
                    .|| (Contains (load (Bind "ptr" Wild) ()) .== Wild)
                    .|| (Contains (load (Bind "ptr" Wild) ()) ./= Wild)
          , until = Location "increment store"
                    . Stmt $ Store
                           (Bind "ptr" Wild)
                           (add (load (Bind "ptr" Wild) ())
                            (Bind "increment_by" Wild) ())
          }
        ]
      ]
  }

returnsFreedPointerPrim :: Prim
returnsFreedPointerPrim = Prim
  { primType = returnsFreedPointerSpec
  , stmtPattern =
    [ Star
    , orr
      [ Location "free" . Primitive freeHeapSpec $ HashMap.fromList
        -- [ ("ptr", Bind "ptr" Wild) ]
        [ ("ptr", Bind "ptr" Wild) ]
      , Location "free" . Primitive returnsFreedPointerSpec $ HashMap.fromList
        [ ("ptr", Bind "ptr" Wild) ]
      ]
    , Star
    -- TODO: Avoid any resetting of the pointer to something else between free and return
    , Location "return" . Stmt . M.Ret $ Bind "ptr" Wild
    ]
  }

-------------------------------------

writeToKernelGlobalPrim :: Prim
writeToKernelGlobalPrim = Prim
  { primType = writeToKernelGlobalSpec
  , stmtPattern =
      [ Star
      , Location "copy" $ orr
        [ Stmt $ Call Nothing (CallFunc
                                . FuncNames
                                . HashSet.fromList
                                $ ["_copy_from_user", "copy_from_user"])
          [ Bind "dest" isGlobal
          , Bind "src" (Contains isInput)
          , Bind "len" Wild
          ]
        , Primitive writeToKernelGlobalSpec $ HashMap.fromList
          [ ("src", Bind "src" $ Contains isInput)
          , ("dest", Bind "dest" isGlobal)
          , ("len", Bind "len" Wild)
          ]
        ]
      ]
  }
  where
    isGlobal = Contains GlobalAddr

---------------

controlledIndirectCallPrim :: Prim
controlledIndirectCallPrim = Prim
  { primType = controlledIndirectCallSpec
  , stmtPattern =
      [ Star
      , orr
        [
          Location "callsite" . Primitive controlledIndirectCallSpec $ HashMap.fromList
          [ ("call_dest", Bind "call_dest" (Contains isInput))
          ]
        , Location "callsite" . Stmt $ Call Nothing (CallIndirect $ Bind "call_dest" (Contains isInput)) []
        ]
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
    primType = PrimSpec
      { name = "FailedToUnregisterKernelHandler"
      , vars = HashSet.fromList ["handler"]
      , locations = HashSet.fromList ["registerHandlerCall"]
      }
    f :: ((Text, Word64), (Text, Word64)) -> Prim
    f ((regName, regArg), (unregName, unregArg)) = Prim
      { primType = primType
      , stmtPattern =
          [ Location "registerHandlerCall"
            . Stmt . Call Nothing (CallFunc $ FuncName regName) . argAt regArg
            $ Bind "handler" Wild
          , AvoidUntil $ AvoidSpec
            { avoid = Stmt . Call Nothing (CallFunc $ FuncName unregName)
                      . argAt unregArg $ Bind "handler" Wild
            , until = EndOfPath
            }
          ]
      }

unboundedCopyFromUserPrim :: Prim
unboundedCopyFromUserPrim = Prim
  { primType = unboundedCopyFromUserSpec
  , stmtPattern =
      [ Star
      , AvoidUntil $ AvoidSpec
        { avoid = Stmt . Constraint 
                  $   (Contains (Bind "size" Wild) .< Wild)
                  .|| (Contains (Bind "size" Wild) .<= Wild)
        , until = Location "copy" $ orr
                  [ Stmt $ Call Nothing (CallFunc
                                         . FuncNames
                                         . HashSet.fromList
                                         $ ["_copy_from_user", "copy_from_user"])
                    [ Bind "dst" Wild
                    , Bind "src" (Contains isInput)
                    , Bind "size" (NotPattern Immediate)
                    ]
                  , Primitive unboundedCopyFromUserSpec $ HashMap.fromList
                    [ ("src", Bind "src" $ Contains isInput)
                    , ("dst", Bind "dst" Wild)
                    , ("size", Bind "size" $ NotPattern Immediate)
                    ]
                  ]
        }
      ]
  }

escapedDataFromLock :: Prim
escapedDataFromLock = Prim
  { primType = escapedDataFromLockSpec
  , stmtPattern =
      [ Location "lock" . Stmt $ Call Nothing (CallFunc $ FuncName "mutex_lock") [Wild]
      , Location "bind" $ orr
        [ Stmt $ Call Nothing (CallFunc $ FuncName "_copy_to_user")
          [ Bind "ptr_containing_escaped_data" (Contains isInput)
          , Wild
          ]
        , Stmt $ Store (Bind "ptr_containing_escaped_data" $ Contains isInput) Wild
        -- TODO: add more copy cases or use Copy primitive
        ]
      , Location "unlock" . Stmt $ Call Nothing (CallFunc $ FuncName "mutex_unlock") [Wild]
      ]
  }
