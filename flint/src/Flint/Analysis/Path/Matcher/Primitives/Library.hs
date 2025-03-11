module Flint.Analysis.Path.Matcher.Primitives.Library where

import Flint.Prelude hiding (Location)

import Flint.Analysis.Path.Matcher
import Flint.Types.Analysis.Path.Matcher.Func
import Flint.Types.Analysis.Path.Matcher.Primitives

import Blaze.Pil.Construct hiding (not)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet


copyPrim :: PrimType
copyPrim = PrimType
  { name = "copy"
  , vars = HashSet.fromList ["dest", "src", "len"]
  , locations = HashSet.fromList ["write"]
  }

controlledFormatString :: PrimType
controlledFormatString = PrimType
  { name = "ControlledFormatString"
  , vars = HashSet.fromList
    [ "fmt" ]
  , locations = HashSet.fromList
    [ "call" ]
  }

---------------------------


isArg :: ExprPattern
isArg = Var "arg"

-- | This is supposed to restrict expr to func args and globals, but I don't know how
-- to do that yet, really this just accepts everything for now (b/c the Wild)
isInput :: ExprPattern
isInput = Var "arg" .|| Var "param"
  -- TODO: need to figue out how to restrict to inputs
  .|| Wild

-- | This indicates the expr is tainted by function input (args/globals)
-- TODO: maybe we can use CodeSummary when matching
fromInput :: ExprPattern
fromInput = Wild

controlledFormatStringPrim :: Prim
controlledFormatStringPrim = Prim
  { primType = controlledFormatString
  , stmtPattern =
      [ Location "call" . Primitive controlledFormatString $ HashMap.fromList
        [ ("fmt", Bind "fmt" (Contains Param)) ] 
      ]
  }
-------------------------------------

writeToKernelGlobal :: Prim
writeToKernelGlobal = Prim
  { primType = PrimType
               { name = "WriteToKernelGlobal"
               , vars = HashSet.fromList
                 [ "src", "dest", "len" ]
               , locations = HashSet.fromList
                 [ "write" ]
               }
  , stmtPattern =
      [ Location "write" . Stmt $ Call Nothing (CallFunc $ FuncName "_copy_from_user")
        [ Bind "dest" isGlobal
        , Bind "src" (isGlobal .|| isArg)
        , Bind "len" Wild
        ]
      ]
  }
  where
    -- TODO: pass in global from CodeSummary?
    isGlobal = Immediate

---------------

controlledIndirectCall :: Prim
controlledIndirectCall = Prim
  { primType = PrimType
               { name = "ControlledIndirectCall"
               , vars = HashSet.fromList
                 [ "callTarget" ]
               , locations = HashSet.fromList
                 [ "call" ]
               }
  , stmtPattern =
      [ Location "call" . Stmt $ Call Nothing (CallIndirect $ Bind "callTarget" (Contains inputDest)) []
      ]
  }
  where
    isGlobal = Immediate -- lame
    inputDest = Contains isArg .|| load isGlobal ()


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
    primType = PrimType
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

