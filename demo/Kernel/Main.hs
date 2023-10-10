module Main (main) where

import Flint.Prelude

import Flint.Types.Analysis
import Flint.Analysis
import Flint.Analysis.Path.Matcher
import Flint.Types.Query

import qualified Binja.Core as BN
import Blaze.CallGraph (getCallGraph)
import Blaze.Import.Source.BinaryNinja (BNImporter)
import Blaze.Import.Binary (openBinary)
import qualified Blaze.Types.Graph as G
import qualified Blaze.Types.Pil as Pil
import qualified Blaze.Import.CallGraph as CG

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (listDirectory)
import qualified Data.HashSet as HashSet


doubleFreeBug :: BugMatch
doubleFreeBug = BugMatch
  { pathPattern =    
    [ Ordered
      [ Stmt $ Call Nothing (CallFunc (FuncName "free")) [ Bind "ptr" Wild ]
      , Stmt $ Call Nothing (CallFunc (FuncName "free")) [ Bind "ptr" Wild ]
      ]
    ]
  , bugName = "Double Free"
  , bugDescription =
    "This path frees " <> TextExpr "ptr" <> " twice. This may result in a memory leak or a write-what-where."
  , mitigationAdvice = "Ensure that " <> TextExpr "ptr" <> " is freed only once."
  }

memAllocatedWithMallocFreedWithKfreeBug :: BugMatch
memAllocatedWithMallocFreedWithKfreeBug = BugMatch
  { pathPattern =    
    [ Ordered
      [ Stmt $ Call (Just $ Bind "ptr" Wild) (CallFunc (FuncName "malloc")) [ Wild ]
      , Stmt $ Call Nothing (CallFunc (FuncName "kfree")) [ Bind "ptr" Wild ]
      ]
    ]
  , bugName = "Mem Allocated with `malloc` freed with `kfree`"
  , bugDescription = "`" <> TextExpr "ptr" <> "` is allocated with `malloc`, but freed with `kfree`. From the `kfree` man page: Don't free memory not originally allocated by `kmalloc` or you will run into trouble."
  , mitigationAdvice = "Allocate " <> TextExpr "ptr" <> " with `kmalloc`."
  }

kernelBugs :: [BugMatch]
kernelBugs =
  [ memAllocatedWithMallocFreedWithKfreeBug
  , doubleFreeBug
  ]

queryVariableInfoConfig :: BinarySearchConfig BNImporter FuncConfig
queryVariableInfoConfig = BinarySearchConfig
  { binaryPath = "/tmp/e1smm/QueryVariableInfo.debug.bndb"
  , excludeFuncsFromStore = []
  , queries =
    [
      ( QueryAllPaths $ QueryAllPathsOpts
        { start = FuncSym "QueryVariableInfoHandler"
        }
      , kernelBugs
      )
    ]    
  }

testGettingCallGraph :: FilePath -> IO ()
testGettingCallGraph fp = do
  t00 <- getCurrentTime
  (Right bv) <- openBinary fp :: IO (Either Text BNImporter)
  putText $ "Got bndb."
  t0 <- getCurrentTime
  putText $ "Time spent: " <> show (diffUTCTime t0 t00)  
  funcs <- CG.getFunctions bv
  t1 <- getCurrentTime
  putText $ "Got funcs: " <> show (length funcs) <> " :hash: " <> show (hash funcs)
  putText $ "Time spent: " <> show (diffUTCTime t1 t0)
  cg <- getCallGraph bv funcs
  t2 <- getCurrentTime
  putText $ "Got callgraph: :hash: " <> show (length (show cg :: String))
  putText $ "Time spent: " <> show (diffUTCTime t2 t1)
  let ddmap = G.calcDescendantsDistanceMap cg
  t3 <- getCurrentTime
  putText $ "Got ddmap: " <> show (hash ddmap) -- (length (show ddmap :: String))
  putText $ "Time spent: " <> show (diffUTCTime t3 t2)
  
  putText $ "TOTAL Time: " <> show (diffUTCTime t3 t00)

main :: IO ()
main = do
  putText "starting"
  -- testGettingCallGraph "res/test_bins/Dive_Logger/Dive_Logger.bndb"
  testGettingCallGraph "/tmp/vmlinux3.16.7_x86.bndb"
  -- summariesOfInterest queryVariableInfoConfig
  -- summariesOfInterest rwVariableConfig
  putText "finished"




-- module Main (main) where

-- import Flint.Prelude

-- import Flint.Types.Analysis
-- import Flint.Analysis
-- import Flint.Types.Query

-- import qualified Binja.Core as BN
-- import Blaze.Import.Source.BinaryNinja (BNImporter)

-- import System.Directory (listDirectory)
-- import qualified Data.HashSet as HashSet


-- -- A demo that looks ipv4 files from the linux kernel

-- -- | Directory that contains ipv4 files
-- ipv4Dir :: FilePath
-- ipv4Dir = "/tmp/ipv4/"

-- sampleAllBndbsInDir :: HashSet Text -> FilePath -> IO ()
-- sampleAllBndbsInDir blacklist' dirPath= do
--   bndbFiles <- fmap (dirPath <>) . filter (".bndb" `isSuffixOf`) <$> listDirectory dirPath
--   forM_ bndbFiles (sampleForAllFunctions blacklist')

-- bndb :: Text -> FilePath
-- bndb fileName = ipv4Dir <> cs fileName <> ".o.bndb"

-- convertDirToBndbs :: FilePath -> IO ()
-- convertDirToBndbs dirPath = do
--   fps <- listDirectory dirPath
--   forM_ fps $ \fp -> do
--     let fp' = ipv4Dir <> fp
--     BN.getBinaryView fp' >>= \case
--       Left s -> do
--         putText $ "Error: " <> s
--         return ()
--       Right bv -> do
--         BN.updateAnalysisAndWait bv
--         void . BN.saveBndb bv $ fp' <> ".bndb"
        
-- -- sdioSmmPathsOfInterest :: [(BndbFilePath, [(Address, [Address])])]
-- -- sdioSmmPathsOfInterest =
-- --   [ ( sdioSmmBndbPath
-- --     , [ ( 0x10c84
-- --         , [0x10c8a]
-- --         )
-- --       ]
-- --     )
-- --   ]

-- -- usbRtPathsOfInterest :: [(BndbFilePath, [(Address, [Address])])]
-- -- usbRtPathsOfInterest =
-- --   [ ( usbRtBndbPath
-- --     , [ ( 0x101c00
-- --         , [0x101c21]
-- --         )
-- --       -- -- , ( 0x11a22c
-- --       -- --   , [0x11a453]
-- --       -- --   )
-- --       , ( 0x117e9c
-- --         , [0x117ea9]
-- --         )
-- --       , ( 0x117f64
-- --         , [0x117fb9]
-- --         )
-- --       , ( 0x11800c
-- --         , [0x11806a]
-- --         )
-- --       , ( 0x11800c
-- --         , [0x118073]
-- --         )
-- --       , ( 0x11800c
-- --         , [0x11807b]
-- --         )
-- --       , ( 0x11800c
-- --         , [0x118083]
-- --         )
-- --       -- , ( 0x1180c0
-- --       --   , [ 0x1180d8
-- --       --     , 0x118136
-- --       --     ]
-- --       --   )
-- --       -- , ( 0x1180c0
-- --       --   , [ 0x11819e
-- --       --     , 0x118136]
-- --       --   )

      
-- --       ]
-- --     )
-- --   ]

-- tcpInput :: [(BndbFilePath, [(Address, [Address])])]
-- tcpInput =
--   [ ( bndb "tcp_input"
--     , [ ( 0x21a0 -- tcp_queue_rcv
--         , []
--         )
--       , ( 0x8510 -- tcp_data_queue
--         , [ 0x8cec -- call to kfree_skb_partial
--           , 0x8d98 -- inlined memcpy
--           ]
--         )
--       ]
--     )
--   , ( bndb "tcp_output"
--     , [ ( 0x5d0 -- tcp_options_write
--         , []
--         )
--       ]
--     )
--   ]

-- icmp :: [(BndbFilePath, [(Address, [Address])])]
-- icmp =
--   [ ( bndb "icmp"
--     , [ ( 0x1950, [] ) -- icmp_build_probe
--         -- ( 0x1ef0 -- icmp_rcv
--         -- , []
--         -- )
--       ]
--     )
--   ]


-- -- Funcs that cause our analysis to crash and why
-- blacklist :: HashSet Text
-- blacklist = HashSet.fromList
--   [ "inet_twsk_hashdance" -- solver hangs
--   , "inet_twsk_kill" -- solver hangs
--   , "udp_lib_get_port" -- svExp: exponentiation only works with unsigned bounded symbolic exponents, kind: SInt64
--   , "nexthop_res_grp_activity_update" -- solver hangs
--   , "icmp_rcv" -- solver hangs
--   , "ip_route_me_harder" -- simplify? hangs after "--------------" in showPaths
--   , "tcp_twsk_destructor" -- fromJust, called at src/Blaze/Types/Cfg.hs:446:19 in blaze-0.1.0-inplace:Blaze.Types.Cfg
--   , "inet_send_prepare" -- fromJust, called at src/Blaze/Types/Cfg.hs:446:19 in blaze-0.1.0-inplace:Blaze.Types.Cfg
--   , "devinet_conf_proc" -- svExp: exponentiation only works with unsigned bounded symbolic exponents, kind: SInt64
--   ]

-- megaConfig :: BinarySearchConfig BNImporter FuncConfig
-- megaConfig = BinarySearchConfig
--   { excludeFuncsFromStore =
--     [ FuncSym "inet_send_prepare"
--     , FuncSym "inet_get_local_port_range"
--     , FuncSym "tcp_twsk_destructor"
--     ]
--   , binaryPath = "/tmp/ipv4/libipv4.so.bndb"
--   , queries =
--     [ ( QueryExpandAll $ QueryExpandAllOpts
--         { start = FuncSym "tcp_recvmsg"
--         , callExpandDepthLimit = 4
--         , numSamples = 20
--         }
--       , []
--       )
--     ]
--   }

-- runMega :: IO ()
-- runMega = summariesOfInterest megaConfig

-- diveLoggerConfig :: BinarySearchConfig BNImporter FuncConfig
-- diveLoggerConfig = BinarySearchConfig
--   { excludeFuncsFromStore = []
--   , binaryPath = "res/test_bins/Dive_Logger/Dive_Logger.bndb"
--   , queries =
--     [
--       -- QueryTarget $ QueryTargetOpts
--       -- { start = FuncSym "cgc_SelectDive"
--       -- , callExpandDepthLimit = 20
--       -- , numSamples = 40
--       -- , mustReachSome = (FuncSym "cgc_atoi", 0x804a367) :| []
--       -- }
--       ( QueryTarget $ QueryTargetOpts
--         { start = FuncSym "cgc_SelectDive"
--         , callExpandDepthLimit = 20
--         , numSamples = 40
--         , mustReachSome = (FuncSym "cgc_isdigit", 0x8049f6c) :| []
--         }
--       , []
--       )
      

--     -- , QueryExpandAll $ QueryExpandAllOpts
--     --   { start = FuncSym "cgc_SelectDive"
--     --   , callExpandDepthLimit = 3
--     --   , numSamples = 1
--     --   }
--     ]
--   }


-- main :: IO ()
-- main = do
--   putText "starting"
--   summariesOfInterest diveLoggerConfig
--   -- runMega


--   -- convertDirToBndbs ipv4Dir
--   -- showPathsOfInterest tcpInput
--   -- showPathsOfInterest icmp
--   -- sampleAllBndbsInDir blacklist ipv4Dir
--   -- sampleForAllFunctions HashSet.empty "/tmp/ipv4/icmp.o.bndb"
--   putText "finished"
