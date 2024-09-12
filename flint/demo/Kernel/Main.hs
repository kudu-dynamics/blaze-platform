{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}

module Main (main) where

import Flint.Prelude

import Flint.Analysis.Path.Matcher
import Flint.Types.Query

import Blaze.CallGraph (getCallGraph)
import Blaze.Import.Source.BinaryNinja (BNImporter)
import Blaze.Import.Binary (openBinary, BinaryImporter(shutdown))
import qualified Blaze.Types.Graph as G
import qualified Blaze.Import.CallGraph as CG

import Data.Time.Clock (getCurrentTime, diffUTCTime)


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

testGettingCallGraph :: FilePath -> IO ()
testGettingCallGraph fp = do
  t00 <- getCurrentTime
  (Right bv) <- openBinary fp :: IO (Either Text BNImporter)
  putText "Got bndb."
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
  testGettingCallGraph "/tmp/vmlinux3.16.7_x86.bndb"
  putText "finished"
  shutdown @BNImporter
