module Main (main) where

import Flint.Prelude

import Flint.Types.Analysis
import Flint.Analysis
import System.Directory (listDirectory)
import qualified Binja.Core as BN

-- A demo that looks ipv4 files from the linux kernel

-- | Directory that contains ipv4 files
ipv4Dir :: FilePath
ipv4Dir = "/tmp/ipv4/"

bndb :: Text -> FilePath
bndb fileName = ipv4Dir <> cs fileName <> ".o.bndb"

convertDirToBndbs :: FilePath -> IO ()
convertDirToBndbs dirPath = do
  fps <- listDirectory dirPath
  forM_ fps $ \fp -> do
    let fp' = ipv4Dir <> fp
    BN.getBinaryView fp' >>= \case
      Left s -> do
        putText $ "Error: " <> s
        return ()
      Right bv -> do
        BN.updateAnalysisAndWait bv
        void . BN.saveBndb bv $ fp' <> ".bndb"
        
-- sdioSmmPathsOfInterest :: [(BndbFilePath, [(Address, [Address])])]
-- sdioSmmPathsOfInterest =
--   [ ( sdioSmmBndbPath
--     , [ ( 0x10c84
--         , [0x10c8a]
--         )
--       ]
--     )
--   ]

-- usbRtPathsOfInterest :: [(BndbFilePath, [(Address, [Address])])]
-- usbRtPathsOfInterest =
--   [ ( usbRtBndbPath
--     , [ ( 0x101c00
--         , [0x101c21]
--         )
--       -- -- , ( 0x11a22c
--       -- --   , [0x11a453]
--       -- --   )
--       , ( 0x117e9c
--         , [0x117ea9]
--         )
--       , ( 0x117f64
--         , [0x117fb9]
--         )
--       , ( 0x11800c
--         , [0x11806a]
--         )
--       , ( 0x11800c
--         , [0x118073]
--         )
--       , ( 0x11800c
--         , [0x11807b]
--         )
--       , ( 0x11800c
--         , [0x118083]
--         )
--       -- , ( 0x1180c0
--       --   , [ 0x1180d8
--       --     , 0x118136
--       --     ]
--       --   )
--       -- , ( 0x1180c0
--       --   , [ 0x11819e
--       --     , 0x118136]
--       --   )

      
--       ]
--     )
--   ]

tcpInput :: [(BndbFilePath, [(Address, [Address])])]
tcpInput =
  [ ( bndb "tcp_input"
    , [ ( 0x21a0 -- tcp_queue_rcv
        , []
        )
      , ( 0x8510 -- tcp_data_queue
        , [ 0x8cec -- call to kfree_skb_partial
          , 0x8d98 -- inlined memcpy
          ]
        )
      ]
    )
  ]

main :: IO ()
main = do
  -- convertDirToBndbs ipv4Dir
  putText "hello"
  showPathsOfInterest tcpInput
