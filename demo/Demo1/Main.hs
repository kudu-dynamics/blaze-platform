module Main (main) where

import Flint.Prelude

import Flint.Types.Analysis
import Flint.Analysis


accessKeyBndb :: BndbFilePath
accessKeyBndb = "/tmp/demo1/Demo1_Access_Key.bndb"


accessKeyPathsOfInterest :: [(BndbFilePath, [(Address, [Address])])]
accessKeyPathsOfInterest =
  [ ( accessKeyBndb
    , [ ( 0x251 -- validate access key
        , []
        )
      ]
    )
  ]


main :: IO ()
main = do
  showPathsOfInterest accessKeyPathsOfInterest
