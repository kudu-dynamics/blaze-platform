module Main (main) where

import Flint.Prelude

import Blaze.Import.Binary (BinaryImporter(shutdown))
import Blaze.Import.Source.BinaryNinja (BNImporter)
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
  shutdown @BNImporter
