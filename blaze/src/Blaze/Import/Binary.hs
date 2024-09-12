module Blaze.Import.Binary where

import Blaze.Prelude

class BinaryImporter a where
  -- | This opens a binary or an importer db (like `bndb` or `gzf`)
  openBinary :: FilePath -> IO (Either Text a)
  -- | Action which is __required__ to be run before exiting the current
  -- process. It's a good idea to add this to the end of your @main@ action for
  -- any importer types that have been used.
  shutdown :: IO ()
  -- | Saves the importer to a db
  -- The FilePath that is returned is the full path to the db.
  -- The FilePath arg might or might not already include a db extension,
  -- so be sure to check before adding an extension.
  saveToDb :: FilePath -> a -> IO (Either Text FilePath)
  rebaseBinary :: a -> Address -> IO a
  getStart :: a -> IO Address
  getEnd :: a -> IO Address
  getOriginalBinaryPath :: a -> IO FilePath
