module Blaze.Import.Binary where

import Blaze.Prelude

class BinaryImporter a where
  -- | This opens a binary or an importer db (like `bndb` or `gzf`)
  openBinary :: FilePath -> IO (Either Text a)
  -- | Saves the importer to a db
  -- The FilePath that is returned is the full path to the db.
  -- The FilePath arg might or might not already include a db extension,
  -- so be sure to check before adding an extension.
  saveToDb :: FilePath -> a -> IO (Either Text FilePath)
  rebaseBinary :: a -> Address -> IO a
  getStart :: a -> IO Address
  getEnd :: a -> IO Address
  getOriginalBinaryPath :: a -> IO FilePath
