module Flint.App where

import Flint.Prelude

import Blaze.Import.Binary (BinaryImporter, openBinary, shutdown)
import Blaze.Import.CallGraph (CallGraphImporter)
import Blaze.Import.Cfg (CfgImporter, NodeDataType)
import Blaze.Import.Pil (PilImporter)

import Blaze.Import.Source.BinaryNinja (BNImporter)
import Blaze.Import.Source.Ghidra qualified as G

import Blaze.Types.Cfg (PilNode)

import qualified Data.Text as Text

data Backend
  = BinaryNinja
  | Ghidra
  deriving (Eq, Ord, Read, Show)

defaultBackend :: Backend
defaultBackend = BinaryNinja

guessFileBackend :: FilePath -> Maybe Backend
guessFileBackend fp
  | Text.isSuffixOf ".bndb" fp' = Just BinaryNinja
  | Text.isSuffixOf ".gzf" fp' = Just Ghidra
  | otherwise = Nothing
  where
    fp' = cs fp

type FullImporter imp =
  ( BinaryImporter imp
  , CallGraphImporter imp
  , CfgImporter imp
  , PilImporter imp
  , NodeDataType imp ~ PilNode
  )

-- | Open @file@ using @backend@, if it is specified; otherwise, attempt to
-- guess backend if @file@ is a database. Then run @f@ with the resulting
-- importer. 'Blaze.Import.Binary.shutdown' will automatically be called after
-- @f@.
withBackend
  :: Maybe Backend -- ^ @backend@
  -> FilePath -- ^ @file@
  -> (forall imp. FullImporter imp => imp -> IO ()) -- ^ @f@
  -> IO ()
withBackend mBackend fp action = do
  let (msg :: Text, backend') = case (mBackend, guessFileBackend fp) of
        (Nothing, Nothing) ->
          ( "Opening binary with default backend (" <> show defaultBackend <> ")"
          , BinaryNinja
          )
        (Nothing, Just b) ->
          ( "Opening " <> show b <> " db with " <> show b <> " backend"
          , b
          )
        (Just specifiedBackend, Nothing) ->
          ( "Opening binary with " <> show specifiedBackend <> " backend"
          , specifiedBackend
          )
        (Just specifiedBackend, Just guessedBackend)
          | specifiedBackend /= guessedBackend ->
            ( "WARNING: detected db file for " <> show guessedBackend <> " but using user-specified backend: " <> show specifiedBackend
            , specifiedBackend
            )
          | otherwise ->
            ( "Opening " <> show specifiedBackend <> " db with " <> show specifiedBackend <> " backend"
            , specifiedBackend
            )
  putText msg
  case backend' of
    BinaryNinja -> do
      (ebv :: Either Text BNImporter) <- openBinary fp
      either (error . cs) action ebv
        `finally`
        shutdown @BNImporter

    Ghidra -> do
      (egz :: Either Text G.GhidraImporter) <- openBinary fp
      either (error . cs) action egz
        `finally`
        shutdown @G.GhidraImporter
