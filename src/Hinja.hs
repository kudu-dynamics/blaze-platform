{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Hinja where

import Hinja.Prelude hiding (onException)
import qualified Prelude as P
import Prelude (String)
import qualified Data.Text as Text
import Foreign hiding (void)
import Foreign.C.Types
import Foreign.ForeignPtr
import qualified Control.Exception as E
import qualified HinjaC as BN
import HinjaC (BNBinaryView)
import System.Envy
import GHC.Generics

a1 :: FilePath
a1 = "/tmp/kudu/assembly/a1"

dive :: FilePath
dive = "/tmp/kudu/blaze/binja-clojure/resources/test_bins/Dive_Logger/Dive_Logger.bndb"

data HinjaConfig = HinjaConfig {
  binjaPluginsDir :: String
} deriving (Generic, Show)

instance FromEnv HinjaConfig where
  fromEnv = HinjaConfig
            <$> env "BINJA_PLUGINS"

initBinja :: HinjaConfig -> IO Bool
initBinja ctx = do
  BN.setBundledPluginDirectory $ binjaPluginsDir ctx
  BN.initCorePlugins
  BN.initUserPlugins
  void $ BN.initRepoPlugins
  BN.isLicenseValidated

-- getAvailableViewTypes :: BNBinaryView -> [BNBinaryViewType]
-- getAvailableViewTypes

getBinaryView :: FilePath -> IO BNBinaryView
getBinaryView fp = (decodeEnv :: IO (Either String HinjaConfig)) >>= \case
  Left s -> P.error $ "Failed to load Env vars: " <> s
  Right ctx -> do
    validated <- initBinja ctx
    case validated of
      False -> P.error "You don't have a Binja license. Sorry."
      True -> do
        case Text.isSuffixOf ".bndb" fpt of
          True -> do
            md <- BN.createFileMetadata
            BN.openExistingDatabase md fp
          False -> undefined
  where
    fpt = Text.pack fp


foreign import ccall "math.h sin"
     c_sin :: CDouble -> CDouble

data Section = Section

data FileMetadata = FileMetadata
  deriving (Show)

foreign import ccall "res/kosher/binaryninjacore.h &BNFreeFileMetadata"
  freeFileMetadata :: FunPtr (Ptr FileMetadata -> IO ())

foreign import ccall "res/kosher/binaryninjacore.h BNCreateFileMetadata"
  createFileMetadata_ :: IO (Ptr FileMetadata)

createFileMetadata :: IO (ForeignPtr FileMetadata)
createFileMetadata = createFileMetadata_ >>= newForeignPtr freeFileMetadata

foreign import ccall "res/kosher/binaryninjacore.h BNFreeSection"
  freeSection :: Ptr Section -> IO ()

fastsin :: Double -> Double
fastsin x = realToFrac (c_sin (realToFrac x))

