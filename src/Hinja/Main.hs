{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Hinja.Main where

import Hinja.Prelude hiding (onException)
import qualified Prelude as P
import Prelude (String)
import qualified Data.Text as Text
import Foreign hiding (void)
import Foreign.C.Types
import Foreign.ForeignPtr
import qualified Control.Exception as E
import qualified Hinja.C.Main as BN
import Hinja.C.Main ( BNBinaryView
                    , BNBinaryViewType
                    , BNFileMetadata
                    )
import qualified Hinja.Function as Func
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

initBinja :: MonadIO m => HinjaConfig -> m Bool
initBinja ctx = liftIO $ do
  BN.setBundledPluginDirectory $ binjaPluginsDir ctx
  BN.initCorePlugins
  BN.initUserPlugins
  void $ BN.initRepoPlugins
  BN.isLicenseValidated

getBestViewType :: MonadIO m => BNBinaryView -> m (Maybe BNBinaryViewType)
getBestViewType bv = liftIO $ do
  vtypes <- BN.getBinaryViewTypesForData bv
  vnames <- mapM BN.getBinaryViewTypeName vtypes
  let vs = zip vtypes vnames
  case headMay (mapMaybe isNotRaw vs) of
    Just t -> return $ Just t
    Nothing -> return . headMay . mapMaybe isRaw $ vs
  where
    isRaw (t, "Raw") = Just t
    isRaw _ = Nothing
    isNotRaw (_, "Raw") = Nothing
    isNotRaw (t, _) = Just t

getBinaryView :: MonadIO m => FilePath -> m (Either Text BNBinaryView)
getBinaryView fp = runExceptT $ do
  ctx <- liftEitherIO (first Text.pack <$> decodeEnv :: IO (Either Text HinjaConfig))
  validated <- initBinja ctx
  case validated of
    False -> throwError "You don't have a Binja license. Sorry."
    True -> do
      case Text.isSuffixOf ".bndb" (Text.pack fp) of
        True -> do
          md <- liftIO BN.createFileMetadata
          bv <- liftMaybeIO "Couldn't open existing db" $
            BN.openExistingDatabase md fp
          getBvOfBestType md bv
        False -> do
          md <- liftIO BN.createFileMetadata
          void . liftIO $ BN.setFilename md fp
          bv <- liftMaybeIO "Couldn't open file." $
            BN.createBinaryDataViewFromFilename md fp
          getBvOfBestType md bv
  where
    getBvOfBestType md bv = do
      vt <- liftMaybeM "No view types" $ getBestViewType bv
      --- why so redudant?
      vtname <- liftIO $ BN.getBinaryViewTypeName vt
      -- why another bv?
      mbv' <- liftIO $ BN.getFileViewOfType md vtname
      case mbv' of
        Nothing -> do
          bv' <- liftMaybeIO "Can't even get raw view type." $ BN.getFileViewOfType md "Raw"
          liftIO $ BN.createBinaryViewOfType vt bv'
        Just bv' -> return bv'

