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

getBestViewType :: BNBinaryView -> IO (Maybe BNBinaryViewType)
getBestViewType bv = do
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

-- getViewOfType :: BNFileMetadata -> String -> IO (Maybe BNBinaryView)
-- getViewOfType meta stype = do
--   view <- 

-- TODO: should return IO (Either err BV)
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
            bv <- BN.openExistingDatabase md fp
            getBvOfBestType md bv
          False -> do
            md <- BN.createFileMetadata
            void $ BN.setFilename md fp
            mbv <- BN.createBinaryDataViewFromFilename md fp
            print mbv
            case mbv of
              Nothing -> P.error "Couldn't open file"
              Just bv -> getBvOfBestType md bv
  where
    getBvOfBestType md bv = do
      mvt <- getBestViewType bv
      print mvt
      case mvt of
        Nothing -> P.error "No view types"
        Just vt -> do
          --- why so redudant?
          vtname <- BN.getBinaryViewTypeName vt
          print vtname
          -- why another bv?
          mbv' <- BN.getFileViewOfType md vtname
          case mbv' of
            Nothing -> do
              BN.getFileViewOfType md "Raw" >>= \case
                Nothing -> P.error "Can't even get raw view type."
                Just bv' -> BN.createBinaryViewOfType vt bv'
            Just bv' -> return bv'
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

