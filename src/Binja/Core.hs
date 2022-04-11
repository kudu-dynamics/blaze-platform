module Binja.Core
  ( module Exports
  , getBestViewType
  , getBinaryView
  , getFunctionsContaining
  , getLLILInstructionIndexAtAddress
  , getOriginalBinary
  , saveBndb
  ) where

import Binja.Prelude
import qualified Data.Text as Text
import qualified Binja.BasicBlock as BB
import Binja.Architecture (Architecture)
import qualified Binja.Architecture as Arch
import qualified Binja.C.Main as BN
import Binja.C.Main as Exports
import Binja.C.Types as Exports
import Binja.Function ( Function, LLILFunction )
import qualified Binja.Function as Func

import System.Envy
import qualified Data.Set as Set
import Control.Monad.Error.Class (liftEither)
import System.Directory (doesFileExist)
import Control.Error (assertErr)

newtype BinjaConfig = BinjaConfig {
  binjaPluginsDir :: String
} deriving (Generic, Show)

instance FromEnv BinjaConfig where
  fromEnv _ = BinjaConfig
              <$> env "BINJA_PLUGINS"

initBinja :: BinjaConfig -> IO Bool
initBinja ctx = do
  BN.setBundledPluginDirectory $ binjaPluginsDir ctx
  BN.initCorePlugins
  BN.initUserPlugins
  void BN.initRepoPlugins
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

-- TODO: check to see if the bndb already esists
saveBndb :: BNBinaryView -> FilePath -> IO Bool
saveBndb bv fp = getFileForView bv
  >>= flip getFileViewOfType "Raw"
  >>= maybe (return False) (`createDatabase` fp)

getBinaryView :: FilePath -> IO (Either Text BNBinaryView)
getBinaryView fp = runExceptT $ do
  liftIO (doesFileExist fp)
    >>= liftEither . assertErr ("Path " <> show fp <> " does not exist or is not a file")
  ctx <- liftEitherIO (first Text.pack <$> decodeEnv :: IO (Either Text BinjaConfig))
  liftIO (initBinja ctx)
    >>= liftEither . assertErr "You don't have a Binja license. Sorry."
  if Text.isSuffixOf ".bndb" (Text.pack fp) then
    do
      md <- liftIO BN.createFileMetadata
      bv <- liftMaybeIO "Couldn't open existing db" $
        BN.openExistingDatabase md fp
      getBvOfBestType md bv
  else
    do
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

getOriginalBinary :: BNBinaryView -> IO (Either Text ByteString)
getOriginalBinary bv = do
  meta <- BN.getFileForView bv
  getFileViewOfType meta "Raw" >>= \case
    Nothing -> return $ Left "Could not get raw file from bndb."
    Just bv' -> do
      len <- BN.getViewLength bv'
      vb <- BN.readViewBuffer bv' 0 len >>= BN.duplicateDataBuffer
      Right <$> getDataBufferContents vb len

getFunctionsContaining :: BNBinaryView -> Address -> IO (Set Function)
getFunctionsContaining bv addr = do
  blocks <- BB.getBasicBlocksAtAddress bv addr
  return . Set.fromList $ fmap (view BB.func) blocks

getLLILInstructionIndexAtAddress :: Function -> Architecture -> Address -> IO (InstructionIndex LLILFunction)
getLLILInstructionIndexAtAddress func arch =
  getLowLevelILForInstruction (func ^. Func.handle) (arch ^. Arch.handle)
