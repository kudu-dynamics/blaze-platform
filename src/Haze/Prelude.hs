module Haze.Prelude
  ( module Exports
  , F
  , liftEitherIO
  , liftMaybeIO
  , liftEitherM
  , liftMaybe
  , liftMaybeM
  , liftMaybeTIO
  , pshow
  , pprint
  ) where

import           Prelude         as Exports        ( String
                                                   , head
                                                   , id
                                                   , (!!)
                                                   )

import qualified Data.Text.Lazy as L (Text)

import System.Random as Exports (randomIO)
import Text.Pretty.Simple as PP
import Data.Data as Exports
import Data.UUID as Exports (UUID)
--import Data.Typeable as Exports
import           Control.Lens    as Exports        ( (%~)
                                                   , (.~)
                                                   , (.=)
                                                   , (%=)
                                                   , (?~)
                                                   , (^.)
                                                   , (^?)
                                                   , Iso'
                                                   , Lens'
                                                   , iso
                                                   , lens
                                                   , makeClassy
                                                   , makeClassyPrisms
                                                   , makeFields
                                                   , makeFieldsNoPrefix
                                                   , makeLenses
                                                   , makePrisms
                                                   , use
                                                   , view
                                                   )
import           Data.Maybe      as Exports        ( fromJust )
import           Protolude       as Exports hiding ( head, Infix, Prefix, Fixity )
import Control.Monad.Trans.Maybe as Exports (runMaybeT, MaybeT)
import Hinja.Function (MLILSSAFunction)

type F = MLILSSAFunction

liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe e Nothing = throwError e
liftMaybe _ (Just x) = return x

liftMaybeM :: Monad m => e -> m (Maybe a) -> ExceptT e m a
liftMaybeM e m = ExceptT $ m >>= return . maybe (Left e) Right

--sort of redundant, actually...
liftEitherM :: m (Either e a) -> ExceptT e m a
liftEitherM = ExceptT

liftEither :: (MonadError e m) => Either e a -> m a
liftEither (Left e) = throwError e
liftEither (Right x) = return x

liftEitherIO :: (MonadError e m, MonadIO m) => IO (Either e a) -> m a
liftEitherIO m = liftIO m >>= liftEither

liftMaybeIO :: (MonadError e m, MonadIO m) => e -> IO (Maybe a) -> m a
liftMaybeIO e m = liftIO m >>= liftEither . maybe (Left e) Right

liftMaybeTIO :: MonadIO m => IO (Maybe a) -> MaybeT m a
liftMaybeTIO m = liftIO m >>= maybe mzero return

ppOptions :: PP.OutputOptions
ppOptions = PP.defaultOutputOptionsNoColor {PP.outputOptionsIndentAmount = 2}

pshow :: Show a => a -> L.Text
pshow = PP.pShowOpt ppOptions

pprint :: Show a => a -> IO ()
pprint = PP.pPrintOpt ppOptions

