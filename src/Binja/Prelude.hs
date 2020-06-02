module Binja.Prelude
  ( module Exports
  , liftEitherIO
  , liftMaybeIO
  , liftEitherM
  , liftMaybe
  , liftMaybeM
  , liftMaybeTIO
  , pshow
  , pprint
  ) where

import           Prelude                   as Exports        ( (!!)
                                                             , String
                                                             , head
                                                             , id
                                                             )

import           Binja.Types.ClassyFields  as Exports        ()
import           Control.Lens              as Exports        ( (%~)
                                                             , (.~)
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
import           Control.Monad.Trans.Maybe as Exports        ( MaybeT
                                                             , runMaybeT
                                                             )
import           Data.BinaryAnalysis       as Exports        ( Address( Address )
                                                             , AddressWidth( AddressWidth )
                                                             , Bits( Bits )
                                                             , Bytes( Bytes )
                                                             , toBits
                                                             , toBytes
                                                             )
import           Data.Data                 as Exports
import           Data.Maybe                as Exports        ( fromJust )
import qualified Data.Text.Lazy            as L              ( Text )
import           Protolude                 as Exports hiding ( Fixity
                                                             , Bits
                                                             , Infix
                                                             , Prefix
                                                             , head
                                                             )
import           Text.Pretty.Simple        as PP

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
pprint = PP.pPrintOpt PP.NoCheckColorTty ppOptions

-- fromRight :: Either e a -> a
-- fromRight (Right x) = x
-- fromRight (Left _) = P.error "You called fromRight on a Left"
