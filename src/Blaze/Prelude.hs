{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blaze.Prelude
  ( module Exports
  , Streaming
  , StreamingIO
  , Pretty(pretty)
  , liftListM
  , liftListIO
  , liftEitherIO
  , liftMaybeIO
  , liftEitherM
  , liftMaybe
  , liftMaybeM
  , liftMaybeTIO
  , pshow
  , pprint
  , pairs
  , hdebug
  , twaddleUUID
  ) where

--import qualified Prelude as P
import           Prelude         as Exports        ( String
                                                   , head
                                                   , (!!)
                                                   )

import qualified Data.Text.Lazy as L (Text)
import System.IO.Unsafe (unsafePerformIO)
import Blaze.Pretty as Exports
import System.Random as Exports (randomIO)
import Text.Pretty.Simple as PP
import Data.Data as Exports
import Data.UUID as Exports (UUID)
import Data.HashMap.Strict as Exports (HashMap)
import Data.HashSet as Exports (HashSet)
import qualified Data.UUID as UUID
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
import Data.String.Conversions as Exports ( cs )
import           Data.Maybe      as Exports        ( fromJust )
import           Protolude       as Exports hiding ( head, Infix, Prefix, Fixity )
import Control.Monad.Trans.Maybe as Exports (runMaybeT, MaybeT)
import Control.Monad.Trans.Class as Exports (MonadTrans)
import Control.Concurrent.Async as Exports (mapConcurrently)
import Streamly as Exports ( IsStream
                           , asyncly )
import qualified Streamly.Prelude
import Data.SBV.Tools.Overflow (ArithOverflow(bvAddO, bvSubO, bvMulO, bvMulOFast, bvDivO, bvNegO))
import Data.SBV.Internals (SBV(SBV, unSBV), SVal)
import Data.SBV.Trans (SWord, SInt, SBool)

type Streaming t m = (Monad m, Monad (t m), MonadTrans t, IsStream t)

type StreamingIO t m = (Monad m, Monad (t m), MonadTrans t, IsStream t, MonadIO m, MonadIO (t m))

liftListM :: Streaming t m => m [a] -> t m a
liftListM = Streamly.Prelude.fromList <=< lift

liftListIO :: (StreamingIO t m) => IO [a] -> t m a
liftListIO = liftListM . liftIO

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

pairs :: [a] -> [(a, a)]
pairs xs = zip xs $ drop 1 xs

-- hardcore debug
hdebug :: b -> IO () -> b
hdebug x f = unsafePerformIO $ f >> return x

twaddleUUID :: Word32 -> UUID -> UUID
twaddleUUID diff' uuid =
  UUID.fromWords (w1 + diff') (w2 + diff') (w3 + diff') (w4 + diff')
  where
    (w1, w2, w3, w4) = UUID.toWords uuid

--------------

l2 :: (SVal -> SVal -> (SBool, SBool)) -> SBV a -> SBV a -> (SBool, SBool)
l2 f (SBV a) (SBV b) = f a b

instance ArithOverflow (SWord 128) where
  bvAddO = l2 bvAddO
  bvSubO = l2 bvSubO
  bvMulO = l2 bvMulO
  bvMulOFast = l2 bvMulOFast
  bvDivO = l2 bvDivO
  bvNegO = bvNegO . unSBV

instance ArithOverflow (SInt 128) where
  bvAddO = l2 bvAddO
  bvSubO = l2 bvSubO
  bvMulO = l2 bvMulO
  bvMulOFast = l2 bvMulOFast
  bvDivO = l2 bvDivO
  bvNegO = bvNegO . unSBV
