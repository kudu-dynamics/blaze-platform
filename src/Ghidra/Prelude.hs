{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ghidra.Prelude
  ( module Exports
  , PShow(PShow)
  , catchEither
  , liftEitherIO
  , liftMaybeIO
  , liftEitherM
  , liftMaybe
  , liftMaybeM
  , liftMaybeTIO
  , pshow
  , pprint
  , pairs
  , indexed
  , hdebug
  , toSnd
  , unfoldWhileJustM
  , unsafeFromRight
  ) where

import qualified Prelude as P

--import Data.Typeable as Exports

import Control.Concurrent.Async as Exports (mapConcurrently)
import Control.Concurrent.STM as Exports (atomically)
import Control.Lens as Exports
  ( (%=),
    (%~),
    (.=),
    (.~),
    (?~),
    (??),
    Iso',
    Lens',
    (^.),
    (^?),
    (^?!),
    iso,
    lens,
    makeClassy,
    makeClassyPrisms,
    makeFields,
    makeFieldsNoPrefix,
    makeLenses,
    makePrisms,
    over,
    use,
    preview,
    view,
    at,
    _Just,
    _Right,
    _Left,
    _1,
    _2,
    _3,
    _4,
    _5,
  )
import Control.Monad.Trans.Class as Exports (MonadTrans)
import Control.Monad.Trans.Maybe as Exports (MaybeT, runMaybeT)
import Data.BinaryAnalysis as Exports
  ( AddressWidth (AddressWidth),
    Bits (Bits),
    Bytes (Bytes),
    toBits,
    toBytes,
    BitOffset (BitOffset),
    ByteOffset (ByteOffset),
    toBitOffset,
    toByteOffset
  )
import Data.Coerce as Exports (coerce)
import Data.Data as Exports
import Data.Generics.Labels as Exports ()
import Data.Generics.Product.Fields as Exports (HasField(field), HasField'(field'))
import Data.HashMap.Strict as Exports (HashMap)
import Data.HashSet as Exports (HashSet)

import Data.Maybe as Exports (fromJust)
import Data.String.Conversions as Exports (cs)
import qualified Data.Text.Lazy as LT (Text)
import Control.Monad.Extra as Exports (whenJust)
import Protolude as Exports hiding ( Bits
                                   , Fixity
                                   , Infix
                                   , Prefix
                                   , HasField
                                   , head
                                   )
import Protolude.Conv as Exports (StringConv)
import System.IO.Unsafe as Exports (unsafePerformIO, unsafeDupablePerformIO)
import System.Random as Exports (randomIO)
import Text.Pretty.Simple as PP
import Prelude as Exports
  ( (!!),
    String,
    head,
    error,
  )
import qualified Data.Text as Text

liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe e Nothing = throwError e
liftMaybe _ (Just x) = return x

liftMaybeM :: Monad m => e -> m (Maybe a) -> ExceptT e m a
liftMaybeM e m = ExceptT $ maybe (Left e) Right <$> m

--sort of redundant, actually...
liftEitherM :: m (Either e a) -> ExceptT e m a
liftEitherM = ExceptT

liftEither :: (MonadError e m) => Either e a -> m a
liftEither (Left e) = throwError e
liftEither (Right x) = return x

catchEither :: MonadError e m => m a -> m (Either e a)
catchEither m = catchError (Right <$> m) $ return . Left

liftEitherIO :: (MonadError e m, MonadIO m) => IO (Either e a) -> m a
liftEitherIO m = liftIO m >>= liftEither

liftMaybeIO :: (MonadError e m, MonadIO m) => e -> IO (Maybe a) -> m a
liftMaybeIO e m = liftIO m >>= liftEither . maybe (Left e) Right

liftMaybeTIO :: MonadIO m => IO (Maybe a) -> MaybeT m a
liftMaybeTIO m = liftIO m >>= maybe mzero return

ppOptions :: PP.OutputOptions
ppOptions = PP.defaultOutputOptionsNoColor {PP.outputOptionsIndentAmount = 2}

pshow :: Show a => a -> LT.Text
pshow = PP.pShowOpt ppOptions

pprint :: Show a => a -> IO ()
pprint = PP.pPrintOpt PP.NoCheckColorTty ppOptions

pairs :: [a] -> [(a, a)]
pairs xs = zip xs $ drop 1 xs

indexed :: [a] -> [(Int, a)]
indexed = zip [0 ..]

-- hardcore debug
hdebug :: b -> IO () -> b
hdebug x f = unsafePerformIO $ f >> return x

unfoldWhileJustM :: Monad m => m (Maybe a) -> m [a]
unfoldWhileJustM p = do
  y <- p
  case y of
    Just z -> (z: ) <$> unfoldWhileJustM p
    _ -> return []


newtype PShow a = PShow a
  deriving (Eq, Ord, Generic)

instance Show a => Show (PShow a) where
  show (PShow x) = cs $ pshow x

unsafeFromRight :: forall e a. Show e => Either e a -> a
unsafeFromRight = \case
  Left e -> P.error $ "unsafeFromRight: got " ++ show (Left e :: Either e ())
  Right x -> x

-- taken from Relude
toSnd :: (a -> b) -> a -> (a, b)
toSnd f a = (a, f a)
{-# INLINE toSnd #-}
