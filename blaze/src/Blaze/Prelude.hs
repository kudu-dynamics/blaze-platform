{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blaze.Prelude
  ( module Exports
  , PShow(PShow)
  , catchEither
  , hoistMaybe
  , liftEitherIO
  , liftMaybeIO
  , liftEitherM
  , liftMaybe
  , liftMaybeM
  , liftMaybeTIO
  , maxFunctionNameLength
  , pshow
  , pprint
  , pairs
  , indexed
  , hdebug
  , toSnd
  , truncateMiddle
  , twaddleUUID
  , unfoldWhileJustM
  , unsafeHead
  , unsafeFromRight
  , ByteBased(toBytes_, fromBytes_)
  , fromByteBased
  , Verbosity(..)
  , getVerbosity
  , setVerbosity
  , debug
  , warn
  , info
  , sequentialPutText
  ) where

import qualified Prelude as P

import Data.Type.Equality as Exports (type (~))

import Control.Concurrent.Async as Exports (mapConcurrently)
import Control.Concurrent.STM.TVar as Exports (TVar)
import Control.Concurrent.STM.TMVar as Exports (TMVar)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan, readTChan)
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
    traverseOf,
    view,
    at,
    ix,
    _Just,
    _Right,
    _Left,
    _1,
    _2,
    _3,
    _4,
    _5,
  )
import Control.Lens.Extras as Exports (is)
import Control.Monad.Trans.Class as Exports (MonadTrans)
import Control.Monad.Trans.Maybe as Exports (MaybeT (MaybeT), runMaybeT)
import Data.Aeson as Exports (FromJSON, ToJSON, ToJSONKey, FromJSONKey)
import Data.BinaryAnalysis as Exports
  ( Address (Address),
    AddressSpace (AddressSpace),
    addrToInt,
    intToAddr,
    AddressWidth (AddressWidth),
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
import Data.Generics.Sum as Exports (_Ctor)
import Data.HashMap.Strict as Exports (HashMap)
import Data.HashSet as Exports (HashSet)
import Data.IORef
import Data.List.NonEmpty as Exports ((<|))
import Data.Maybe as Exports (fromJust)
import Data.SBV.Internals (SBV (SBV, unSBV), SVal)
import Data.SBV.Tools.Overflow (ArithOverflow (bvAddO, bvDivO, bvMulO, bvNegO, bvSubO))
import Data.SBV.Trans (SBool, SInt, SWord)
import Data.String.Conversions as Exports (cs)
import qualified Data.Text.Lazy as LT (Text)
import Data.UUID as Exports (UUID)
import Control.Monad.Extra as Exports (whenJust, mapMaybeM)
import qualified Data.UUID as UUID
import Protolude as Exports hiding ( Bits
                                   , Fixity
                                   , Infix
                                   , Prefix
                                   , HasField
                                   , head
                                   )
import Protolude.Conv as Exports (StringConv)
import System.IO.Unsafe (unsafePerformIO)
import System.Random as Exports (randomIO, randomRIO)
import Text.Pretty.Simple as PP
import Prelude as Exports
  ( (!!),
    String,
    head,
    error,
  )
import qualified Data.Text as Text

-- | Convert a 'Maybe' computation to 'MaybeT'.
hoistMaybe :: (Applicative m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure

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

twaddleUUID :: Word32 -> UUID -> UUID
twaddleUUID diff' uuid =
  UUID.fromWords (w1 + diff') (w2 + diff') (w3 + diff') (w4 + diff')
  where
    (w1, w2, w3, w4) = UUID.toWords uuid

unfoldWhileJustM :: Monad m => m (Maybe a) -> m [a]
unfoldWhileJustM p = do
  y <- p
  case y of
    Just z -> (z: ) <$> unfoldWhileJustM p
    _ -> return []
--------------

l2 :: (SVal -> SVal -> SBool) -> SBV a -> SBV a -> SBool
l2 f (SBV a) (SBV b) = f a b

instance ArithOverflow (SWord 128) where
  bvAddO = l2 bvAddO
  bvSubO = l2 bvSubO
  bvMulO = l2 bvMulO
  bvDivO = l2 bvDivO
  bvNegO = bvNegO . unSBV

instance ArithOverflow (SInt 128) where
  bvAddO = l2 bvAddO
  bvSubO = l2 bvSubO
  bvMulO = l2 bvMulO
  bvDivO = l2 bvDivO
  bvNegO = bvNegO . unSBV


newtype PShow a = PShow a
  deriving (Eq, Ord, Generic)

instance Show a => Show (PShow a) where
  show (PShow x) = cs $ pshow x

unsafeHead :: [a] -> a
unsafeHead [] = error "unsafeHead: got empty list"
unsafeHead (x:_) = x

unsafeFromRight :: forall e a. Show e => Either e a -> a
unsafeFromRight = \case
  Left e -> P.error $ "unsafeFromRight: got " ++ show (Left e :: Either e ())
  Right x -> x

maxFunctionNameLength :: Int
maxFunctionNameLength = 48

truncateMiddle :: Int -> Text -> Text
truncateMiddle n t
  | n <= 0 = ""
  | Text.length t > n = Text.take n2 t <> "..." <> Text.takeEnd n2 t
  | otherwise = t
  where
    n2 = n `div` 2

-- taken from Relude
toSnd :: (a -> b) -> a -> (a, b)
toSnd f a = (a, f a)
{-# INLINE toSnd #-}

-- | Class for converting between types that are based on Bytes
-- This is safer than using `fromIntegral`, which doesn't care if types
-- use Bytes or Bits
-- TODO: Think of a better name, one that's just as catchy as "Integral".
class ByteBased a where
  fromBytes_ :: Bytes -> a
  toBytes_ :: a -> Bytes

  default fromBytes_ :: Integral a => Bytes -> a
  fromBytes_ = fromIntegral

  default toBytes_ :: Integral a => a -> Bytes
  toBytes_ = fromIntegral

fromByteBased :: (ByteBased a, ByteBased b) => a -> b
fromByteBased = fromBytes_ . toBytes_

instance ByteBased Bytes
instance ByteBased ByteOffset
-- instance ByteBased Address

data Verbosity
  = VInfo   -- | only show normal stuff and errors
  | VWarn  -- | show warnings + normal
  | VDebug    -- | show all
  deriving (Eq, Ord, Read, Show, Generic)

{-# NOINLINE verbosity #-}
verbosity :: IORef Verbosity
verbosity = unsafePerformIO $ newIORef VInfo

setVerbosity :: Verbosity -> IO ()
setVerbosity = writeIORef verbosity

getVerbosity :: IO Verbosity
getVerbosity = readIORef verbosity

log_ :: MonadIO m => Verbosity -> Text -> m ()
log_ necessaryLevel t = liftIO $ do
  currentLevel <- readIORef verbosity
  bool (return ()) (sequentialPutTextTo stderr t)
    $ currentLevel >= necessaryLevel

debug :: MonadIO m => Text -> m ()
debug = log_ VDebug . (<> "[DEBUG] ")

warn :: MonadIO m => Text -> m ()
warn = log_ VWarn . (<> "[WARNING] ")

info :: MonadIO m => Text -> m ()
info = log_ VInfo . (<> "[INFO] ")


-- | Prints out text to a handle in a single thread to ensure messages aren't jumbled.
-- This is good for printing within concurrent tasks.
{-# NOINLINE sequentialPutTextTo #-}
sequentialPutTextTo :: Handle -> Text -> IO ()
sequentialPutTextTo = unsafePerformIO $ do
  tchan <- newTChanIO :: IO (TChan (Handle, Text))
  void . forkOS $ do
    forever $ atomically (readTChan tchan) >>= uncurry hPutStrLn
  return $ runnerFunc tchan
  where
    runnerFunc :: TChan (Handle, Text) -> Handle -> Text -> IO ()
    runnerFunc tchan h t = atomically $ writeTChan tchan (h, t)

sequentialPutText :: Text -> IO ()
sequentialPutText = sequentialPutTextTo stdout

