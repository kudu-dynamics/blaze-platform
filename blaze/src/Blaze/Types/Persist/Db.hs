{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blaze.Types.Persist.Db where

import Blaze.Prelude hiding ((:*:), Symbol)

import Blaze.Types.Persist.Db.Address ()

import Blaze.Types.Function (FuncParamInfo)

import Data.BinaryAnalysis (Symbol)

import Database.Selda
import Database.Selda.SQLite
import qualified Data.Aeson as Aeson
import Database.Selda.SqlType ( Lit(LBlob, LCustom)
                              , SqlTypeRep(TBlob)
                              , SqlValue(SqlBlob)
                              )
import Database.Selda.Backend (SeldaConnection, runSeldaT)
import Control.Concurrent.STM.TMVar (putTMVar, takeTMVar, newTMVarIO)


newtype Conn = Conn (TMVar (SeldaConnection SQLite))

newtype Blob a = Blob { unBlob :: a }
  deriving (Eq, Ord, Show, Generic, Typeable)

instance (ToJSON a, FromJSON a, Typeable (Blob a)) => SqlType (Blob a) where
   mkLit (Blob x) = LCustom TBlob . LBlob . cs . Aeson.encode $ x

   sqlType _ = TBlob

   fromSql (SqlBlob s) = case Aeson.decode (cs s) of
     Nothing -> error "Could not convert json blob"
     Just x -> Blob x
   fromSql x = error $ "Unexpected sql field type: " <> show x

   defaultValue = LCustom TBlob (LBlob "")

type FuncId = Word64

data Function = Function
  { address :: Address
  , name :: Text
  , symbol :: Maybe (Blob Symbol)
  , params :: Blob [FuncParamInfo]
  } deriving (Generic, SqlRow)

data CallSite = CallSite
  { pid :: ID CallSite
  , addr :: Address
  , srcFunc :: Address
  , destFunc :: Address
  } deriving (Generic, SqlRow)

data CallGraphEdge = CallGraphEdge
  { pid :: ID CallGraphEdge
  , srcFunc :: Address
  , destFunc :: Address
  } deriving (Generic, SqlRow)

functionTable :: Table Function
functionTable = table "function" [#address :- primary]

callSiteTable :: Table CallSite
callSiteTable = table "callsite" [#pid :- autoPrimary]

callGraphEdgeTable :: Table CallGraphEdge
callGraphEdgeTable = table "callgraph_edge" [#pid :- autoPrimary]

open :: FilePath -> IO Conn
open dbPath = do
  c <- sqliteOpen dbPath
  Conn <$> newTMVarIO c

close :: Conn -> IO ()
close = flip withConn seldaClose

withConn :: MonadIO m => Conn -> (SeldaConnection SQLite -> m a) -> m a
withConn (Conn tconn) f = do
  conn <- liftIO . atomically $ takeTMVar tconn
  r <- f conn
  liftIO . atomically $ putTMVar tconn conn
  return r

runSelda :: (MonadMask m, MonadIO m) => Conn -> SeldaT SQLite m a -> m a
runSelda conn m = withConn conn (runSeldaT m)

class (MonadMask m, MonadIO m, Monad m) => MonadDb m where
  withDb :: SeldaT SQLite m a -> m a

onlyOne :: [a] -> Maybe a
onlyOne [] = Nothing
onlyOne [x] = Just x
onlyOne _ = error "Expected only one result"
