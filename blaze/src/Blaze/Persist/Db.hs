{- HLINT ignore "Redundant <$>" -}

module Blaze.Persist.Db
  ( module Blaze.Persist.Db
  , module Db
  ) where

import Blaze.Prelude hiding ((:*:), Selector)

import Blaze.Types.Persist.Db as Db

import qualified Blaze.Types.CallGraph as CG
import qualified Blaze.Types.Graph as G
import qualified Blaze.Types.Function as Func

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Database.Selda


init :: FilePath -> IO Conn
init flintDb = do
  conn <- open flintDb
  runSelda conn $ do
    tryCreateTable functionTable
    tryCreateTable callGraphEdgeTable
  return conn

insertCallGraph :: Conn -> CG.CallGraph -> IO ()
insertCallGraph conn cg = do
  let funcs = fmap toCGFunc . HashSet.toList $ G.nodes cg
      edges = toCGEdge . snd . G.toTupleLEdge <$> G.edges cg
  runSelda conn $ do
    insert_ functionTable funcs
    insert_ callGraphEdgeTable edges
  where
    toCGFunc :: Func.Function -> Db.Function
    toCGFunc func = Db.Function
      (func ^. #address)
      (func ^. #name)
      (Blob <$> func ^. #symbol)
      (Blob $ func ^. #params)

    toCGEdge :: (Func.Function, Func.Function) -> CallGraphEdge
    toCGEdge (a, b) = CallGraphEdge def (a ^. #address) (b ^. #address)

loadCallGraph :: Conn -> IO (Maybe CG.CallGraph)
loadCallGraph conn = do
  mResult <- runSelda conn $ do
    edges <- query $ select callGraphEdgeTable
    case edges of
      [] -> return Nothing
      _ -> do
        funcs <- query $ select functionTable
        return $ Just (funcs, edges)
  case mResult of
    Nothing -> return Nothing
    Just (dbFuncs, edges) -> do
      let funcs = toFunc <$> dbFuncs
          funcMap :: HashMap Address Func.Function
          funcMap = HashMap.fromList . fmap (\fn -> (fn ^. #address, fn)) $ funcs
          getFunc :: Address -> Func.Function
          getFunc = fromJust . flip HashMap.lookup funcMap
          ledges = (\e -> G.fromTupleLEdge ((), ( getFunc $ e ^. #srcFunc
                                                , getFunc $ e ^. #destFunc
                                                )))
                   <$> edges
      return . Just . G.addNodes funcs $ G.fromEdges ledges
  where
    toFunc dbFunc = Func.Function
      (unBlob <$> dbFunc ^. #symbol)
      (dbFunc ^. #name)
      (dbFunc ^. #address)
      (unBlob $ dbFunc ^. #params)    
