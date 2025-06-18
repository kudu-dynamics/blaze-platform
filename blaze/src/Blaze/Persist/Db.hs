{- HLINT ignore "Redundant <$>" -}

module Blaze.Persist.Db
  ( module Blaze.Persist.Db
  , module Db
  ) where

import Blaze.Prelude hiding ((:*:), Selector)

import Blaze.Types.Persist.Db as Db

import qualified Blaze.Types.CallGraph as CG
import qualified Blaze.Types.Graph as G
import Blaze.Types.Function (Func(Internal, External))
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
    toCGFunc :: Func -> Db.Function
    toCGFunc (Internal func) = Db.Function
      { address = func ^. #address
      , name = func ^. #name
      , symbol = Blob <$> func ^. #symbol
      , library = Nothing
      , isExtern = False
      , params = Blob $ func ^. #params
      }
    toCGFunc (External func) = Db.Function
      { address = fromIntegral $ func ^. #address . #externalIndex
      , name = func ^. #name
      , symbol = Blob <$> func ^. #symbol
      , library = func ^. #library
      , isExtern = True
      , params = Blob $ func ^. #params
      }

    toCGEdge (a, b) = CallGraphEdge def (getAddr a) (isExtern a) (getAddr b) (isExtern b) 

    getAddr (External func) = fromIntegral $ func ^. #address . #externalIndex
    getAddr (Internal func) = func ^. #address

    isExtern (External _) = True
    isExtern (Internal _) = False

type IsExtern = Bool

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
          getFullAddress :: Func -> (IsExtern, Address)
          getFullAddress (Internal func) = (False, func ^. #address)
          getFullAddress (External func) = (True, fromIntegral $ func ^. #address . #externalIndex)
          funcMap :: HashMap (IsExtern, Address) Func
          funcMap = HashMap.fromList . fmap (\fn -> (getFullAddress fn, fn)) $ funcs
          getFunc :: (IsExtern, Address) -> Func
          getFunc = fromJust . flip HashMap.lookup funcMap
          ledges = (\e -> G.fromTupleLEdge ((), ( getFunc (e ^. #srcFuncIsExtern, e ^. #srcFunc)
                                                , getFunc (e ^. #destFuncIsExtern, e ^. #destFunc)
                                                )))
                   <$> edges
      return . Just . G.addNodes funcs $ G.fromEdges ledges
  where
    toFunc :: Function -> Func
    toFunc dbFunc
      | dbFunc ^. #isExtern = External $ Func.ExternFunction
        { symbol = unBlob <$> dbFunc ^. #symbol
        , name = dbFunc ^. #name
        , library = dbFunc ^. #library
        , address = Func.ExternAddress . fromIntegral $ dbFunc ^. #address
        , params = unBlob $ dbFunc ^. #params
        }
      | otherwise = Internal $ Func.Function
        { symbol = unBlob <$> dbFunc ^. #symbol
        , name = dbFunc ^. #name
        , address = dbFunc ^. #address
        , params = unBlob $ dbFunc ^. #params
        }
