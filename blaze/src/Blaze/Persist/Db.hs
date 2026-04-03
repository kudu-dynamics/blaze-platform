{- HLINT ignore "Redundant <$>" -}

module Blaze.Persist.Db
  ( module Blaze.Persist.Db
  , module Db
  ) where

import Blaze.Prelude hiding ((:*:), Selector)

import Blaze.Types.Persist.Db as Db

import qualified Blaze.Types.CallGraph as CG
import qualified Blaze.Types.Graph as G
import Blaze.Types.Function (FuncRef)
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
    toCGFunc :: FuncRef -> Db.Function
    toCGFunc (Func.InternalRef fm) = Db.Function
      { address = Blob $ fm ^. #address
      , name = fm ^. #name
      , symbol = Blob <$> fm ^. #symbol
      , library = Nothing
      , isExtern = False
      , params = Blob []
      }
    toCGFunc (Func.ExternalRef fm) = Db.Function
      { address = Blob $ fm ^. #address
      , name = fm ^. #name
      , symbol = Blob <$> fm ^. #symbol
      , library = Nothing
      , isExtern = True
      , params = Blob []
      }

    toCGEdge (a, b) = CallGraphEdge def (toBlobAddr a) (isExtern a) (toBlobAddr b) (isExtern b)

    toBlobAddr (Func.ExternalRef fm) = Blob $ fm ^. #address
    toBlobAddr (Func.InternalRef fm) = Blob $ fm ^. #address

    isExtern (Func.ExternalRef _) = True
    isExtern (Func.InternalRef _) = False

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
      let funcs = toFuncRef <$> dbFuncs
          getFullAddress :: FuncRef -> (IsExtern, Address)
          getFullAddress (Func.InternalRef fm) = (False, fm ^. #address)
          getFullAddress (Func.ExternalRef fm) = (True, fm ^. #address)
          funcMap :: HashMap (IsExtern, Address) FuncRef
          funcMap = HashMap.fromList . fmap (\fn -> (getFullAddress fn, fn)) $ funcs
          getFunc :: (IsExtern, Address) -> FuncRef
          getFunc = fromJust . flip HashMap.lookup funcMap
          ledges = (\e -> G.fromTupleLEdge ((), ( getFunc (e ^. #srcFuncIsExtern, unBlob $ e ^. #srcFunc)
                                                , getFunc (e ^. #destFuncIsExtern, unBlob $ e ^. #destFunc)
                                                )))
                   <$> edges
      return . Just . G.addNodes funcs $ G.fromEdges ledges
  where
    toFuncRef :: Function -> FuncRef
    toFuncRef dbFunc
      | dbFunc ^. #isExtern = Func.ExternalRef $ Func.FunctionRef
        { symbol = unBlob <$> dbFunc ^. #symbol
        , name = dbFunc ^. #name
        , address = unBlob $ dbFunc ^. #address
        }
      | otherwise = Func.InternalRef $ Func.FunctionRef
        { symbol = unBlob <$> dbFunc ^. #symbol
        , name = dbFunc ^. #name
        , address = unBlob $ dbFunc ^. #address
        }
