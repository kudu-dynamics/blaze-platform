module Main (main) where

import Flint.Prelude

import Flint.App (withBackend, Backend)
import qualified Flint.Cfg.Store as Store
import Flint.Query

import Blaze.Cfg (nodeContainsAddress)
import Blaze.Pretty (pretty', prettyPrint')
import Blaze.Types.Cfg (PilNode)

import qualified Data.HashSet as HashSet
import qualified Data.List.NonEmpty as NE
import Numeric (readHex)
import Options.Applicative


data Options = Options
  { backend :: Maybe Backend
  , maxCallDepth :: Maybe Word64
  , inputFile :: FilePath
  , startAddress :: Address
  , waypoints :: NonEmpty Address
  }
  deriving (Eq, Ord, Read, Show, Generic)

defaultMaxCallDepth :: Word64
defaultMaxCallDepth = 5

parseBackend :: Parser Backend
parseBackend = option auto
  ( long "backend"
    <> metavar "BACKEND"
    <> help "preferred backend (BinaryNinja or Ghidra)"
  )

parseMaxCallDepth :: Parser Word64
parseMaxCallDepth = option auto
  ( long "maxCallDepth"
    <> metavar "MAX_CALL_DEPTH"
    <> help ("max depth of calls to expand (default is " <> show defaultMaxCallDepth <> ")")
  )

parseInputFile :: Parser FilePath
parseInputFile = argument str
  ( metavar "INPUT_FILE"
    <> help "input file"
  )

hexReader :: Integral a => ReadM a
hexReader = eitherReader $ \s ->
  case readHex $ stripHexPrefix s of
    [(n, "")] -> Right $ fromIntegral (n :: Integer)
    _ -> Left $ "Invalid hex address: " <> s

stripHexPrefix :: String -> String
stripHexPrefix ('0':'x':xs) = xs
stripHexPrefix ('0':'X':xs) = xs
stripHexPrefix xs           = xs


parseStartAddress :: Parser Address
parseStartAddress = argument hexReader (metavar "START_ADDR")

parseWaypoints :: Parser (NonEmpty Address)
parseWaypoints = NE.fromList <$> some (argument hexReader $ metavar "WAYPOINTS")

optionsParser :: Parser Options
optionsParser = Options
  <$> optional parseBackend
  <*> optional parseMaxCallDepth
  <*> parseInputFile
  <*> parseStartAddress
  <*> parseWaypoints

main :: IO ()
main = do
  opts <- execParser optsParser
  findRoutes opts
  where
    optsParser = info (optionsParser <**> helper)
      ( fullDesc
        <> progDesc "Finds high level routes in a binary that hit waypoints"
        <> header "Routes" )

newtype GetNodesContainingAddrError = AddressNotFound Address
  deriving (Eq, Ord, Read, Show)

getNodesContainingAddr
  :: HashSet PilNode
  -> Address
  -> Either GetNodesContainingAddrError (HashSet PilNode)
getNodesContainingAddr allNodes addr = case HashSet.null ns of
  True -> Left $ AddressNotFound addr
  False -> Right ns
  where
    ns = HashSet.filter (nodeContainsAddress addr) allNodes

getNodeSequenceContainingAddrs
  :: HashSet PilNode
  -> [Address]
  -> Either GetNodesContainingAddrError [HashSet PilNode]
getNodeSequenceContainingAddrs allNodes = traverse $ getNodesContainingAddr allNodes

-- | Checks for bugs by blindly sampling paths from every function
findRoutes :: Options -> IO ()
findRoutes opts = withBackend (opts ^. #backend) (opts ^. #inputFile) $ \imp -> do
  store <- Store.init Nothing imp
  (ctx, _innerNodes, allNodes) <-
    Store.getRouteMakerCtx' (fromMaybe defaultMaxCallDepth $ opts ^. #maxCallDepth) store
  case getNodeSequenceContainingAddrs allNodes (opts ^. #startAddress : NE.toList (opts ^. #waypoints)) of
    Left (AddressNotFound addr) -> do
      hPutStrLn stderr $ "Could not find any basic block containing address: " <> pretty' addr
      exitFailure
    Right combos -> do
      let r = getAllRoutesForAllSeqCombos ctx Nothing combos
      print $ HashSet.size <$> combos
      prettyPrint' $ AllRoutes r
      exitSuccess
