module CallGraph where

import Criterion.Main

import Blaze.Prelude
import qualified Blaze.Cfg as Cfg
import Blaze.Types.Graph (Graph)
import Binja.Function (Function, getFunctions)

import qualified Binja.Core as Binja

doBench :: Binja.BNBinaryView -> IO ()
doBench bv =
  defaultMain [bgroup "call graph" [bench "call graph" $ whnfIO (Cfg.getCallGraph bv $ getFunctions bv)]]

main :: IO ()
main = do
  eitherView <- Binja.getBinaryView "res/test_bins/Dive_Logger/Dive_Logger.bndb"
  case eitherView of 
    Right bv -> doBench bv
    Left err -> putStrLn err
  return ()