{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Evaluate" -}
{- HLINT ignore "Use uncurry" -}
{- HLINT ignore "Move brackets to avoid $" -}

module Main (main) where

import Flint.Prelude hiding (const)

import qualified Flint.Cfg.Store as Store
import qualified Flint.Types.CachedCalc as CC

import qualified Blaze.Graph as G
import Blaze.Import.CallGraph (CallGraphImporter)
import Blaze.Import.Cfg (CfgImporter, NodeDataType)
#ifdef FLINT_SUPPORT_BINARYNINJA
import Blaze.Import.Source.BinaryNinja (BNImporter)
#endif
import Blaze.Import.Source.Ghidra (GhidraImporter)
import Blaze.Import.Binary (BinaryImporter(openBinary))
import Blaze.Types.Cfg (PilNode)

import qualified Data.HashSet as HashSet
import qualified Data.Text as Text


tryPlt
  :: ( CallGraphImporter imp
     , NodeDataType imp ~ PilNode
     , CfgImporter imp
     )
  => imp
  -> IO ()
tryPlt imp = do
  store' <- Store.init Nothing imp
  (Just cg) <- CC.get () (store' ^. #callGraphCache)

  let foos = filter (\fn -> "foo" `Text.isInfixOf` (fn ^. #name))
             . HashSet.toList
             $ G.nodes cg
  pprint foos

#ifdef FLINT_SUPPORT_BINARYNINJA
binjaPlt :: IO ()
binjaPlt = do
  putText "starting binja"
  imp <- openBinary "res/test_bins/plt_thunk/libplt_thunk.so" >>= \case
    (Right (imp :: BNImporter)) -> return imp
    (Left err) -> error $ cs err
  putText "Loaded libplt_thunk.so with Binja"
  tryPlt imp
#endif

ghidraPlt :: IO ()
ghidraPlt = do
  putText "starting ghidra"
  imp <- openBinary "res/test_bins/plt_thunk/libplt_thunk.so" >>= \case
    (Right (imp :: GhidraImporter)) -> return imp
    (Left err) -> error $ cs err
  putText "Loaded libplt_thunk.so with Ghidra"
  tryPlt imp

main :: IO ()
main = do
#ifdef FLINT_SUPPORT_BINARYNINJA
  binjaPlt
#endif
  ghidraPlt
