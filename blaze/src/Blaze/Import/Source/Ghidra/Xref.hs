module Blaze.Import.Source.Ghidra.Xref where

import Blaze.Prelude hiding (Symbol)

import Blaze.Import.Source.Ghidra.Types (convertAddress, GhidraImporter(GhidraImporter))
import Blaze.Import.Source.Ghidra.CallGraph (mkInternalFunc)
import Blaze.Import.Xref (Xref(Xref))
import qualified Blaze.Import.Xref as Xref

import Ghidra.Core (runGhidraOrError)
import qualified Ghidra.Address as GAddr
import qualified Ghidra.Function as GFunc
import qualified Ghidra.Reference as GRef
import qualified Ghidra.State as State


-- | Get all xrefs to an address, returning only those from internal functions.
getXrefsTo :: GhidraImporter -> Address -> IO [Xref]
getXrefsTo imp@(GhidraImporter gs _ _) addr = do
  let prg = gs ^. #program
  refsWithAddrs <- runGhidraOrError $ do
    jaddr <- State.mkAddress prg addr
    refs <- GRef.getReferencesToAddress prg jaddr
    forM refs $ \ref -> do
      jFromAddr <- GRef.getFromAddress ref
      fromAddr <- GAddr.mkAddress jFromAddr
      mJFunc <- GFunc.fromAddr prg jFromAddr
      return (fromAddr, mJFunc)
  fmap catMaybes . forM refsWithAddrs $ \(fromAddr, mJFunc) ->
    case mJFunc of
      Nothing -> return Nothing
      Just jfunc -> do
        isExt <- runGhidraOrError $ GFunc.isExternal jfunc
        if isExt
          then return Nothing
          else do
            func <- mkInternalFunc imp jfunc
            return $ Just Xref
              { Xref.function = func
              , Xref.address = convertAddress fromAddr
              }
