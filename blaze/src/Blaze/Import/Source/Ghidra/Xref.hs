module Blaze.Import.Source.Ghidra.Xref where

import Blaze.Prelude hiding (Symbol)

import Blaze.Import.Source.Ghidra.Types (convertAddress, GhidraImporter)
import Blaze.Import.Source.Ghidra.CallGraph (mkFunctionRef)
import Blaze.Import.Xref (Xref(Xref))
import qualified Blaze.Import.Xref as Xref

import Ghidra.Core (runGhidraOrError)
import qualified Ghidra.Address as GAddr
import qualified Ghidra.Function as GFunc
import qualified Ghidra.Reference as GRef
import qualified Ghidra.State as State


-- | Get all xrefs to an address, returning only those from internal functions.
getXrefsTo :: GhidraImporter -> Address -> IO [Xref]
getXrefsTo imp addr = do
  let prg = imp ^. #ghidraState . #program
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
        -- Resolve thunks: if the containing function is a thunk,
        -- attribute the xref to the real (dethunked) function so it
        -- matches what's in the CfgStore function list.
        dethunkedJFunc <- runGhidraOrError $ GFunc.resolveThunk jfunc
        isExt <- runGhidraOrError $ GFunc.isExternal dethunkedJFunc
        if isExt
          then return Nothing
          else do
            func <- mkFunctionRef dethunkedJFunc
            return $ Just Xref
              { Xref.function = func
              , Xref.address = convertAddress fromAddr
              }
