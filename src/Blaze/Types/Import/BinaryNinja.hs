{-# LANGUAGE TemplateHaskell #-}

module Blaze.Types.Import.BinaryNinja where

import qualified Binja.Core as Binja
import Binja.Core (BNBinaryView, BNSymbol)
import qualified Binja.Function as BNFunc
import qualified Binja.MLIL as MLIL
import qualified Binja.Reference as Ref
import qualified Binja.View
import Blaze.Function (CallInstruction, toCallInstruction)
import qualified Blaze.Function as Func
import Blaze.Import.CallGraph (CallGraphImporter (getCallSites, getFunctions))
import Blaze.Prelude hiding (Symbol)
import qualified Blaze.Types.CallGraph as CG
import Blaze.Types.CallGraph (CallSite (CallSite, _callSiteAddress, _callSiteCaller, _callSiteDest))
import Control.Monad.Extra (mapMaybeM)
import Data.BinaryAnalysis (Address, Symbol (Symbol, _symbolName, _symbolRawName))
import Data.Set as Set
import qualified Data.Text as Text

newtype BNImporter
  = BNImporter
      { _binaryView :: BNBinaryView
      }
  deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''BNImporter)

-- TODO: Have a proper Symbol type in BN bindings with fields already populated
--       so that IO is not needed here
convertSymbol :: Maybe BNSymbol -> IO (Maybe Symbol)
convertSymbol mbns = case mbns of
  (Just bns) -> do
    sname <- Text.pack <$> Binja.getSymbolFullName bns
    srawname <- Text.pack <$> Binja.getSymbolRawName bns
    return $
      Just
        Symbol
          { _symbolName = sname,
            _symbolRawName = srawname
          }
  Nothing -> return Nothing

convertFunction :: BNBinaryView -> BNFunc.Function -> IO CG.Function
convertFunction bv bnf = do
  let name = bnf ^. BNFunc.name
      address = bnf ^. BNFunc.start
  symbol <- convertSymbol =<< Binja.View.getSymbolAtAddress bv address Nothing
  return
    CG.Function
      { _functionSymbol = symbol,
        _functionName = name,
        _functionAddress = address
      }

getCallInstruction :: BNFunc.Function -> Ref.ReferenceSource -> IO (Maybe CallInstruction)
getCallInstruction caller ref = do
  llilFunc <- BNFunc.getLLILFunction caller
  llilIndex <- Binja.getLLILInstructionIndexAtAddress caller (ref ^. Ref.arch) (ref ^. Ref.addr)
  mlilIndex <- MLIL.getMLILFromLLIL llilFunc llilIndex
  mlilFunc <- BNFunc.getMLILFunction caller
  mlilSSAFunc <- BNFunc.getMLILSSAFunction caller
  mlilSSAIndex <- MLIL.getMLILSSSAFromMLIL mlilFunc mlilIndex
  toCallInstruction <$> MLIL.instruction mlilSSAFunc mlilSSAIndex

type MLILSSAOp = MLIL.Operation (MLIL.Expression BNFunc.MLILSSAFunction)

getCallDestAddr :: CallInstruction -> Maybe Address
getCallDestAddr ci =
  case ci ^. Func.dest of
    Just dexpr ->
      case (dexpr ^. MLIL.op :: MLILSSAOp) of
        (MLIL.CONST_PTR cpop) -> Just $ fromIntegral $ cpop ^. MLIL.constant
        _ -> Nothing
    _ -> Nothing

createCallSite :: BNBinaryView -> BNFunc.Function -> CallInstruction -> IO (Maybe CallSite)
createCallSite bv bnCaller callInstr = do
  caller <- convertFunction bv bnCaller
  let instrAddr = callInstr ^. Func.address
      mDestAddr = getCallDestAddr callInstr
  case mDestAddr of
    Nothing -> return Nothing
    Just addr -> do
      mBnFunc <- BNFunc.getFunctionStartingAt bv Nothing addr
      case mBnFunc of
        Nothing -> return Nothing
        Just bnFunc -> do
          callee <- convertFunction bv bnFunc
          return $
            Just
              ( CallSite
                  { _callSiteCaller = caller,
                    _callSiteAddress = instrAddr,
                    _callSiteDest = CG.DestFunc callee
                  }
              )

instance CallGraphImporter BNImporter where
  getFunctions imp = do
    let bv = imp ^. binaryView
    funcs <- BNFunc.getFunctions bv
    sequence $ convertFunction bv <$> funcs

  getCallSites imp func = do
    let bv = imp ^. binaryView
    refs <- Ref.getCodeReferences bv (func ^. CG.address)
    concatMapM (getCallSites' bv) refs
    where
      getMaybeCallerAndInstr :: BNFunc.Function -> Maybe CallInstruction -> Maybe (BNFunc.Function, CallInstruction)
      getMaybeCallerAndInstr bnFunc maybeInstr = (,) <$> Just bnFunc <*> maybeInstr
      getCallSites' :: BNBinaryView -> Ref.ReferenceSource -> IO [CG.CallSite]
      getCallSites' bv ref = do
        callers <- Set.toList <$> Binja.getFunctionsContaining bv (ref ^. Ref.addr)
        mCallInstrs <- mapM (`getCallInstruction` ref) callers
        -- TODO: Is there a better way to link non-Nothing callers and call instructions?
        --       Right now we're fmap'ing a tuple constructor over them. This seems excessive.
        let callSiteArgs = catMaybes $ uncurry getMaybeCallerAndInstr <$> zip callers mCallInstrs
        mapMaybeM (uncurry $ createCallSite bv) callSiteArgs
