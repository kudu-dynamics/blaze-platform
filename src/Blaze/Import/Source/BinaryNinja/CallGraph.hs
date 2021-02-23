module Blaze.Import.Source.BinaryNinja.CallGraph where

import Binja.Core (BNBinaryView, BNSymbol)
import qualified Binja.Core as Binja
import qualified Binja.Function as BnFunc
import qualified Binja.MLIL as Mlil
import qualified Binja.Reference as Ref
import qualified Binja.Variable as BnVar
import qualified Binja.View
import qualified Blaze.Types.Function as Func
import Blaze.Import.Source.BinaryNinja.Types hiding (CallSite)
import Blaze.Prelude hiding (Symbol)
import Blaze.Types.CallGraph (
  CallSite (
    CallSite
  ),
 )
import qualified Blaze.Types.CallGraph as CG
import Blaze.Types.Function (
  Function,
  FuncParamInfo (FuncParamInfo),
  ParamInfo (ParamInfo),
 )
import Control.Monad.Extra (mapMaybeM)
import Data.BinaryAnalysis (Symbol (Symbol, _symbolName, _symbolRawName))
import qualified Data.Set as Set
import qualified Data.Text as Text

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
          { _symbolName = sname
          , _symbolRawName = srawname
          }
  Nothing -> return Nothing

toBinjaFunction :: BNBinaryView -> Function -> IO (Maybe BnFunc.Function)
toBinjaFunction bv cgFunc =
  BnFunc.getFunctionStartingAt bv Nothing (cgFunc ^. #address)

convertFunction :: BNBinaryView -> BnFunc.Function -> IO Function
convertFunction bv bnf = do
  let name = bnf ^. BnFunc.name
      address = bnf ^. BnFunc.start
  symbol <- convertSymbol =<< Binja.View.getSymbolAtAddress bv address Nothing
  bnParams <- BnFunc.getFunctionParameterVariables bnf
  hasVarArgs <- BnFunc.hasVariableArguments bnf
  return
    Func.Function
      { symbol = symbol
      , name = name
      , address = address
      , params = FuncParamInfo . (`ParamInfo` Func.Unknown) . view BnVar.name <$> bnParams
      }

getCallInstruction :: BnFunc.Function -> Ref.ReferenceSource -> IO (Maybe CallInstruction)
getCallInstruction caller ref = do
  llilFunc <- BnFunc.getLLILFunction caller
  llilIndex <- Binja.getLLILInstructionIndexAtAddress caller (ref ^. Ref.arch) (ref ^. Ref.addr)
  mlilIndex <- Mlil.getMLILFromLLIL llilFunc llilIndex
  mlilFunc <- BnFunc.getMLILFunction caller
  mlilSSAFunc <- BnFunc.getMLILSSAFunction caller
  mlilSSAIndex <- Mlil.getMLILSSSAFromMLIL mlilFunc mlilIndex
  toCallInstruction <$> Mlil.instruction mlilSSAFunc mlilSSAIndex

getCallDestAddr :: CallInstruction -> Maybe Address
getCallDestAddr ci =
  case ci ^. #dest of
    Just dexpr ->
      case (dexpr ^. Mlil.op :: MLILSSAOp) of
        (Mlil.CONST_PTR cpop) -> Just $ fromIntegral $ cpop ^. Mlil.constant
        _ -> Nothing
    _ -> Nothing

createCallSite :: BNBinaryView -> BnFunc.Function -> CallInstruction -> IO (Maybe CallSite)
createCallSite bv bnCaller callInstr = do
  caller <- convertFunction bv bnCaller
  let instrAddr = callInstr ^. #address
      mDestAddr = getCallDestAddr callInstr
  case mDestAddr of
    Nothing -> return Nothing
    Just addr -> do
      mBnFunc <- BnFunc.getFunctionStartingAt bv Nothing addr
      case mBnFunc of
        Nothing -> return Nothing
        Just bnFunc -> do
          callee <- convertFunction bv bnFunc
          return $
            Just
              ( CallSite
                  { caller = caller
                  , address = instrAddr
                  , dest = CG.DestFunc callee
                  }
              )

getFunction :: BNBinaryView -> Address -> IO (Maybe Function)
getFunction bv addr = do
  func' <- BnFunc.getFunctionStartingAt bv Nothing addr :: IO (Maybe BnFunc.Function)
  traverse (convertFunction bv) func'

getFunctions :: BNBinaryView -> IO [Function]
getFunctions bv = BnFunc.getFunctions bv >>= traverse (convertFunction bv)

getCallSites :: BNBinaryView -> Function -> IO [CallSite]
getCallSites bv func' = do
  refs <- Ref.getCodeReferences bv (func' ^. #address)
  concatMapM getCallSites' refs
 where
  getMaybeCallerAndInstr :: BnFunc.Function -> Maybe CallInstruction -> Maybe (BnFunc.Function, CallInstruction)
  getMaybeCallerAndInstr bnFunc maybeInstr = (,) <$> Just bnFunc <*> maybeInstr

  getCallSites' :: Ref.ReferenceSource -> IO [CG.CallSite]
  getCallSites' ref = do
    callers <- Set.toList <$> Binja.getFunctionsContaining bv (ref ^. Ref.addr)
    mCallInstrs <- mapM (`getCallInstruction` ref) callers
    -- TODO: Is there a better way to link non-Nothing callers and call instructions?
    --       Right now we're fmap'ing a tuple constructor over them. This seems excessive.
    let callSiteArgs = catMaybes $ uncurry getMaybeCallerAndInstr <$> zip callers mCallInstrs
    mapMaybeM (uncurry $ createCallSite bv) callSiteArgs
