module Blaze.Import.Source.BinaryNinja.CallGraph where

import Binja.Core (BNBinaryView, BNSymbol)
import qualified Binja.Core as Binja
import qualified Binja.Function as BnFunc
import qualified Binja.MLIL as Mlil
import qualified Binja.Reference as Ref
import qualified Binja.Variable as BnVar
import qualified Binja.View
import qualified Blaze.Types.Function as Func
import Blaze.Import.Source.BinaryNinja.Types hiding (callInstr, caller, address, params, CallSite)
import Blaze.Prelude hiding (Symbol)
import Blaze.Types.CallGraph (
  CallSite (
    CallSite
  ),
 )
import qualified Blaze.Types.CallGraph as CG
import Blaze.Types.Function (
  Function,
  FuncParamInfo (FuncParamInfo, FuncVarArgInfo),
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
  let name = truncateMiddle maxFunctionNameLength $ bnf ^. BnFunc.name
      address = bnf ^. BnFunc.start
  symbol <- convertSymbol =<< Binja.View.getSymbolAtAddress bv address Nothing
  bnParams <- BnFunc.getFunctionParameterVariables bnf
  hasVarArgs <- BnVar._value <$> BnFunc.hasVariableArguments bnf
  -- Append "#0" as we are importing MLIL SSA vars with versions embedded in the name,
  -- but BN uses the Variable (rather than SSAVariable) to refer to the parameters.
  let params =
        if hasVarArgs
          then
            let posParams =
                  fmap (FuncParamInfo . (`ParamInfo` Func.Unknown) . getSymbol 0)
                    <$> initMay bnParams
                varArgParam =
                  FuncVarArgInfo . (`ParamInfo` Func.Unknown) . getSymbol 0
                    <$> lastMay bnParams
             in fromMaybe [] (fmap (++) posParams <*> sequence [varArgParam])
          else
            FuncParamInfo . (`ParamInfo` Func.Unknown) . getSymbol 0
              <$> bnParams

  -- varParam = FuncParamInfo (ParamInfo )
  return
    Func.Function
      { symbol = symbol
      , name = name
      , address = address
      , params = params
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

createCallSite :: BNBinaryView -> BnFunc.Function -> Address -> Maybe Address -> IO (Maybe CallSite)
createCallSite bv bnCaller instrAddr destAddr = do
  caller <- convertFunction bv bnCaller
  -- let instrAddr = callInstr ^. #address
  --     mDestAddr = getCallDestAddr callInstr
  case destAddr of
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
  refs <- Ref.getCodeReferences bv calleeAddr
  concatMapM (getCallSites' calleeAddr) refs
 where
  calleeAddr :: Address
  calleeAddr = func' ^. #address

  getCallSites' :: Address -> Ref.ReferenceSource -> IO [CG.CallSite]
  getCallSites' destAddr ref = do
    let callInstrAddr = ref ^. Ref.addr
    callers <- Set.toList <$> Binja.getFunctionsContaining bv callInstrAddr
    mapMaybeM
      (\caller -> createCallSite bv caller callInstrAddr (Just destAddr))
      callers
