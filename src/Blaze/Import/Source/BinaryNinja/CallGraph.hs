module Blaze.Import.Source.BinaryNinja.CallGraph where
  
import qualified Binja.Core as Binja
import Binja.Core (BNBinaryView, BNSymbol)
import qualified Binja.Function as BNFunc
import qualified Binja.MLIL as Mlil
import qualified Binja.Reference as Ref
import qualified Binja.View
import Blaze.Types.Function (CallInstruction, toCallInstruction)
import Blaze.Prelude hiding (Symbol)
import qualified Blaze.Types.CallGraph as CG
import Blaze.Types.CallGraph
  ( CallSite
      ( CallSite
      ),
  )
import Control.Monad.Extra (mapMaybeM)
import Data.BinaryAnalysis (Symbol (Symbol, _symbolName, _symbolRawName))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Blaze.Import.Source.BinaryNinja.Types

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

toBinjaFunction :: BNBinaryView -> CG.Function -> IO (Maybe BNFunc.Function)
toBinjaFunction bv cgFunc =
  BNFunc.getFunctionStartingAt bv Nothing (cgFunc ^. #address)

convertFunction :: BNBinaryView -> BNFunc.Function -> IO CG.Function
convertFunction bv bnf = do
  let name = bnf ^. BNFunc.name
      address = bnf ^. BNFunc.start
  symbol <- convertSymbol =<< Binja.View.getSymbolAtAddress bv address Nothing
  return
    CG.Function
      { symbol = symbol
      , name = name
      , address = address
      }

getCallInstruction :: BNFunc.Function -> Ref.ReferenceSource -> IO (Maybe CallInstruction)
getCallInstruction caller ref = do
  llilFunc <- BNFunc.getLLILFunction caller
  llilIndex <- Binja.getLLILInstructionIndexAtAddress caller (ref ^. Ref.arch) (ref ^. Ref.addr)
  mlilIndex <- Mlil.getMLILFromLLIL llilFunc llilIndex
  mlilFunc <- BNFunc.getMLILFunction caller
  mlilSSAFunc <- BNFunc.getMLILSSAFunction caller
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

createCallSite :: BNBinaryView -> BNFunc.Function -> CallInstruction -> IO (Maybe CallSite)
createCallSite bv bnCaller callInstr = do
  caller <- convertFunction bv bnCaller
  let instrAddr = callInstr ^. #address
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
              (CallSite
                 { caller = caller
                 , address = instrAddr
                 , dest = CG.DestFunc callee
                 })

getFunction :: BNBinaryView -> Address -> IO (Maybe CG.Function)
getFunction bv addr = do
  func' <- BNFunc.getFunctionStartingAt bv Nothing addr :: IO (Maybe BNFunc.Function)
  traverse (convertFunction bv) func'

getFunctions :: BNBinaryView -> IO [CG.Function]
getFunctions bv = BNFunc.getFunctions bv >>= traverse (convertFunction bv)

getCallSites :: BNBinaryView -> CG.Function -> IO [CallSite]
getCallSites bv func' = do
  refs <- Ref.getCodeReferences bv (func' ^. #address)
  concatMapM getCallSites' refs
  where
    getMaybeCallerAndInstr :: BNFunc.Function -> Maybe CallInstruction -> Maybe (BNFunc.Function, CallInstruction)
    getMaybeCallerAndInstr bnFunc maybeInstr = (,) <$> Just bnFunc <*> maybeInstr

    getCallSites' :: Ref.ReferenceSource -> IO [CG.CallSite]
    getCallSites' ref = do
      callers <- Set.toList <$> Binja.getFunctionsContaining bv (ref ^. Ref.addr)
      mCallInstrs <- mapM (`getCallInstruction` ref) callers
      -- TODO: Is there a better way to link non-Nothing callers and call instructions?
      --       Right now we're fmap'ing a tuple constructor over them. This seems excessive.
      let callSiteArgs = catMaybes $ uncurry getMaybeCallerAndInstr <$> zip callers mCallInstrs
      mapMaybeM (uncurry $ createCallSite bv) callSiteArgs

  
