{-# LANGUAGE TemplateHaskell #-}
module Blaze.Util.MLIL where

import Blaze.Prelude

import qualified Prelude as P
import qualified Binja.Core as BN
import Binja.Core ( BNBinaryView, InstructionIndex )
import Binja.C.Enums (BNVariableSourceType(StackVariableSourceType))
import Binja.Function (Function, MLILSSAFunction)
import qualified Binja.Function as Func
import qualified Binja.Variable as Var
import Binja.MLIL (Instruction, OperationSize)
import qualified Binja.MLIL as MLIL
import qualified Data.Text as Text

type F = MLILSSAFunction

data FoundInstruction fun = FoundInstruction
  { _foundFunction :: Function
  , _foundIndex :: InstructionIndex fun
  , _foundInstruction :: Instruction fun
  } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''FoundInstruction)

data OpWithSize fun = OpWithSize
  { _size :: MLIL.OperationSize
  , _op :: MLIL.Operation (MLIL.Expression fun)
  } deriving (Eq, Ord, Show)
$(makeFieldsNoPrefix ''OpWithSize)


getOperations :: forall fun. Instruction fun -> [OpWithSize fun]
getOperations x = rootOp : foldr f [] (x ^. MLIL.op)
  where
    rootOp = OpWithSize (x ^. MLIL.size) (x ^. MLIL.op)
    f y ops = (OpWithSize (x ^. MLIL.size) (y ^. MLIL.op)) : foldr f ops (y ^. MLIL.op)

instructionContainsOp :: (OpWithSize fun -> Bool)
                      -> Instruction fun
                      -> Bool
instructionContainsOp f = any f . getOperations

getFoundFromFunction :: (OpWithSize F -> Bool)
                     -> Function
                     -> IO [FoundInstruction F]
getFoundFromFunction f fn = do
  mlilFunc <- Func.convertFunction fn
  stmts <- MLIL.fromFunction mlilFunc
  return . mapMaybe g . indexed $ stmts
  where
    g (i, x) = if instructionContainsOp f x
      then Just $ FoundInstruction fn (fromIntegral i) x
      else Nothing

getFoundInstructions :: BNBinaryView
                     -> (OpWithSize F -> Bool)
                     -> IO [FoundInstruction F]
getFoundInstructions bv f = do
  funcs <- Func.getFunctions bv
  concat <$> traverse (getFoundFromFunction f) funcs

-- convenient..
getInstructionsWithOpAndSize :: (OpWithSize F -> Bool) -> FilePath -> IO [(Text, Int)]
getInstructionsWithOpAndSize g binPath = do
  ebv <- BN.getBinaryView binPath
  case ebv of
    Left err -> P.error . cs $ err
    Right bv -> do
      BN.updateAnalysisAndWait bv
      xs <- getFoundInstructions bv g
      return $ f <$> xs
  where
    f x = ( x ^. foundFunction . Func.name
          , fromIntegral $ x ^. foundIndex
          )

getInstructionsWithOp :: (MLIL.Operation (MLIL.Expression F) -> Bool) -> FilePath -> IO [(Text, Int)]
getInstructionsWithOp f fp = getInstructionsWithOpAndSize (f . view op) fp

-- convenience function, returns func name and statement index
getInstructionsWithOpByName :: Text -> FilePath -> IO [(Text, Int)]
getInstructionsWithOpByName opName binPath = do
  ebv <- BN.getBinaryView binPath
  case ebv of
    Left err -> P.error . cs $ err
    Right bv -> do
      BN.updateAnalysisAndWait bv
      xs <- getFoundInstructions bv (matchInstructionByName opName . view op)
      return $ f <$> xs
  where
    f x = ( x ^. foundFunction . Func.name
          , fromIntegral $ x ^. foundIndex
          )

-------------------
--- queries

matchInstructionByName :: Text -> MLIL.Operation (MLIL.Expression F) -> Bool
matchInstructionByName opName op =
  Text.takeWhile (/= ' ') (show op) == opName

-- looks like ADDRESS_OF is always with a stack var
matchNonStackAddressOf :: MLIL.Operation (MLIL.Expression F) -> Bool
matchNonStackAddressOf (MLIL.ADDRESS_OF x) =
  (x ^. MLIL.src . Var.sourceType) /= StackVariableSourceType
matchNonStackAddressOf _ = False

-- args are stored at positive location
matchPositiveStackOffset :: MLIL.Operation (MLIL.Expression F) -> Bool
matchPositiveStackOffset (MLIL.ADDRESS_OF x) =
  (x ^. MLIL.src . Var.sourceType) == StackVariableSourceType
  && (x ^. MLIL.src . Var.storage) >= 0
matchPositiveStackOffset _ = False

-- found 0
matchVarAliasedWithNonStackVar :: MLIL.Operation (MLIL.Expression F) -> Bool
matchVarAliasedWithNonStackVar (MLIL.VAR_ALIASED x) =
  x ^. MLIL.src . MLIL.var . Var.sourceType /= StackVariableSourceType
matchVarAliasedWithNonStackVar _ = False

-- found 0
matchSetVarAliasedWithNonStackVar :: MLIL.Operation (MLIL.Expression F) -> Bool
matchSetVarAliasedWithNonStackVar (MLIL.SET_VAR_ALIASED x) =
  x ^. MLIL.prev . MLIL.src . MLIL.var . Var.sourceType
  /= StackVariableSourceType
matchSetVarAliasedWithNonStackVar _ = False

-- found some in 
matchVarFieldWithNonZeroOffset :: MLIL.Operation (MLIL.Expression F) -> Bool
matchVarFieldWithNonZeroOffset (MLIL.VAR_SSA_FIELD x) =
  x ^. MLIL.offset /= 0
matchVarFieldWithNonZeroOffset _ = False


-- found 0 in , , ls, kill
matchSetVarFieldWhereVarHasNoWidth :: MLIL.Operation (MLIL.Expression F) -> Bool
matchSetVarFieldWhereVarHasNoWidth (MLIL.SET_VAR_SSA_FIELD x) =
  isNothing $ x ^. MLIL.prev . MLIL.src . MLIL.var . Var.varType
matchSetVarFieldWhereVarHasNoWidth _ = False

-- 0 in , , kill, ls
matchSetVarFieldWithNegativeOffset :: MLIL.Operation (MLIL.Expression F) -> Bool
matchSetVarFieldWithNegativeOffset (MLIL.SET_VAR_SSA_FIELD x) =
  (x ^. MLIL.offset) < 0
matchSetVarFieldWithNegativeOffset _ = False

-- 16 in , of 3688 adds
matchAddWhereArgSizesUnequal :: MLIL.Operation (MLIL.Expression F) -> Bool
matchAddWhereArgSizesUnequal (MLIL.ADD x) =
  (x ^. MLIL.left . MLIL.size /= x ^. MLIL.right . MLIL.size)
matchAddWhereArgSizesUnequal _ = False

-- of 3688
-- 2384 left == right == ret
-- 16 left /= right
-- 
matchAddReturnAndFirstArgSizeUnequal :: OpWithSize F -> Bool
matchAddReturnAndFirstArgSizeUnequal (OpWithSize sz (MLIL.ADD x)) =
  (x ^. MLIL.right . MLIL.size /= sz) ||
  (x ^. MLIL.left . MLIL.size /= sz)
matchAddReturnAndFirstArgSizeUnequal _ = False

