{-# LANGUAGE TemplateHaskell #-}
module Blaze.Util.MLIL where

import Blaze.Prelude

import qualified Binja.Core as BN
import Binja.Core ( BNBinaryView, InstructionIndex )
import Binja.Function (Function, MLILSSAFunction)
import qualified Binja.Function as Func
import Binja.MLIL (Instruction)
import qualified Binja.MLIL as MLIL
import qualified Data.Text as Text
import Blaze.Pretty (showHex)

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
    f y ops = OpWithSize (y ^. MLIL.size) (y ^. MLIL.op) : foldr f ops (y ^. MLIL.op)

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
  bv <- unsafeFromRight <$> BN.getBinaryView binPath
  BN.updateAnalysisAndWait bv
  xs <- getFoundInstructions bv g
  return $ f <$> xs
  where
    f x = ( x ^. foundFunction . Func.name
            <> " @ " <> showHex (x ^. foundFunction . Func.start)
          , fromIntegral $ x ^. foundIndex
          )

getInstructionsWithOp :: (MLIL.Operation (MLIL.Expression F) -> Bool) -> FilePath -> IO [(Text, Int)]
getInstructionsWithOp f = getInstructionsWithOpAndSize $ f . view op

-- convenience function, returns func name and statement index
getInstructionsWithOpByName :: Text -> FilePath -> IO [(Text, Int)]
getInstructionsWithOpByName opName binPath = do
  bv <- unsafeFromRight <$> BN.getBinaryView binPath
  BN.updateAnalysisAndWait bv
  xs <- getFoundInstructions bv (matchInstructionByName opName . view op)
  return $ f <$> xs
  where
    f x = ( x ^. foundFunction . Func.name
          , fromIntegral $ x ^. foundIndex
          )

matchInstructionByName :: Text -> MLIL.Operation (MLIL.Expression F) -> Bool
matchInstructionByName opName op' =
  Text.takeWhile (/= ' ') (show op') == opName
