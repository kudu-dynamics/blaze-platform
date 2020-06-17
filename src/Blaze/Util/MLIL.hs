{-# LANGUAGE TemplateHaskell #-}
module Blaze.Util.MLIL where

import Blaze.Prelude

import qualified Prelude as P
import qualified Binja.Core as BN
import Binja.Core (BNBinaryView, InstructionIndex)
import Binja.Function (Function, MLILSSAFunction)
import qualified Binja.Function as Func
import Binja.MLIL (Instruction)
import qualified Binja.MLIL as MLIL
import qualified Data.Text as Text
import System.IO.Unsafe (unsafePerformIO)

type F = MLILSSAFunction

data FoundInstruction fun = FoundInstruction
  { _foundFunction :: Function
  , _foundIndex :: InstructionIndex fun
  , _foundInstruction :: Instruction fun
  } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''FoundInstruction)

getOperations :: Instruction fun -> [MLIL.Operation (MLIL.Expression fun)]
getOperations x = rootOp : foldr f [] rootOp
  where
    rootOp = x ^. MLIL.op
    f y ops = y ^. MLIL.op : foldr f ops (y ^. MLIL.op)

instructionContainsOp :: (MLIL.Operation (MLIL.Expression fun) -> Bool)
                      -> Instruction fun
                      -> Bool
instructionContainsOp f = any f . getOperations

getFoundFromFunction :: (MLIL.Operation (MLIL.Expression F) -> Bool)
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
                     -> (MLIL.Operation (MLIL.Expression F) -> Bool)
                     -> IO [FoundInstruction F]
getFoundInstructions bv f = do
  funcs <- Func.getFunctions bv
  concat <$> traverse (getFoundFromFunction f) funcs

matchInstructionByName :: Text -> MLIL.Operation (MLIL.Expression F) -> Bool
matchInstructionByName opName op =
  Text.takeWhile (/= ' ') (show op) == opName

-- convenience function, returns func name and statement index
getInstructionsWithOpByName :: Text -> FilePath -> IO [(Text, Int)]
getInstructionsWithOpByName opName binPath = do
  ebv <- BN.getBinaryView binPath
  case ebv of
    Left err -> P.error . cs $ err
    Right bv -> do
      BN.updateAnalysisAndWait bv
      xs <- getFoundInstructions bv (matchInstructionByName opName)
      return $ f <$> xs
  where
    f x = ( x ^. foundFunction . Func.name
          , fromIntegral $ x ^. foundIndex
          )
