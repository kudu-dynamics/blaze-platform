{-# LANGUAGE DataKinds #-}
module Blaze.Solver.Op where

import Blaze.Prelude

import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil ( Expression( Expression )
                       , Stmt
                       , PilVar
                       , TypeEnv(TypeEnv)
                       )
import qualified Data.HashMap.Strict as HashMap
import Blaze.Types.Solver
import qualified Data.SBV.Trans as SBV
import qualified Data.SBV.Trans.Control as SBV
import qualified Data.Text as Text
import qualified Binja.Function as Func
import Data.SBV (SWord, SInt, fromSized, toSized, FromSized, ToSized)

add :: (SIntegral a) => SBV a -> SBV a -> SBV a
add a b = a + b



--- TODO: make handleSx and handleZx less boilerplatey
-- use GHC nats, perhaps

handleSx :: Pil.Type -> SymExpr -> Solver SymExpr
handleSx et x = case Pil.getTypeWidth et of
  (Just 8) -> case x of
    (SymWord8 v) -> return . SymWord64 . fromSized  $ (SBV.signExtend $ toSized v :: SWord 64)
    (SymInt8 v) -> return . SymInt64 . fromSized  $ (SBV.signExtend $ toSized v :: SInt 64)

    (SymWord16 v) -> return . SymWord64 . fromSized  $ (SBV.signExtend $ toSized v :: SWord 64)
    (SymInt16 v) -> return . SymInt64 . fromSized  $ (SBV.signExtend $ toSized v :: SInt 64)

    (SymWord32 v) -> return . SymWord64 . fromSized  $ (SBV.signExtend $ toSized v :: SWord 64)
    (SymInt32 v) -> return . SymInt64 . fromSized  $ (SBV.signExtend $ toSized v :: SInt 64)
    _ -> throwError SignExtendResultMustBeWiderThanArgument

  (Just 4) -> case x of
    (SymWord8 v) -> return . SymWord32 . fromSized  $ (SBV.signExtend $ toSized v :: SWord 32)
    (SymInt8 v) -> return . SymInt32 . fromSized  $ (SBV.signExtend $ toSized v :: SInt 32)

    (SymWord16 v) -> return . SymWord32 . fromSized  $ (SBV.signExtend $ toSized v :: SWord 32)
    (SymInt16 v) -> return . SymInt32 . fromSized  $ (SBV.signExtend $ toSized v :: SInt 32)
    _ -> throwError SignExtendResultMustBeWiderThanArgument

  (Just 2) -> case x of
    (SymWord8 v) -> return . SymWord16 . fromSized  $ (SBV.signExtend $ toSized v :: SWord 16)
    (SymInt8 v) -> return . SymInt16 . fromSized  $ (SBV.signExtend $ toSized v :: SInt 16)
    _ -> throwError SignExtendResultMustBeWiderThanArgument

  (Just n) -> throwError $ UnrecognizedTypeWidth n
  Nothing -> throwError ExpectedTypeWidth


handleZx :: Pil.Type -> SymExpr -> Solver SymExpr
handleZx et x = case Pil.getTypeWidth et of
  (Just 8) -> case x of
    (SymWord8 v) -> return . SymWord64 . fromSized  $ (SBV.zeroExtend $ toSized v :: SWord 64)
    (SymInt8 v) -> return . SymInt64 . fromSized  $ (SBV.zeroExtend $ toSized v :: SInt 64)

    (SymWord16 v) -> return . SymWord64 . fromSized  $ (SBV.zeroExtend $ toSized v :: SWord 64)
    (SymInt16 v) -> return . SymInt64 . fromSized  $ (SBV.zeroExtend $ toSized v :: SInt 64)

    (SymWord32 v) -> return . SymWord64 . fromSized  $ (SBV.zeroExtend $ toSized v :: SWord 64)
    (SymInt32 v) -> return . SymInt64 . fromSized  $ (SBV.zeroExtend $ toSized v :: SInt 64)
    _ -> throwError SignExtendResultMustBeWiderThanArgument

  (Just 4) -> case x of
    (SymWord8 v) -> return . SymWord32 . fromSized  $ (SBV.zeroExtend $ toSized v :: SWord 32)
    (SymInt8 v) -> return . SymInt32 . fromSized  $ (SBV.zeroExtend $ toSized v :: SInt 32)

    (SymWord16 v) -> return . SymWord32 . fromSized  $ (SBV.zeroExtend $ toSized v :: SWord 32)
    (SymInt16 v) -> return . SymInt32 . fromSized  $ (SBV.zeroExtend $ toSized v :: SInt 32)
    _ -> throwError SignExtendResultMustBeWiderThanArgument

  (Just 2) -> case x of
    (SymWord8 v) -> return . SymWord16 . fromSized  $ (SBV.zeroExtend $ toSized v :: SWord 16)
    (SymInt8 v) -> return . SymInt16 . fromSized  $ (SBV.zeroExtend $ toSized v :: SInt 16)
    _ -> throwError SignExtendResultMustBeWiderThanArgument

  (Just n) -> throwError $ UnrecognizedTypeWidth n
  Nothing -> throwError ExpectedTypeWidth

