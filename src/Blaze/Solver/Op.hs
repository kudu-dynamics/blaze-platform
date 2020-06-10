{-# LANGUAGE RankNTypes #-}
-- {-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Blaze.Solver.Op where

import Blaze.Prelude

import qualified Blaze.Types.Pil as Pil
import qualified Data.SBV.Dynamic as D
import Data.SBV.Internals (SBV(SBV))
import Blaze.Types.Pil ( PilVar
                       )
import qualified Data.HashMap.Strict as HashMap
import Blaze.Types.Solver
import qualified Data.SBV.Trans as SBV
import Data.SBV.List as SList
import GHC.TypeNats

add :: (SIntegral a) => SBV a -> SBV a -> SBV a
add a b = a + b

--- TODO: make handleSx and handleZx less boilerplatey
-- use GHC nats, perhaps

handleSx :: Pil.Type -> SymExpr -> Solver SymExpr
handleSx et x = case Pil.getTypeBitWidth et of
  (Just 128) -> case x of
    (SymWord8 v) -> return . SymWord128 $ (SBV.signExtend $ toSized v :: SWord 128)
    (SymInt8 v) -> return . SymInt128 $ (SBV.signExtend $ toSized v :: SInt 128)

    (SymWord16 v) -> return . SymWord128 $ (SBV.signExtend $ toSized v :: SWord 128)
    (SymInt16 v) -> return . SymInt128 $ (SBV.signExtend $ toSized v :: SInt 128)

    (SymWord32 v) -> return . SymWord128 $ (SBV.signExtend $ toSized v :: SWord 128)
    (SymInt32 v) -> return . SymInt128 $ (SBV.signExtend $ toSized v :: SInt 128)

    (SymWord64 v) -> return . SymWord128 $ (SBV.signExtend $ toSized v :: SWord 128)
    (SymInt64 v) -> return . SymInt128 $ (SBV.signExtend $ toSized v :: SInt 128)

    _ -> throwError SignExtendResultMustBeWiderThanArgument

  (Just 64) -> case x of
    (SymWord8 v) -> return . SymWord64 . fromSized  $ (SBV.signExtend $ toSized v :: SWord 64)
    (SymInt8 v) -> return . SymInt64 . fromSized  $ (SBV.signExtend $ toSized v :: SInt 64)

    (SymWord16 v) -> return . SymWord64 . fromSized  $ (SBV.signExtend $ toSized v :: SWord 64)
    (SymInt16 v) -> return . SymInt64 . fromSized  $ (SBV.signExtend $ toSized v :: SInt 64)

    (SymWord32 v) -> return . SymWord64 . fromSized  $ (SBV.signExtend $ toSized v :: SWord 64)
    (SymInt32 v) -> return . SymInt64 . fromSized  $ (SBV.signExtend $ toSized v :: SInt 64)
    _ -> throwError SignExtendResultMustBeWiderThanArgument

  (Just 32) -> case x of
    (SymWord8 v) -> return . SymWord32 . fromSized  $ (SBV.signExtend $ toSized v :: SWord 32)
    (SymInt8 v) -> return . SymInt32 . fromSized  $ (SBV.signExtend $ toSized v :: SInt 32)

    (SymWord16 v) -> return . SymWord32 . fromSized  $ (SBV.signExtend $ toSized v :: SWord 32)
    (SymInt16 v) -> return . SymInt32 . fromSized  $ (SBV.signExtend $ toSized v :: SInt 32)
    _ -> throwError SignExtendResultMustBeWiderThanArgument

  (Just 16) -> case x of
    (SymWord8 v) -> return . SymWord16 . fromSized  $ (SBV.signExtend $ toSized v :: SWord 16)
    (SymInt8 v) -> return . SymInt16 . fromSized  $ (SBV.signExtend $ toSized v :: SInt 16)
    _ -> throwError SignExtendResultMustBeWiderThanArgument

  (Just n) -> throwError $ UnrecognizedTypeWidth (fromIntegral n)
  Nothing -> throwError ExpectedTypeWidth


handleZx :: Pil.Type -> SymExpr -> Solver SymExpr
handleZx et x = case Pil.getTypeBitWidth et of
  (Just 128) -> case x of
    (SymWord8 v) -> return . SymWord128 $ (SBV.zeroExtend $ toSized v :: SWord 128)
    (SymInt8 v) -> return . SymInt128 $ (SBV.zeroExtend $ toSized v :: SInt 128)

    (SymWord16 v) -> return . SymWord128 $ (SBV.zeroExtend $ toSized v :: SWord 128)
    (SymInt16 v) -> return . SymInt128 $ (SBV.zeroExtend $ toSized v :: SInt 128)

    (SymWord32 v) -> return . SymWord128 $ (SBV.zeroExtend $ toSized v :: SWord 128)
    (SymInt32 v) -> return . SymInt128 $ (SBV.zeroExtend $ toSized v :: SInt 128)

    (SymWord64 v) -> return . SymWord128 $ (SBV.zeroExtend $ toSized v :: SWord 128)
    (SymInt64 v) -> return . SymInt128 $ (SBV.zeroExtend $ toSized v :: SInt 128)

    _ -> throwError SignExtendResultMustBeWiderThanArgument

  (Just 64) -> case x of
    (SymWord8 v) -> return . SymWord64 . fromSized  $ (SBV.zeroExtend $ toSized v :: SWord 64)
    (SymInt8 v) -> return . SymInt64 . fromSized  $ (SBV.zeroExtend $ toSized v :: SInt 64)

    (SymWord16 v) -> return . SymWord64 . fromSized  $ (SBV.zeroExtend $ toSized v :: SWord 64)
    (SymInt16 v) -> return . SymInt64 . fromSized  $ (SBV.zeroExtend $ toSized v :: SInt 64)

    (SymWord32 v) -> return . SymWord64 . fromSized  $ (SBV.zeroExtend $ toSized v :: SWord 64)
    (SymInt32 v) -> return . SymInt64 . fromSized  $ (SBV.zeroExtend $ toSized v :: SInt 64)
    _ -> throwError SignExtendResultMustBeWiderThanArgument

  (Just 32) -> case x of
    (SymWord8 v) -> return . SymWord32 . fromSized  $ (SBV.zeroExtend $ toSized v :: SWord 32)
    (SymInt8 v) -> return . SymInt32 . fromSized  $ (SBV.zeroExtend $ toSized v :: SInt 32)

    (SymWord16 v) -> return . SymWord32 . fromSized  $ (SBV.zeroExtend $ toSized v :: SWord 32)
    (SymInt16 v) -> return . SymInt32 . fromSized  $ (SBV.zeroExtend $ toSized v :: SInt 32)
    _ -> throwError SignExtendResultMustBeWiderThanArgument

  (Just 16) -> case x of
    (SymWord8 v) -> return . SymWord16 . fromSized  $ (SBV.zeroExtend $ toSized v :: SWord 16)
    (SymInt8 v) -> return . SymInt16 . fromSized  $ (SBV.zeroExtend $ toSized v :: SInt 16)
    _ -> throwError SignExtendResultMustBeWiderThanArgument

  (Just n) -> throwError $ UnrecognizedTypeWidth (fromIntegral n)
  Nothing -> throwError ExpectedTypeWidth


handleLowPart :: Pil.Type -> SymExpr -> Solver SymExpr
handleLowPart et x = case Pil.getTypeBitWidth et of
  (Just 128) -> case x of
    -- no change because 8 bytes returns the whole thing
    (SymWord128 v) -> return . SymWord128 $ v
    (SymInt128 v) -> return . SymInt128 $ v
    _ -> throwError UnexpectedArgType

    
  (Just 64) -> case x of
    -- no change because 8 bytes returns the whole thing
    (SymWord64 v) -> return . SymWord64 $ v
    (SymInt64 v) -> return . SymInt64 $ v

    (SymWord128 v) -> return . SymWord64 . fromSized $ SBV.bvDrop (Proxy @ 64) v
    (SymInt128 v) -> return . SymInt64 . fromSized $ SBV.bvDrop (Proxy @ 64) v
    _ -> throwError UnexpectedArgType

  (Just 32) -> case x of
    (SymWord32 v) -> return . SymWord32 $ v
    (SymInt32 v) -> return . SymInt32 $ v

    (SymWord64 v) -> return . SymWord32 . fromSized $ SBV.bvDrop (Proxy @ 32) (toSized v)
    (SymInt64 v) -> return . SymInt32 . fromSized $ SBV.bvDrop (Proxy @ 32) (toSized v)

    (SymWord128 v) -> return . SymWord32 . fromSized $ SBV.bvDrop (Proxy @ 96) v
    (SymInt128 v) -> return . SymInt32 . fromSized $ SBV.bvDrop (Proxy @ 96) v

    _ -> throwError UnexpectedArgType

  (Just 16) -> case x of
    (SymWord16 v) -> return . SymWord16 $ v
    (SymInt16 v) -> return . SymInt16 $ v
    
    (SymWord32 v) -> return . SymWord16 . fromSized $ SBV.bvDrop (Proxy @ 16) (toSized v)
    (SymInt32 v) -> return . SymInt16 . fromSized $ SBV.bvDrop (Proxy @ 16) (toSized v)

    (SymWord64 v) -> return . SymWord16 . fromSized $ SBV.bvDrop (Proxy @ 48) (toSized v)
    (SymInt64 v) -> return . SymInt16 . fromSized $ SBV.bvDrop (Proxy @ 48) (toSized v)

    (SymWord128 v) -> return . SymWord16 . fromSized $ SBV.bvDrop (Proxy @ 112) v
    (SymInt128 v) -> return . SymInt16 . fromSized $ SBV.bvDrop (Proxy @ 112) v

    _ -> throwError UnexpectedArgType


  (Just 8) -> case x of
    (SymWord8 v) -> return . SymWord8 $ v
    (SymInt8 v) -> return . SymInt8 $ v

    (SymWord16 v) -> return . SymWord8 . fromSized $ SBV.bvDrop (Proxy @ 8) (toSized v)
    (SymInt16 v) -> return . SymInt8 . fromSized $ SBV.bvDrop (Proxy @ 8) (toSized v)
    
    (SymWord32 v) -> return . SymWord8 . fromSized $ SBV.bvDrop (Proxy @ 24) (toSized v)
    (SymInt32 v) -> return . SymInt8 . fromSized $ SBV.bvDrop (Proxy @ 24) (toSized v)

    (SymWord64 v) -> return . SymWord8 . fromSized $ SBV.bvDrop (Proxy @ 56) (toSized v)
    (SymInt64 v) -> return . SymInt8 . fromSized $ SBV.bvDrop (Proxy @ 56) (toSized v)

    (SymWord128 v) -> return . SymWord8 . fromSized $ SBV.bvDrop (Proxy @ 120) v
    (SymInt128 v) -> return . SymInt8 . fromSized $ SBV.bvDrop (Proxy @ 120) v

    _ -> throwError UnexpectedArgType  

  (Just n) -> throwError $ UnrecognizedTypeWidth (fromIntegral n)
  Nothing -> throwError ExpectedTypeWidth


atLeastOneIsNaN :: IEEEFloating a => SBV a -> SBV a -> SBool
atLeastOneIsNaN a b = fpIsNaN a .|| fpIsNaN b

neitherIsNaN :: IEEEFloating a => SBV a -> SBV a -> SBool
neitherIsNaN a b = sNot $ atLeastOneIsNaN a b

integralNeg :: SIntegral a => SBV a -> SBV a
integralNeg x = x * (-1)


testSBit :: (SFiniteBits a, SIntegral b) => SBV a -> SBV b -> SBool
testSBit n bitIndex =
  (ix .< SList.length bits) .&& SList.elemAt bits ix
  where
    ix :: SInteger
    ix = sFromIntegral bitIndex

    bits :: SList Bool
    bits = SList.implode $ blastLE n

rotateLeftWithCarry :: forall a b c bv.
                        ( IsNonZero a
                        , KnownNat a
                        , KnownNat (1 + a)
                        , SymVal (bv a)
                        , SIntegral (bv (1 + a))
                        , (2 <=? (1 + a)) ~ 'True
                        , (((1 + a) - (1 + a)) <=? 0) ~ 'True
                        , IsNonZero (1 + a)
                        , IsNonZero ((1 + a) - 1)
                        , SFiniteBits (bv 1)
                        , SIntegral b, SFiniteBits c)
                     => SBV (bv a) -> SBV b -> SBV c
                     -> SBV (bv a)
rotateLeftWithCarry n rot carry = 
  bvDrop (Proxy @1) $ sRotateLeft (carryBit # n) rot
  where
    carryBit :: SBV (bv 1)
    carryBit = fromBitsLE [lsb carry]

rotateRightWithCarry :: forall a b c bv.
                        ( IsNonZero a
                        , IsNonZero (a + 1)
                        , KnownNat a
                        , KnownNat (a + 1)
                        , SymVal (bv a)
                        , SIntegral (bv (a + 1))
                        , (a <=? (a + 1)) ~ 'True
                        , SFiniteBits (bv 1)
                        , SIntegral b, SFiniteBits c)
                     => SBV (bv a) -> SBV b -> SBV c
                     -> SBV (bv a)
rotateRightWithCarry n rot carry = 
  bvTake (Proxy @a) $ sRotateRight (n # carryBit) rot
  where
    carryBit :: SBV (bv 1)
    carryBit = fromBitsLE [lsb carry]
  

handleRRC :: Pil.Type -> SymExpr -> SymExpr -> SymExpr
          -> Solver SymExpr
handleRRC et n rot' carry' = do
  bool (throwError $ ArgAndRetNotTheSameType (symType n)) (return ())
    $ sameType n et
  -- all we care about is the lsb, so we can just convert carry to a word8
  carry <- getIntegral carry' :: Solver SWord8
  rot <- getIntegral rot' :: Solver SWord64

  -- todo -- reduce boilerplate
  case n of
    (SymWord8 x) -> return . SymWord8 . fromSized $ rotateRightWithCarry (toSized x) rot carry
    (SymWord16 x) -> return . SymWord16 . fromSized $ rotateRightWithCarry (toSized x) rot carry
    (SymWord32 x) -> return . SymWord32 . fromSized $ rotateRightWithCarry (toSized x) rot carry
    (SymWord64 x) -> return . SymWord64 . fromSized $ rotateRightWithCarry (toSized x) rot carry
    (SymWord128 x) -> return . SymWord128 $ rotateRightWithCarry x rot carry
    (SymInt8 x) -> return . SymInt8 . fromSized $ rotateRightWithCarry (toSized x) rot carry
    (SymInt16 x) -> return . SymInt16 . fromSized $ rotateRightWithCarry (toSized x) rot carry
    (SymInt32 x) -> return . SymInt32 . fromSized $ rotateRightWithCarry (toSized x) rot carry
    (SymInt64 x) -> return . SymInt64 . fromSized $ rotateRightWithCarry (toSized x) rot carry
    (SymInt128 x) -> return . SymInt128 $ rotateRightWithCarry x rot carry
    _ -> throwError UnexpectedArgType


handleRLC :: Pil.Type -> SymExpr -> SymExpr -> SymExpr
          -> Solver SymExpr
handleRLC et n rot' carry' = do
  bool (throwError $ ArgAndRetNotTheSameType (symType n)) (return ())
    $ sameType n et
  -- all we care about is the lsb, so we can just convert carry to a word8
  carry <- getIntegral carry' :: Solver SWord8
  rot <- getIntegral rot' :: Solver SWord64

  case n of
    (SymWord8 x) -> return . SymWord8 . fromSized $ rotateLeftWithCarry (toSized x) rot carry
    (SymWord16 x) -> return . SymWord16 . fromSized $ rotateLeftWithCarry (toSized x) rot carry
    (SymWord32 x) -> return . SymWord32 . fromSized $ rotateLeftWithCarry (toSized x) rot carry
    (SymWord64 x) -> return . SymWord64 . fromSized $ rotateLeftWithCarry (toSized x) rot carry
    (SymWord128 x) -> return . SymWord128 $ rotateLeftWithCarry x rot carry
    (SymInt8 x) -> return . SymInt8 . fromSized $ rotateLeftWithCarry (toSized x) rot carry
    (SymInt16 x) -> return . SymInt16 . fromSized $ rotateLeftWithCarry (toSized x) rot carry
    (SymInt32 x) -> return . SymInt32 . fromSized $ rotateLeftWithCarry (toSized x) rot carry
    (SymInt64 x) -> return . SymInt64 . fromSized $ rotateLeftWithCarry (toSized x) rot carry
    (SymInt128 x) -> return . SymInt128 $ rotateLeftWithCarry x rot carry
    _ -> throwError UnexpectedArgType


handleVarSplit :: Pil.Type -> PilVar -> PilVar -> Solver SymExpr
handleVarSplit et pvHigh pvLow = do
  vm <- use varMap
  (a, b) <- maybe (throwError CannotFindPilVarInVarMap) return $ do
    (,) <$> HashMap.lookup pvHigh vm <*> HashMap.lookup pvLow vm
  (signedness, twidth) <- maybe (throwError $ UnexpectedReturnType_ "handleVarSplit'1") return
    $ (,) <$> Pil.getSignedness et <*> Pil.getTypeBitWidth et
  case (signedness, twidth) of
    (True, 16) -> case (a, b) of
      (SymInt8 x, SymInt8 y) -> return . SymInt16 . fromSized $ toSized x # toSized y
      _ -> throwError $ UnexpectedArgs (symType a) (symType b)
    (False, 16) -> case (a, b) of
      (SymWord8 x, SymWord8 y) -> return . SymWord16 . fromSized $ toSized x # toSized y
      _ -> throwError $ UnexpectedArgs (symType a) (symType b)

    (True, 32) -> case (a, b) of
      (SymInt16 x, SymInt16 y) -> return . SymInt32 . fromSized $ toSized x # toSized y
      _ -> throwError $ UnexpectedArgs (symType a) (symType b)
    (False, 32) -> case (a, b) of
      (SymWord16 x, SymWord16 y) -> return . SymWord32 . fromSized $ toSized x # toSized y
      _ -> throwError $ UnexpectedArgs (symType a) (symType b)

    (True, 64) -> case (a, b) of
      (SymInt32 x, SymInt32 y) -> return . SymInt64 . fromSized $ toSized x # toSized y
      _ -> throwError $ UnexpectedArgs (symType a) (symType b)
    (False, 64) -> case (a, b) of
      (SymWord32 x, SymWord32 y) -> return . SymWord64 . fromSized $ toSized x # toSized y
      _ -> throwError $ UnexpectedArgs (symType a) (symType b)

    (True, 128) -> case (a, b) of
      (SymInt64 x, SymInt64 y) -> return . SymInt128 $ toSized x # toSized y
      _ -> throwError $ UnexpectedArgs (symType a) (symType b)
    (False, 128) -> case (a, b) of
      (SymWord64 x, SymWord64 y) -> return . SymWord128 $ toSized x # toSized y
      _ -> throwError $ UnexpectedArgs (symType a) (symType b)

    _ -> throwError $ UnexpectedReturnType_ "handleVarSplit'2"


extract' :: Pil.Type -> Int64 -> SymExpr -> Solver SymExpr
extract' et bytePos x = do
  let bitPos = Bits . fromIntegral $ bytePos * 8
  (extractedWidth, extractedSignedness) <- maybe (throwError $ UnexpectedReturnType_ "extract'1") return
    $ (,) <$> Pil.getTypeBitWidth et <*> Pil.getSignedness et
  totalWidth <- getIntegralWidth x
  when (extractedWidth + bitPos > totalWidth) $ throwError ExtractionOutOfBounds
  let x' = D.svExtract (fromIntegral $ bitPos + extractedWidth - 1)
                       (fromIntegral bitPos)
                       (toSVal x)
  case (extractedWidth, extractedSignedness) of
    (8, False) -> return . SymWord8 . SBV $ x'
    (16, False) -> return . SymWord16 . SBV $ x'
    (32, False) -> return . SymWord32 . SBV $ x'
    (64, False) -> return . SymWord64 . SBV $ x'
    (128, False) -> return . SymWord128 . SBV $ x'
    (8, True) -> return . SymInt8 . SBV $ x'
    (16, True) -> return . SymInt16 . SBV $ x'
    (32, True) -> return . SymInt32 . SBV $ x'
    (64, True) -> return . SymInt64 . SBV $ x'
    (128, True) -> return . SymInt128 . SBV $ x'
    _ -> throwError $ UnexpectedReturnType_ "extract'2"


-- --- todo: Just use SBV.Dynamic's svExtract
-- -- it will be faster than having to convert x to a SWord64
-- extract :: Pil.Type -> Int64 -> SymExpr -> Solver SymExpr
-- extract et bytePos x = do
--   let bitPos = fromIntegral $ bytePos * 8
--   w <- maybe (throwError UnexpectedReturnType) return $ Pil.getTypeBitWidth et
--   xwidth <- getIntegralWidth x
--   when (w + bitPos > xwidth) $ throwError ExtractionOutOfBounds
--   -- convert to Word64 to make case statement easier...
--   x' <- toSized <$> (getIntegral x :: Solver SWord64)
--   case w of
--     8 -> case bitPos of
--       0 -> return . SymWord8 . fromSized $ bvExtract (Proxy @7) (Proxy @0) x'
--       8 -> return . SymWord8 . fromSized $ bvExtract (Proxy @15) (Proxy @8) x'
--       16 -> return . SymWord8 . fromSized $ bvExtract (Proxy @23) (Proxy @16) x'
--       24 -> return . SymWord8 . fromSized $ bvExtract (Proxy @31) (Proxy @24) x'
--       32 -> return . SymWord8 . fromSized $ bvExtract (Proxy @39) (Proxy @32) x'
--       40 -> return . SymWord8 . fromSized $ bvExtract (Proxy @47) (Proxy @40) x'
--       48 -> return . SymWord8 . fromSized $ bvExtract (Proxy @55) (Proxy @48) x'
--       56 -> return . SymWord8 . fromSized $ bvExtract (Proxy @63) (Proxy @56) x'
--       _ -> throwError UnexpectedArgType
--     16 -> case bitPos of
--       0 -> return . SymWord16 . fromSized $ bvExtract (Proxy @15) (Proxy @0) x'
--       8 -> return . SymWord16 . fromSized $ bvExtract (Proxy @23) (Proxy @8) x'
--       16 -> return . SymWord16 . fromSized $ bvExtract (Proxy @31) (Proxy @16) x'
--       24 -> return . SymWord16 . fromSized $ bvExtract (Proxy @39) (Proxy @24) x'
--       32 -> return . SymWord16 . fromSized $ bvExtract (Proxy @47) (Proxy @32) x'
--       40 -> return . SymWord16 . fromSized $ bvExtract (Proxy @55) (Proxy @40) x'
--       48 -> return . SymWord16 . fromSized $ bvExtract (Proxy @63) (Proxy @48) x'
--       _ -> throwError UnexpectedArgType
--     32 -> case bitPos of
--       0 -> return . SymWord32 . fromSized $ bvExtract (Proxy @31) (Proxy @0) x'
--       8 -> return . SymWord32 . fromSized $ bvExtract (Proxy @39) (Proxy @8) x'
--       16 -> return . SymWord32 . fromSized $ bvExtract (Proxy @47) (Proxy @16) x'
--       24 -> return . SymWord32 . fromSized $ bvExtract (Proxy @55) (Proxy @24) x'
--       32 -> return . SymWord32 . fromSized $ bvExtract (Proxy @63) (Proxy @32) x'
--       _ -> throwError UnexpectedArgType
--     64 -> case bitPos of
--       0 -> return . SymWord64 . fromSized $ bvExtract (Proxy @63) (Proxy @0) x'
--       _ -> throwError UnexpectedArgType
--     _ -> throwError UnexpectedArgType

