{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
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
import Data.SBV.List as SList
import qualified Data.Text as Text
import qualified Binja.Function as Func
import Data.SBV (SWord, SInt, fromSized, toSized, FromSized, ToSized)
import GHC.TypeNats
import Unsafe.Coerce (unsafeCoerce)

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


handleLowPart :: Pil.Type -> SymExpr -> Solver SymExpr
handleLowPart et x = case Pil.getTypeWidth et of
  (Just 8) -> case x of
    -- no change because 8 bytes returns the whole thing
    (SymWord64 v) -> return . SymWord64 $ v
    (SymInt64 v) -> return . SymInt64 $ v
    _ -> throwError UnexpectedArgType

  (Just 4) -> case x of
    (SymWord32 v) -> return . SymWord32 $ v
    (SymInt32 v) -> return . SymInt32 $ v

    (SymWord64 v) -> return . SymWord32 . fromSized $ SBV.bvDrop (Proxy @ 32) (toSized v)
    (SymInt64 v) -> return . SymInt32 . fromSized $ SBV.bvDrop (Proxy @ 32) (toSized v)
    _ -> throwError UnexpectedArgType

  (Just 2) -> case x of
    (SymWord16 v) -> return . SymWord16 $ v
    (SymInt16 v) -> return . SymInt16 $ v
    
    (SymWord32 v) -> return . SymWord16 . fromSized $ SBV.bvDrop (Proxy @ 16) (toSized v)
    (SymInt32 v) -> return . SymInt16 . fromSized $ SBV.bvDrop (Proxy @ 16) (toSized v)

    (SymWord64 v) -> return . SymWord16 . fromSized $ SBV.bvDrop (Proxy @ 48) (toSized v)
    (SymInt64 v) -> return . SymInt16 . fromSized $ SBV.bvDrop (Proxy @ 48) (toSized v)
    _ -> throwError UnexpectedArgType

  (Just 1) -> case x of
    (SymWord8 v) -> return . SymWord8 $ v
    (SymInt8 v) -> return . SymInt8 $ v

    (SymWord16 v) -> return . SymWord8 . fromSized $ SBV.bvDrop (Proxy @ 8) (toSized v)
    (SymInt16 v) -> return . SymInt8 . fromSized $ SBV.bvDrop (Proxy @ 8) (toSized v)
    
    (SymWord32 v) -> return . SymWord8 . fromSized $ SBV.bvDrop (Proxy @ 24) (toSized v)
    (SymInt32 v) -> return . SymInt8 . fromSized $ SBV.bvDrop (Proxy @ 24) (toSized v)

    (SymWord64 v) -> return . SymWord8 . fromSized $ SBV.bvDrop (Proxy @ 56) (toSized v)
    (SymInt64 v) -> return . SymInt8 . fromSized $ SBV.bvDrop (Proxy @ 56) (toSized v)
    _ -> throwError UnexpectedArgType  

  (Just n) -> throwError $ UnrecognizedTypeWidth n
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

--     nWithCarry = toSized n # carryBit

-- data RotType = WordRot SWord64
--              | IntRot SInt64

-- rotateRightWithCarry :: Pil.Type -> SymExpr -> SymExpr -> SymExpr
--                      -> Solver SymExpr
-- rotateRightWithCarry et n rot carry = do
--   bool (throwError $ ArgAndRetNotTheSameType (symType n)) (return ())
--     $ sameType n et
--   -- all we care about is the lsb, so we can just convert carry to a word8
--   carryBool <- lsb <$> (getIntegral carry :: Solver SWord8)
--   let carryWord1 :: SWord 1
--       carryWord1 = fromBitsLE [carryBool]

--       carryInt1 :: SInt 1
--       carryInt1 = fromBitsLE [carryBool]
--   rotType <- case rot of
--     (SymWord8 x) -> return . WordRot $ sFromIntegral x
--     (SymWord16 x) -> return . WordRot $ sFromIntegral x
--     (SymWord32 x) -> return . WordRot $ sFromIntegral x
--     (SymWord64 x) -> return . WordRot $ x
--     (SymInt8 x) -> return . IntRot $ sFromIntegral x
--     (SymInt16 x) -> return . IntRot $ sFromIntegral x
--     (SymInt32 x) -> return . IntRot $ sFromIntegral x
--     (SymInt64 x) -> return . IntRot $ x
--     _ -> throwError UnexpectedArgType

--   -- todo -- reduce boilerplate
--   case n of
--     (SymWord8 x) -> case rotType of
--       (WordRot r) -> return . SymWord8 . fromSized
--         $ bvTake (Proxy @8) $ sRotateRight (toSized x # carryWord1) r
      -- (IntRot r) -> return . SymWord8 . fromSized
      --   $ bvTake (Proxy @8) $ sRotateRight (toSized x # carryWord1) r
--     (SymWord16 x) -> case rotType of
--       (WordRot r) -> return . SymWord16 . fromSized
--         $ bvTake (Proxy @16) $ sRotateRight (toSized x # carryWord1) r
--       (IntRot r) -> return . SymWord16 . fromSized
--         $ bvTake (Proxy @16) $ sRotateRight (toSized x # carryWord1) r
--     -- (SymWord32 x) -> return . WordRot $ sFromIntegral x
--     -- (SymWord64 x) -> return . WordRot $ sFromIntegral x
--     -- (SymInt8 x) -> return . IntRot $ sFromIntegral x
--     -- (SymInt16 x) -> return . IntRot $ sFromIntegral x
--     -- (SymInt32 x) -> return . IntRot $ sFromIntegral x
--     -- (SymInt64 x) -> return . IntRot $ sFromIntegral x
    
--   undefined
-- --   -- where
-- --   --   carryBit :: SWord 1
-- --   --   carryBit = fromBitsLE [lsb carry]

-- --   --   nWithCarry = toSized n # carryBit

