{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Blaze.Solver2 where

import Blaze.Prelude hiding (zero)
import qualified Prelude as P

import qualified Blaze.Types.Pil as Pil
import Blaze.Types.Pil ( Expression( Expression )
                       , Stmt
                       , PilVar
                       , TypeEnv(TypeEnv)
                       , HasLeft
                       , HasRight
                       )
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Blaze.Types.Solver2
import qualified Blaze.Pil.Analysis as A
import Data.SBV.Tools.Overflow (ArithOverflow, bvAddO)
import qualified Data.SBV.Trans as SBV
import qualified Data.SBV.Trans.Control as SBV
import qualified Data.Text as Text
import qualified Binja.Function as Func
import Data.SBV.Dynamic as D hiding (Solver)
import qualified Blaze.Types.Pil.Checker as Ch
--import Data.SBV.Core.Data (SBV(SBV, unSBV))
import Data.SBV.Internals (SBV(SBV), unSBV)

-- makeSymVar :: PilVar -> DeepSymType -> Solver SVal
-- makeSymVar pv pt = case pt of
--   TArray tlen telemType -> undefined
--   TChar 
--                | TInt { bitWidth :: t, signed :: t }
--                | TFloat { bitWidth :: t }
--                | TBitVector { bitWidth :: t }
--                | TPointer { bitWidth :: t, pointeeType :: t }
--                | TRecord (HashMap BitOffset -- todo: change bitwidth to 't'?
--                            -- TODO: change bitwidth to signed offset
--                                   t -- type
--                          )
--                -- Bottom is labeled with error info
--                | TBottom Sym
--                | TFunction { ret :: t, params :: [t] }
              
--                -- type level values for some dependent-type action
--                | TVBitWidth BitWidth
--                | TVLength Word64
--                | TVSign Bool

--   PTInt w -> SBV.symbolicEnv >>= liftIO . D.svMkSymVar Nothing (D.KBounded True w) (Just . cs $ nm)
--   PTWord w -> SBV.symbolicEnv >>= liftIO . D.svMkSymVar Nothing (D.KBounded False w) (Just . cs $ nm)

constInt :: Bits -> Integer -> SVal
constInt w = svInteger (KBounded True $ fromIntegral w)

constWord :: Bits -> Integer -> SVal
constWord w = svInteger (KBounded False $ fromIntegral w)

constFloat :: Float -> SVal
constFloat = svFloat

-- | requires: targetWidth >= bv
--           : kindOf bv is bounded
zeroExtend :: Bits -> SVal -> SVal
zeroExtend targetWidth bv = case kindOf bv of
  (KBounded s w)
    | tw == w -> bv
    | tw > w -> svJoin ext bv
    | otherwise -> P.error "zeroExtend: target width less than bitvec width"
    where
      ext = svInteger (KBounded s $ fromIntegral targetWidth - w) 0
  _ -> P.error "zeroExtend: arg not bitvec"
  where
    tw = fromIntegral targetWidth

-- | most significant bit
-- requires: kindOf bv is Bounded
--         : width bv > 0
msb :: SVal -> SVal
msb bv = case kindOf bv of
  (KBounded _ w)
    | w == 0 -> P.error "msb: bv has zero width"
    | otherwise -> svTestBit bv (w - 1)
  _ -> P.error "msb: bv must be Bounded kind"

-- | requires: targetWidth >= bv
--           : kindOf bv is bounded
--           : width bv > 0
signExtend :: Bits -> SVal -> SVal
signExtend targetWidth bv = case kindOf bv of
  (KBounded s w)
    | tw == w -> bv
    | tw > w -> svJoin ext bv
    | otherwise -> P.error "signExtend: target width less than bitvec width"
    where
      tw = fromIntegral targetWidth
      zero = svInteger (KBounded s $ fromIntegral targetWidth - w) 0
      ones = svNot zero
      ext  = svIte (msb bv) ones zero
  _ -> P.error "signExtend: bv must be Bounded kind"

-- | Extends b to match a's width.
-- | requires: width a >= width b
--           : kindOf a, b are bounded
--           : widths a, b > 0
matchBoundedWidth :: SVal -> SVal -> SVal
matchBoundedWidth a b = case (kindOf a, kindOf b) of
  (KBounded s w1, KBounded _ w2)
    | w1 == w2 -> b
    | otherwise -> if s then signExtend w1' b else zeroExtend w1' b
    where
      w1' = fromIntegral w1
  _ -> P.error "matchBoundedWidth: a and b must be kind Bounded"

-- | Matches second to first bounded integral sign
-- error if either not bounded.
matchSign :: SVal -> SVal -> SVal
matchSign a b = case (kindOf a, kindOf b) of
  (KBounded s1 _, KBounded s2 _)
    | s1 == s2 -> b
    | otherwise -> if s1 then svSign b else svUnsign b
  _ -> P.error "matchSign: a and b must be kind Bounded"


pilAdd :: SVal -> SVal -> SVal
pilAdd a b = a `svPlus` b'
  where
    b' = matchSign a (matchBoundedWidth a b)

pilSub :: SVal -> SVal -> SVal
pilSub a b = a `svMinus` b'
  where
    b' = matchSign a (matchBoundedWidth a b)

pilCeil :: SVal -> SVal
pilCeil x = case kindOf x of
  KFloat -> unSBV . SBV.fpRoundToIntegral SBV.sRoundTowardPositive $ toSFloat' x
  _ -> P.error "pilCeil: x is not a KFloat"

pilFloor :: SVal -> SVal
pilFloor x = case kindOf x of
  KFloat -> unSBV . SBV.fpRoundToIntegral SBV.sRoundTowardNegative $ toSFloat' x
  _ -> P.error "pilFloor: x is not a KFloat"

pilFAdd :: SVal -> SVal -> SVal
pilFAdd a b = case (kindOf a, kindOf b) of
  (KFloat, KFloat) -> unSBV $ SBV.fpAdd 
                                SBV.sRoundNearestTiesToAway
                                (toSFloat' a)
                                (toSFloat' b)
  _ -> P.error "pilFAdd: one or both arguments are not KFloat"

pilFDiv :: SVal -> SVal -> SVal
pilFDiv a b = case (kindOf a, kindOf b) of
  (KFloat, KFloat) -> unSBV $ SBV.fpDiv 
                                SBV.sRoundNearestTiesToAway
                                (toSFloat' a)
                                (toSFloat' b)
  _ -> P.error "pilFDiv: one or both arguments are not KFloat"

pilRol :: SVal -> SVal -> SVal
pilRol a b = a `svRotateLeft` b

pilLowPart :: SVal -> SVal -> SVal
pilLowPart src sz = case (kindOf src, kindOf sz) of
  (KBounded _ w0, KBounded _ w1) -> src `svAnd` mask
    where
      w1' = fromIntegral w1
      w0' = fromIntegral w0
      mask = svShiftRight ones diff
      diff = srcSize `svMinus` szInBits
      szInBits = sz `svTimes` (constWord (Bits w1') 8)
      srcSize = constWord (Bits w1') (toInteger w0)
      ones = svNot (constWord (Bits w0') 0) 
  _ -> P.error "pilLowPart: one or both arguments are not KBounded"

-- pilStrNCmp :: [SVal] -> [SVal] -> SVal -> SVal
-- pilStrNCmp str0 str1 n = 

-- svSelect
      -- mask (svInteger (KBounded True w1') 0) = constInt 0 0
-- for cmp's flags are set, (nothing?) done on vars

-- binOpFirstArgMatchesReturnType :: (SVal -> SVal -> SVal) -> SVal -> SVal -> Solver
--sRoundTowardPositive
-- -- add :: SVal -> SVal -> Symbolic SVal
toSFloat' :: SVal -> SBV.SFloat
toSFloat' x = case kindOf x of
  KFloat -> SBV x
  _ -> P.error "toSFloat: x is not KFloat kind"

toSBool' :: SVal -> SBool
toSBool' x = case kindOf x of
  KBool -> SBV x
  _ -> P.error "toSBool: x is not KBool kind"

toSBool :: SVal -> Solver SBool
toSBool x = case kindOf x of
  KBool -> return $ SBV x
  _ -> solverError "toSBool: x is not KBool kind"

-- toSFloat :: SVal -> SFloat
-- toSFloat x = case kindOf x of
--   KFloat -> SBV x
--   _ -> solverError "toSFloat: x is not KFloat kind"
--   SBV.sRoundTowardPositive

constrain :: SVal -> Solver ()
constrain x = toSBool x >>= SBV.constrain

-- constrainSVal :: SVal -> Solver

newSymVar :: Text -> Kind -> Solver SVal
newSymVar name k = SBV.symbolicEnv >>= liftIO . D.svMkSymVar Nothing k (Just $ cs name)
 
--   PTInt w -> SBV.symbolicEnv >>= liftIO . D.svMkSymVar Nothing (D.KBounded True w) (Just . cs $ nm)
--   PTWord w -> SBV.symbolicEnv >>= liftIO . D.svMkSymVar Nothing (D.KBounded False w) (Just . cs $ nm)

test :: Solver ()
test = do
  a <- newSymVar "a" (KBounded False 32)
  b <- newSymVar "b" (KBounded False 20)
  c <- newSymVar "c" (KBounded False 32)
  let r = constWord 32 88
  constrain $ c `svEqual` r
  let b' = zeroExtend 32 b
  constrain $ a `svLessThan` b'
  constrain $ svEqual c (a `svPlus` b')


test2 :: SymbolicT (ExceptT () IO) ()
test2 = do
  SBV.constrain . toSBool' $ constInt 32 99 `svEqual` constInt 32 99
  return ()

