module Blaze.Pil.Inference where

import Blaze.Prelude hiding (Type)
import qualified Prelude as P
import Blaze.Types.Pil

data UnificationError = UWidth
                      | USign
                      | UPtrSigned
                      deriving (Eq, Ord, Show)

-- most specific unification
-- goes from general to specific

unify :: Type -> Type -> Either UnificationError Type
unify TBool TBool = Right TBool

unify t@(TBitVec b1) (TBitVec b2)
  | b1 ^. width /= b2 ^. width = Left UWidth
  | otherwise = Right t

unify t@(TInt n1) (TInt n2)
  | n1 ^. width /= n2 ^. width = Left UWidth
  | n1 ^. signed /= n2 ^. signed = Left USign
  | otherwise = Right t
unify b@(TBitVec _) n@(TInt _) = unify n b
unify t@(TInt n) (TBitVec b)
  | b ^. width /= n ^. width = Left UWidth
  | otherwise = Right t

unify (TPtr p1) (TPtr p2)
  | p1 ^. width /= p2 ^. width = Left UWidth
  | otherwise = unify (p1 ^. pointeeType) (p2 ^. pointeeType)
unify b@(TBitVec _) p@(TPtr _) = unify p b
unify t@(TPtr p) (TBitVec b)
  | p ^. width /= b ^. width = Left UWidth
  | otherwise = Right t
unify b@(TInt _) p@(TPtr _) = unify p b
unify t@(TPtr p) (TInt n)
  | n ^. signed = Left UPtrSigned
  | p ^. width /= n ^. width = Left UWidth
  | otherwise = Right t

unify _ _ = P.error "type not implemented in unify"
