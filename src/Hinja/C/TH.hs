{-# LANGUAGE TemplateHaskell #-}

module Hinja.C.TH where

import Hinja.Prelude

import Foreign.ForeignPtr ( ForeignPtr
                          , FinalizerPtr
                          )
import Hinja.C.Types
import Language.Haskell.TH

derivePointer :: Name -> Q [Dec]
derivePointer name  = do
  d1 <- [d| deriving instance Show $x |]
  d2 <- [d| deriving instance Eq $x |]
  return $ d1 <> d2
  where
   x = conT name

-- dd :: Name -> Q [Dec]
-- dd pname = do
--   x <- newName "x"
--   return [InstanceD Nothing [] (AppT (ConT ''Pointer) (ConT pname))
--           [ ValD (VarP 'pointerWrap) (NormalB (ConE pname)) []
--           , FunD 'pointerUnwrap [Clause [ConP pname [VarP x]] (NormalB (VarE x)) []]
--           , ValD (VarP 'pointerFinalizer) (NormalB (ConE 'Nothing)) []]]

-- dd' :: Q [Dec]
-- dd' = [d| instance Pointer Bill where
--            pointerWrap = Bill 
--            pointerUnwrap (Bill x) = x
--            pointerFinalizer = Just billdog
--        |]

mkFinalizerName :: String -> Name
mkFinalizerName finalizerString = mkName $ "fin" <> finalizerString

mkPointerInstance :: Name -> Maybe String -> Q Dec
mkPointerInstance pointerName mFinalizerString = do
  x <- newName "x"
  return $ InstanceD Nothing [] (AppT (ConT ''Pointer) (ConT pointerName))
    [ ValD (VarP 'pointerWrap) (NormalB (ConE pointerName)) []
    , FunD 'pointerUnwrap [Clause [ConP pointerName [VarP x]] (NormalB (VarE x)) []]
    , ValD (VarP 'pointerFinalizer) (NormalB fin) []]
  where
    mFinalizerName = mkFinalizerName <$> mFinalizerString
--    fin = undefined
    fin = case mFinalizerName of
      Nothing -> ConE 'Nothing
      Just fname -> AppE (ConE 'Just) (VarE fname)

-- returns (declaration, finalizerName)
mkPointerFinalizer :: Name -> String -> Dec
mkPointerFinalizer pointerName finalizerString =
  ForeignD $ ImportF CCall Unsafe
    ("Bindings.chs.h  &" ++ finalizerString)
    finalizerName
    (AppT (ConT ''FinalizerPtr) (ConT pointerName))
  where
   finalizerName = mkFinalizerName finalizerString

mkPointer' :: String -> Maybe String -> Q [Dec]
mkPointer' pointerString mFinalizerString = do
  pinstance <- mkPointerInstance pointerName mFinalizerString
  return $ catMaybes [ Just ntDec
                     , mkPointerFinalizer pointerName <$> mFinalizerString
                     , Just pinstance
                     ]
  where
    ntDec = NewtypeD [] pointerName [] Nothing
      (NormalC pointerName [(Bang NoSourceUnpackedness NoSourceStrictness
                     , AppT (ConT ''ForeignPtr) (ConT pointerName))])
      [ ConT ''Eq
      , ConT ''Ord
      , ConT ''Show]
    pointerName = mkName pointerString

mkPointer :: String -> String -> Q [Dec]
mkPointer p f = mkPointer' p (Just f)

mkPointer_ :: String -> Q [Dec]
mkPointer_ p = mkPointer' p Nothing
