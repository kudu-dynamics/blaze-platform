{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Hinja.C.TH where

import Hinja.Prelude

import Foreign.ForeignPtr ( ForeignPtr
                          , FinalizerPtr
                          ) 
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
-- import qualified GHC.Types

mkAdder :: Int -> Q Exp
mkAdder n = [| \x -> x + n |]

derivePointer :: Name -> Q [Dec]
derivePointer name  = do
  d1 <- [d| deriving instance Show $x |]
  d2 <- [d| deriving instance Eq $x |]
  return $ d1 <> d2
  where
   x = conT name

dd :: Q [Dec]
dd = return [ForeignD $ ImportF CCall Unsafe
             "Bindings.chs.h &BNFreeFileMetadata"
             (mkName "bNFreeFileMetadata")
            (AppT (ConT ''FinalizerPtr) (ConT ''Int))
            ]

-- dd :: Q [Dec]
-- dd = [d| foo :: FinalizerPtr Int
--          foo = undefined
--        |]

mkPointer :: String -> Q [Dec]
mkPointer x = do
  return [ntDec]
  --[d| newtype Jones = Jones (ForeignPtr Jones) deriving (Eq, Show) |]
  where
    intType = mkName "Int"
    ntDec = NewtypeD [] name [] Nothing
      (NormalC name [(Bang NoSourceUnpackedness NoSourceStrictness
                     , AppT (ConT ''ForeignPtr) (ConT name))])
      [ ConT ''Eq
      , ConT ''Show]
    name = mkName x

