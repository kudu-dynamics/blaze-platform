{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blaze.Types.Persist.Db.Address where

import Blaze.Prelude hiding ((:*:))

import qualified Prelude as P
import Database.Selda
import Database.Selda.SqlType ( Lit(LText, LCustom)
                              , SqlTypeRep(TBlob, TText)
                              , SqlValue(SqlString)
                              )


-- oh no, it's an orphan!
instance SqlType Address where
   mkLit (Address (Bytes x)) = LCustom TBlob . LText . show $ x

   sqlType _ = TText

   fromSql (SqlString s) = case readMaybe s of
     Nothing -> P.error $ "Cannot convert " <> cs s <> " to Address"
     Just n -> Address . Bytes $ n
   fromSql x = P.error $ "Unexpected sql field type: " <> show x

   defaultValue = LCustom TText (LText "")
