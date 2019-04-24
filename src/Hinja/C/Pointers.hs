{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Hinja.C.Pointers where

-- import Hinja.Prelude

import Hinja.C.TH

-- ddp :: Q [Dec]
-- ddp = mkPointer "BNBinaryView" "BNFreeBinaryView"

$(mkPointer "BNBinaryView" "BNFreeBinaryView")
$(mkPointer_ "BNBinaryViewType")
$(mkPointer "BNFileMetadata" "BNFreeFileMetadata")
$(mkPointer "BNFunction" "BNFreeFunction")
