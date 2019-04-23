{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Hinja where

import Hinja.Prelude hiding (onException)
import qualified Prelude as P
import Prelude (String)
import qualified Data.Text as Text
import Foreign hiding (void)
import Foreign.C.Types
import Foreign.ForeignPtr
import qualified Control.Exception as E
import qualified Hinja.C.Main as BN
import Hinja.C.Main (BNBinaryView)
import System.Envy
import GHC.Generics

a1 :: FilePath
a1 = "/tmp/kudu/assembly/a1"

dive :: FilePath
dive = "/tmp/kudu/blaze/binja-clojure/resources/test_bins/Dive_Logger/Dive_Logger.bndb"

