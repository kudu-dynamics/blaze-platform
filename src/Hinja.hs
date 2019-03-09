{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Hinja where

import Hinja.Prelude hiding (onException)
import qualified Prelude as P
import Data.Text as Text
import Foreign
import Foreign.C.Types
import qualified Control.Exception as E
import qualified HinjaC as HC

a1 :: Text
a1 = "/tmp/kudu/assembly/a1"

main :: IO ()
main = return ()

foreign import ccall "math.h sin"
     c_sin :: CDouble -> CDouble

fastsin :: Double -> Double
fastsin x = realToFrac (c_sin (realToFrac x))
