{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Hinja.Types.Printer where

import Hinja.Prelude
import qualified Data.Text as Text

indentSize :: Int
indentSize = 4

data PrinterState = PrinterState
  { indentLevel :: Int
  , printedLines :: [Text] -- a Seq would be much more efficient...
  } deriving (Eq, Ord, Read, Show)

newtype Printer a = Printer (State PrinterState a)
  deriving (Functor, Applicative, Monad, MonadState PrinterState)

toText :: Printer a -> Text
toText (Printer m) = let s = execState m (PrinterState 0 []) in
  Text.intercalate "\n" $ printedLines s

pr :: Text -> Printer ()
pr = printLn

printLn :: Text -> Printer ()
printLn t = do
  i <- indentLevel <$> get
  let indentText = Text.replicate (indentSize * i) " "
  modify $ \s -> s { printedLines = printedLines s <> [indentText <> t] }
  return ()

commaList :: [Text] -> Printer ()
commaList [] = return ()
commaList [t] = pr t
commaList (t:ts) = pr (t <> ",") >> commaList ts

appendTo :: Printer a -> Text -> Printer a
appendTo p t = do
  r <- p
  s <- get
  let lines = printedLines s
  case lastMay lines of
    Nothing -> pr t >> return r
    Just x -> do
      put $ s { printedLines = take (length lines - 1) lines <> [x <> t] }
      return r

indent :: Printer a -> Printer a
indent p = do
  i <- indentLevel <$> get
  modify $ \s -> s { indentLevel = i + 1 }
  r <- p
  modify $ \s -> s { indentLevel = i }
  return r

