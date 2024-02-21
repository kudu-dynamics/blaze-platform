{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}

module Binja.Header.ParseStruct where

-- import qualified Prelude              as P
import           Binja.Header.Prelude                  hiding ( takeWhile )

import Text.RawString.QQ
import Binja.Header.Types.Printer ( Printer, pr, indent )
import           Data.Attoparsec.Text                  ( Parser
                                                       , char
                                                       , manyTill
                                                       , satisfy
                                                       , skipSpace
                                                       , string
                                                       , takeTill
                                                       , takeWhile
                                                       )
import qualified Data.Text            as Text

type StructField = Text

-- really need to grab the field names out...

-- data StructField = StructField
--   { fieldName :: Text
--   , fieldType :: Text
--   }

data Struct = Struct
  { name :: Text
  , fields :: Maybe [StructField]
  } deriving (Eq, Ord, Read, Show)


printStructField :: StructField -> Printer ()
printStructField = pr

printStruct :: Struct -> Printer ()
printStruct s = case fields s of
  Nothing -> pr $ "typedef struct " <> name s <> " " <> name s <> ";"
  Just fs -> do
    pr $ "typedef struct " <> name s
    pr "{"
    indent $ mapM_ printStructField fs
    pr $ "} " <> name s <> ";"
       

validCName :: Parser Text
validCName = do
  start <- satisfy isAlpha
  rest <- takeWhile (\c -> isAlphaNum c || c == '_')
  return $ Text.cons start rest

parseStruct :: Parser Struct
parseStruct = do
  void $ string "struct "
  n <- validCName
  skipSpace
  mf <- optional parseFields
  void $ char ';'
  return $ Struct { name = n
                  , fields = mf }


parseFields :: Parser [StructField]
parseFields = do
  void $ char '{'
  manyTill parseField (skipSpace >> void (char '}'))

parseField :: Parser StructField
parseField = do
  skipSpace
  takeTill (== '\n') <* char '\n'
  
--  takeTill \c -> c == ';' || c == '}'

demo0 :: Text
demo0 = "struct BNTransformParameter;"

demo1 :: Text
demo1 = "struct BNTransformParameter\n {\n const char* name;\n BNDataBuffer* value;\n };"


demo2 :: Text
demo2 = [r|struct BNObjectDestructionCallbacks
	{
		void* context;
		void (*destructBinaryView)(void* ctxt, BNBinaryView* view);
		void (*destructFileMetadata)(void* ctxt, BNFileMetadata* file);
		void (*destructFunction)(void* ctxt, BNFunction* func);
	};|]

demo3 :: Text
demo3 = [r|struct BNInteractionHandlerCallbacks
	{
		void* context;
		void (*showPlainTextReport)(void* ctxt, BNBinaryView* view, const char* title, const char* contents);
		void (*showMarkdownReport)(void* ctxt, BNBinaryView* view, const char* title, const char* contents,
			const char* plaintext);
		void (*showHTMLReport)(void* ctxt, BNBinaryView* view, const char* title, const char* contents,
			const char* plaintext);
		void (*showGraphReport)(void* ctxt, BNBinaryView* view, const char* title, BNFlowGraph* graph);
		void (*showReportCollection)(void* ctxt, const char* title, BNReportCollection* reports);
		bool (*getTextLineInput)(void* ctxt, char** result, const char* prompt, const char* title);
		bool (*getIntegerInput)(void* ctxt, int64_t* result, const char* prompt, const char* title);
		bool (*getAddressInput)(void* ctxt, uint64_t* result, const char* prompt, const char* title,
			BNBinaryView* view, uint64_t currentAddr);
		bool (*getChoiceInput)(void* ctxt, size_t* result, const char* prompt, const char* title,
			const char** choices, size_t count);
		bool (*getOpenFileNameInput)(void* ctxt, char** result, const char* prompt, const char* ext);
		bool (*getSaveFileNameInput)(void* ctxt, char** result, const char* prompt, const char* ext,
			const char* defaultName);
		bool (*getDirectoryNameInput)(void* ctxt, char** result, const char* prompt, const char* defaultName);
		bool (*getFormInput)(void* ctxt, BNFormInputField* fields, size_t count, const char* title);
		BNMessageBoxButtonResult (*showMessageBox)(void* ctxt, const char* title, const char* text,
			BNMessageBoxButtonSet buttons, BNMessageBoxIcon icon);
	};|]
