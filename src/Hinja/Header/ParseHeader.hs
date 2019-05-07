{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}

module Hinja.Header.ParseHeader where

import           Hinja.Prelude                         hiding ( try )

import qualified Prelude as P

import           Data.Attoparsec.Text                         ( Parser
                                                              , anyChar
                                                              , endOfInput
                                                              , endOfInput
                                                              , manyTill
                                                              , parseOnly
                                                              , try
                                                              )
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as TextIO
import           Hinja.Header.ParseComment                    ( Comment
                                                              , parseComment
                                                              )
import           Hinja.Header.ParseEnums                      ( EnumType
                                                              , findAll
                                                              , parseEnumType
                                                              , printEnumType
                                                              )
import           Hinja.Header.ParseFunction                   ( Function
                                                              , parseFunction
                                                              , printFunction
                                                              )
import           Hinja.Header.ParseStruct                     ( Struct
                                                              , parseStruct
                                                              , printStruct
                                                              )
import           Hinja.Types.Printer                          ( Printer
                                                              , pr
                                                              )
import qualified Hinja.Types.Printer        as Pr
import           Language.C                                   ( parseCFile )
import           Language.C.Parser                            ( ParseError )
import           Language.C.Syntax.AST                        ( CTranslUnit )
import           Language.C.System.GCC                        ( newGCC )
import           System.IO                                    ( hClose
                                                              , openTempFile
                                                              )
import           Text.RawString.QQ

data StructOrEnum = SStruct Struct
                  | SEnum EnumType
                  deriving (Eq, Ord, Read, Show)

parseStructOrEnum :: Parser StructOrEnum
parseStructOrEnum =(SStruct <$> parseStruct) <|> (SEnum <$> parseEnumType)

data Target a = Target a
              | Other Text
              deriving (Eq, Ord, Read, Show)

parseTarget :: Parser a -> Parser (Target a)
parseTarget p = (Target <$> p) <|>
  (do
      s <- manyTill anyChar (endOfInput <|> void (try p))
      return . Other $ Text.pack s)

maybeThing :: Parser a -> Parser (Maybe a)
maybeThing p = (Just <$> p) <|> return Nothing

parseTargets :: Parser a -> Parser [Target a]
parseTargets p = (endOfInput >> end) <|> (p >>= isTarget) <|> (isOther "")
  where
    end = return []
    isTarget t = ((Target t):) <$> parseTargets p
    isOther txt = do
      c <- anyChar
      let txt' = c:txt
      meof <- maybeThing endOfInput
      case meof of
        Just _ -> return [Other . Text.pack . reverse $ txt']
        Nothing -> do
          mtarget <- maybeThing p
          case mtarget of
            Just target -> ([Other . Text.pack . reverse $ txt' , Target target]++)
                           <$> parseTargets p
            Nothing -> isOther txt'

parseStructAndEnums :: Parser [Target StructOrEnum]
parseStructAndEnums = parseTargets parseStructOrEnum

printStructOrEnum :: StructOrEnum -> Printer ()
printStructOrEnum (SStruct s) = printStruct s
printStructOrEnum (SEnum et) = printEnumType et

printStructOrEnumTarget :: Target StructOrEnum -> Printer ()
printStructOrEnumTarget (Target t) = printStructOrEnum t
printStructOrEnumTarget (Other t) = pr t

beautifyHeader :: Text -> Text
beautifyHeader t = Pr.toText . mapM_ printStructOrEnumTarget $ targets
  where
    targets :: [Target StructOrEnum]
    targets = either (const []) identity . parseOnly parseStructAndEnums $ t

    

data Decl = DeclStruct Struct
          | DeclFunction Function
          | DeclEnum EnumType
          deriving (Eq, Ord, Read, Show)

parseDecl :: Parser Decl
parseDecl = (DeclStruct <$> parseStruct)
  <|> (DeclFunction <$> parseFunction)
  <|> (DeclEnum <$> parseEnumType)

parseAllDecls :: Parser [Decl]
parseAllDecls = findAll parseDecl

parseAllStructs :: Parser [Struct]
parseAllStructs = findAll parseStruct

demoStructs :: Text
demoStructs = [r|
        struct BNLowLevelILInstruction
	{
		BNLowLevelILOperation operation;
		size_t size;
		uint32_t flags;
		uint32_t sourceOperand;
		uint64_t operands[4];
		uint64_t address;
	};

        struct Evangeline;
|]


getDecls :: Text -> Either Text [Decl]
getDecls = first Text.pack . parseOnly parseAllDecls

printDecl :: Decl -> Printer ()
printDecl (DeclStruct s) = printStruct s
printDecl (DeclFunction f) = printFunction f
printDecl (DeclEnum et) = printEnumType et

printAllDecls :: [Decl] -> Printer ()
printAllDecls = mapM_ f
  where
    f d = do
      printDecl d
      pr ""

writeBeautyHeader :: FilePath -> FilePath -> IO ()
writeBeautyHeader fpin fpout = do
  hin <- TextIO.readFile fpin
  case removeComments hin of
    (Left err) -> putText $ "Big Error: " <> err
    (Right t) -> TextIO.writeFile fpout $ beautifyHeader t


kosherHeader :: Text -> Text
kosherHeader = Pr.toText . printAllDecls . either (const []) identity . getDecls

writeKosherHeader :: FilePath -> FilePath -> IO ()
writeKosherHeader fpin fpout = do
  hin <- TextIO.readFile fpin
  case removeComments hin of
    (Left err) -> putText $ "Big Error: " <> err
    (Right t) -> TextIO.writeFile fpout . cleanHeader $ kosherHeader t
 

demo :: IO (Either ParseError CTranslUnit) 
demo = do
  let gcc = newGCC "gcc"
  htext <- cleanHeader <$> TextIO.readFile "/tmp/h/binaryninjacore.h"
  (tempFp, h) <- openTempFile "/tmp" "binaryninjacore.h"
  hClose h
  TextIO.writeFile tempFp htext
  parseCFile gcc Nothing [] tempFp

binjaHeader :: IO Text 
binjaHeader = TextIO.readFile "/tmp/h/binaryninjacore.h"

someHeader :: FilePath -> IO Text 
someHeader fp = TextIO.readFile fp


data RemoveComments = IsComment Comment
                    | IsText Text
                    deriving (Eq, Ord, Read, Show)
                    
parseRemoveComments :: Parser RemoveComments
parseRemoveComments = (IsComment <$> parseComment)
  <|> (IsText . Text.pack <$> manyTill anyChar (void parseComment <|> endOfInput))

removeComments_ :: Text -> Text
removeComments_ txt = either (P.error . Text.unpack) id $ removeComments txt

removeComments :: Text -> Either Text Text
removeComments t = case parseOnly (manyTill parseRemoveComments endOfInput) t of
  Left err -> Left $ Text.pack err
  Right xs -> Right. Text.concat $ fmap f xs where
    f (IsComment _) = ""
    f (IsText nt) = nt

commentOut :: Text -> Text -> Text
commentOut needle =
  Text.replace needle (" // " <> needle)

cleanHeader :: Text -> Text
cleanHeader = commentOut "typedef bool (*BNCorePluginInitFunction)(void);"
  . commentOut "typedef bool (*BNLoadPluginCallback)(const char* repoPath, const char* pluginPath, void* ctx);"
  . commentOut "void BNRegisterObjectDestructionCallbacks(BNObjectDestructionCallbacks* callbacks);"
  . commentOut "void BNUnregisterObjectDestructionCallbacks(BNObjectDestructionCallbacks* callbacks);"
  . commentOut "bool BNExecuteWorkerProcess"
  . commentOut "void BNRegisterLogListener(BNLogListener* listener);"
  . commentOut "void BNUnregisterLogListener(BNLogListener* listener);"
