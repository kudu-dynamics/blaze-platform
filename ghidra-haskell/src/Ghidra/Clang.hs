{-# LANGUAGE DerivingStrategies #-}

module Ghidra.Clang
  ( module Ghidra.Clang
  ) where

import Ghidra.Prelude hiding (replace)
import Data.List (span)
import Data.Text (replace)
import qualified Data.Text as T
import qualified Language.Java as Java
import qualified Ghidra.Types as J
import Ghidra.Types.Internal (Ghidra, runIO)
import Foreign.JNI.Types (JClass)
import Ghidra.Pcode (getBarePcodeOp)
import Ghidra.Types.Pcode (BarePcodeOp)
import Ghidra.Types.Variable (VarNode(..), HighVariable(..), HighSymbol(..))
import Ghidra.Variable (mkVarNode, mkHighVariable, getHighSymbol)
import Ghidra.Types.Address (Address)
import Ghidra.Address (mkAddress)
import Language.Java (J)
import Ghidra.Util (isJNull)
import Data.Type.Equality
import Ghidra.GhidraDataTypes (parseDataTypeWithTransaction)
import Ghidra.Types.GhidraDataTypes (GhidraDataType)

getClassName :: J.ClangNode -> Ghidra Text
getClassName node = do
  nodeClass :: JClass <- runIO $ Java.call node "getClass"
  className :: Text <- runIO $ Java.call nodeClass "getName" >>= Java.reify
  return className

getChildren :: J.ClangNode -> Ghidra [J.ClangNode]
getChildren node = do
  numChildren :: Int32 <- runIO $ Java.call node "numChildren"
  buildChildrenList node 0 numChildren

buildChildrenList :: J.ClangNode -> Int32 -> Int32 -> Ghidra [J.ClangNode]
buildChildrenList node currChild maxChild
  | currChild == maxChild = return []
  | otherwise = do
      child :: J.ClangNode <- runIO $ Java.call node "Child" currChild
      fmap (child :) (buildChildrenList node (currChild + 1) maxChild)

getAddressRange :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra (Maybe AddrRange)
getAddressRange node = do
  maxAddrPointer :: J.Address <- runIO $ Java.call node "getMaxAddress"
  minAddrPointer :: J.Address <- runIO $ Java.call node "getMinAddress"
  if isJNull maxAddrPointer || isJNull minAddrPointer then
    return Nothing
  else do
    maxAddr <- mkAddress maxAddrPointer
    minAddr <- mkAddress minAddrPointer
    return $ Just AddrRange { maxAddr = maxAddr, minAddr = minAddr }

getTokenText :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra Text
getTokenText node = runIO $ Java.call node "getText" >>= Java.reify

getOpToken :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra (OpToken, Text)
getOpToken node = do
  tokStr <- getTokenText node
  let tok = matchToken tokStr
  return (tok, tokStr)

matchToken :: Text -> OpToken
matchToken t
  | t == "if"       = If
  | t == "else"     = Else
  | t == "=="       = Equal
  | t == "!="       = Neq
  | t == "+"        = Plus
  | t == "-"        = Minus
  | t == "*"        = Mult
  | t == "/"        = Div
  | t == "%"        = Mod
  | t == "<"        = Lt
  | t == ">"        = Gt
  | t == "<="       = Le
  | t == ">="       = Ge
  | t == "&&"       = And
  | t == "||"       = Or
  | t == "!"        = Not
  | t == "&"        = BitAnd
  | t == "|"        = BitOr
  | t == "^"        = BitXor
  | t == "~"        = BitNot
  | t == "<<"       = Shl
  | t == ">>"       = Shr
  | t == "++"       = Increment
  | t == "--"       = Decrement
  | t == "switch"   = Switch
  | t == "return"   = Return
  | t == "while"    = While
  | t == "do"       = Do
  | t == "case"     = Case
  | t == "default"  = Default'
  | t == "for"      = For
  | t == "goto"     = Goto
  | t == "break"    = Break'
  | t == "continue" = Continue'
  | t == "="        = Assignment
  | t == "+="       = AddAssign
  | t == "-="       = SubAssign
  | t == "*="       = MulAssign
  | t == "/="       = DivAssign
  | t == "%="       = ModAssign
  | t == "&="       = AndAssign
  | t == "|="       = OrAssign
  | t == "^="       = XorAssign
  | t == "<<="      = ShlAssign
  | t == ">>="      = ShrAssign
  | otherwise       = Unimplemented

data OpToken
  = If
  | Else
  | Equal
  | Neq
  | Plus
  | Minus
  | Mult
  | Div
  | Mod
  | Lt
  | Gt
  | Le
  | Ge
  | And
  | Or
  | Not
  | BitAnd
  | BitOr
  | BitXor
  | BitNot
  | Shl
  | Shr
  | Increment
  | Decrement
  | Switch
  | Return
  | While
  | Do
  | Case
  | Default'
  | For
  | Goto
  | Break'
  | Continue'
  | Assignment
  | AddAssign
  | SubAssign
  | MulAssign
  | DivAssign
  | ModAssign
  | AndAssign
  | OrAssign
  | XorAssign
  | ShlAssign
  | ShrAssign
  | Unimplemented
  deriving (Eq, Ord, Show, Generic, Hashable)
  


-- gets the number of indent levels following this line break
getIndent :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra Int32
getIndent node = runIO $ Java.call node "getIndent"

-- gets the byte offset of this field with its structure
getOffset :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra Int32
getOffset node = runIO $ Java.call node "getOffset"

-- true if this token represents a variable (in source code)
getIsVarRef :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra Bool
getIsVarRef node = do
  runIO $ Java.call node "isVariableRef"

getDataType :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => J.ProgramDB -> a -> Ghidra (Maybe GhidraDataType)
getDataType prog node = do
  dt :: J.DataType <- runIO $ Java.call node "getDataType"
  if isJNull dt then
    return Nothing
  else do
    dt' <- parseDataTypeWithTransaction prog dt
    return $ Just dt' 

getVarNode :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra (Maybe VarNode)
getVarNode node = do
  varNode :: J.VarNode <- runIO $ Java.call node "getVarnode"
  if isJNull varNode then
    return Nothing
  else Just <$> mkVarNode varNode

{- if a syntax token is some pair of tokens (ex: '(' and ')' or '{' and '}'), then the token has a pair id that matches
   it with its pair; this function just gets the id of a pair token; it the token is not part of a pair, then it does not
   have a pair id and then we return Nothing-}
getPairId :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra (Maybe Int32)
getPairId node = do
  openId <- runIO $ Java.call node "getOpen"
  closeId <- runIO $ Java.call node "getClose"
  return $
    -- if both openId and closeId return -1, then they are not a pair token
    if openId == -1 && closeId == -1 then Nothing
    {- a pair token can only either be an opening token (ex: '(') or a closing token (ex: ')'), so we short-circuit if the token
       if an opening token -}
    else if openId /= -1 then Just openId
    else Just closeId

getPcodeOp :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra (Maybe BarePcodeOp)
getPcodeOp node = do
  pcodeOp :: J.PcodeOp <- runIO $ Java.call node "getPcodeOp"
  if isJNull pcodeOp then return Nothing 
  else do
    pcode :: BarePcodeOp <- getBarePcodeOp pcodeOp
    return $ Just pcode

-- gets the BRANCHIND PcodeOp that jumps to this label
getSwitchOp :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra (Maybe BarePcodeOp)
getSwitchOp node = do
  pcodeOp :: J.PcodeOp <- runIO $ Java.call node "getSwitchOp"
  if isJNull pcodeOp then return Nothing 
  else do
    switchOp :: BarePcodeOp <- getBarePcodeOp pcodeOp
    return $ Just switchOp

-- if the token represents an underlying integer constant, return the constant as a Scalar. Otherwise return null.
getScalar :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra (Maybe Scalar)
getScalar node = do
  scalar :: J.Scalar <- runIO $ Java.call node "getScalar"
  if isJNull scalar then return Nothing
  else do
    value :: Int64 <- runIO $ Java.call scalar "getSignedValue"
    isSigned' :: Bool <- runIO $ Java.call scalar "isSigned"
    return $ Just Scalar { value = value, isSigned = isSigned' }

getHighVariable :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => J.ProgramDB -> a -> Ghidra (Maybe HighVariable)
getHighVariable prog node = do
  highVar :: J.HighVariable <- runIO $ Java.call node "getHighVariable"
  if isJNull highVar then return Nothing
  else Just <$> mkHighVariable prog highVar

getHighSymbol' :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra (Maybe HighSymbol)
getHighSymbol' node = do
  highVar :: J.HighVariable <- runIO $ Java.call node "getHighVariable"
  if isJNull highVar then return Nothing
  else getHighSymbol highVar


buildClangAST :: J.ProgramDB -> J.ClangNode -> Ghidra (ClangAST ClangNode)
buildClangAST prog node = do
  children <- getChildren node
  childrenAST <- mapM (buildClangAST prog) children
  node' <- constructClangNode prog node
  return $
    if null childrenAST then Leaf node'
    else Branch node' childrenAST


constructClangNode :: J.ProgramDB -> J.ClangNode -> Ghidra ClangNode
constructClangNode prog node = do
  className <- replace "ghidra.app.decompiler." "" <$> getClassName node
  addrRange <- getAddressRange node
  case className of
    "ClangBreak" -> do
      let node' :: J.ClangBreak = coerce node
      indent <- getIndent node'
      return $ ClangBreak ClangBreakOpts { indent = indent }
    "ClangCaseToken"      ->  do
      let node' :: J.ClangCaseToken = coerce node
      txt           <- getTokenText node'
      highSymbol    <- fromJust <$> getHighSymbol'  node'
      highVariable  <- fromJust <$> getHighVariable prog node'
      pcodeOp       <- fromJust <$> getPcodeOp      node'
      scalar        <- fromJust <$> getScalar       node'
      switchOp      <- fromJust <$> getSwitchOp     node'
      varnode       <- fromJust <$> getVarNode      node'
      isVarRef      <- getIsVarRef node'
      return $ ClangCaseToken ClangCaseTokenOpts { text = txt, addrRange = fromJust addrRange, highSymbol = highSymbol, highVariable = highVariable
                                                 , pcodeOp = pcodeOp, scalar = scalar, switchOp = switchOp, varnode = varnode, isVarRef = isVarRef }
    "ClangCommentToken"   -> do
      let node' :: J.ClangCommentToken = coerce node
      txt <- getTokenText node'
      isVarRef <- getIsVarRef node'
      return $ ClangCommentToken ClangCommentTokenOpts { text = txt, addrRange = addrRange, isVarRef = isVarRef }
    "ClangFieldToken"     -> do
      let node' :: J.ClangFieldToken = coerce node
      txt <- getTokenText node'
      datatype <- fromJust <$> getDataType prog node'
      offset <- getOffset node'
      pcodeOp <- fromJust <$> getPcodeOp node'
      return $ ClangFieldToken ClangFieldTokenOpts { text = txt, addrRange = addrRange, datatype = datatype, offset = offset, pcodeOp = pcodeOp }
    "ClangFuncNameToken"  ->  do
      let node' :: J.ClangFuncNameToken = coerce node
      txt <- getTokenText node'
      pcodeOp <- getPcodeOp node'
      return $ ClangFuncNameToken ClangFuncNameTokenOpts { text = txt, addrRange = addrRange, pcodeOp = pcodeOp }
    "ClangFuncProto"      ->  return $ ClangFuncProto ClangFuncProtoOpts
    "ClangFunction"       ->  return $ ClangFunction  ClangFunctionOpts       { addrRange = fromJust addrRange }
    "ClangLabelToken"     -> do
      let node' :: J.ClangLabelToken = coerce node
      txt <- getTokenText node'
      highSymbol    <- getHighSymbol'  node'
      highVariable  <- getHighVariable prog node'
      pcodeOp       <- getPcodeOp      node'
      scalar        <- getScalar       node'
      varnode       <- getVarNode      node'
      isVarRef      <- getIsVarRef node'
      return $ ClangLabelToken ClangLabelTokenOpts { text = txt, addrRange = addrRange, highSymbol = highSymbol, highVariable = highVariable
                                                   , pcodeOp = pcodeOp, scalar = scalar, varnode = varnode, isVarRef = isVarRef }
    "ClangOpToken"        ->  do
      let node' :: J.ClangOpToken = coerce node
      pcodeOp       <- getPcodeOp      node'
      (tok, txt)    <- getOpToken node'
      return $ ClangOpToken ClangOpTokenOpts { text = txt, addrRange = addrRange, pcodeOp = pcodeOp, opToken = tok }
    "ClangReturnType"     ->  do
      let node' :: J.ClangReturnType = coerce node
      dt <- getDataType prog node'
      varnode <- getVarNode node'
      return $ ClangReturnType ClangReturnTypeOpts { datatype = dt, varnode = varnode }
    "ClangStatement"      ->  do
      let node' :: J.ClangStatement = coerce node
      barePCodeOp <- getPcodeOp node'
      return $ ClangStatement ClangStatementOpts { addrRange = addrRange, pcodeOp = barePCodeOp }
    "ClangSyntaxToken"    -> do
      let node' :: J.ClangSyntaxToken = coerce node
      txt <- getTokenText node'
      isVarRef <- getIsVarRef node'
      pairId <- getPairId node'
      return $ ClangSyntaxToken ClangSyntaxTokenOpts { text = txt, isVarRef = isVarRef, pairId = pairId }
    "ClangToken"          ->  do
      txt <- getTokenText node
      return $ ClangToken ClangTokenOpts { text = txt, addrRange = addrRange }
    "ClangTokenGroup"     ->  return $ ClangTokenGroup ClangTokenGroupOpts { addrRange = addrRange }
    "ClangTypeToken"      -> do
      let node' :: J.ClangTypeToken = coerce node
      txt <- getTokenText node'
      isVarRef <- getIsVarRef node'
      datatype <- fromJust <$> getDataType prog node'
      return $ ClangTypeToken ClangTypeTokenOpts { text = txt, datatype = datatype, isVarRef = isVarRef }
    "ClangVariableDecl"   -> do
      let node' :: J.ClangVariableDecl = coerce node
      datatype <- fromJust <$> getDataType prog node'
      highSymbol <- getHighSymbol' node'
      highVariable <- getHighVariable prog node'
      return $ ClangVariableDecl ClangVariableDeclOpts { datatype = datatype, highSymbol = highSymbol, highVariable = highVariable }
    "ClangVariableToken"  -> do
      let node' :: J.ClangVariableToken = coerce node
      txt <- getTokenText node'
      highSymbol <- getHighSymbol' node'
      highVariable <- getHighVariable prog node'
      pcodeOp <- getPcodeOp node'
      scalar <- getScalar node'
      varnode <- getVarNode node'
      isVarRef <- getIsVarRef node'
      return $ ClangVariableToken ClangVariableTokenOpts { text = txt, addrRange = addrRange, highSymbol = highSymbol, highVariable = highVariable
                                                         , pcodeOp = pcodeOp, scalar = scalar, varnode = varnode, isVarRef = isVarRef }

    _ -> do
      txt <- runIO $ Java.call node "toString" >>= Java.reify @Text
      return $ ClangSyntaxToken ClangSyntaxTokenOpts
        { text = txt, isVarRef = False, pairId = Nothing }


data AddrRange = AddrRange 
  { maxAddr :: Address
  , minAddr :: Address 
  } deriving (Eq, Ord, Show, Generic, Hashable)

-- value is always in the signed form, isSigned specifies whether or not the original value was signed or not
data Scalar = Scalar
  { value    :: Int64
  , isSigned :: Bool
  } deriving (Eq, Ord, Show, Generic, Hashable)

{- The values in each of these have been tested (unless stated otherwise), however it is not comprehensive since Ghidra does not provide 
   documentation as to which values are guaranteed. So the values that are not wrapped in Maybe so far have always been present, but 
   this could potentially change -}

newtype ClangBreakOpts = ClangBreakOpts 
  { indent    :: Int32
  } deriving (Eq, Ord, Show, Generic)
    deriving anyclass Hashable

data ClangCaseTokenOpts = ClangCaseTokenOpts
  { text          :: Text
  , addrRange     :: AddrRange
  , highSymbol    :: HighSymbol
  , highVariable  :: HighVariable
  , pcodeOp       :: BarePcodeOp
  , scalar        :: Scalar
  , switchOp      :: BarePcodeOp
  , varnode       :: VarNode
  , isVarRef      :: Bool
  } deriving (Eq, Ord, Show, Generic, Hashable)

-- not tested; not sure how this would be present in a binary
data ClangCommentTokenOpts = ClangCommentTokenOpts
  { text      :: Text
  , addrRange :: Maybe AddrRange
  , isVarRef  :: Bool
  } deriving (Eq, Ord, Show, Generic, Hashable)

-- not tested (add more information from ClangToken when testing)
data ClangFieldTokenOpts = ClangFieldTokenOpts
  { text      :: Text
  , addrRange :: Maybe AddrRange
  , datatype  :: GhidraDataType
  , offset    :: Int32
  , pcodeOp   :: BarePcodeOp
  } deriving (Eq, Ord, Show, Generic, Hashable)

data ClangFuncNameTokenOpts = ClangFuncNameTokenOpts
  { text      :: Text
  , addrRange :: Maybe AddrRange
  , pcodeOp   :: Maybe BarePcodeOp -- it seems like the only pcode is Call
  } deriving (Eq, Ord, Show, Generic, Hashable)

data ClangFuncProtoOpts = ClangFuncProtoOpts
  deriving (Eq, Ord, Show, Generic, Hashable)

newtype ClangFunctionOpts = ClangFunctionOpts
  { addrRange :: AddrRange 
  } deriving (Eq, Ord, Show, Generic)
    deriving anyclass Hashable

-- not tested
data ClangLabelTokenOpts = ClangLabelTokenOpts
  { text          :: Text
  , addrRange     :: Maybe AddrRange
  , highSymbol    :: Maybe HighSymbol
  , highVariable  :: Maybe HighVariable
  , pcodeOp       :: Maybe BarePcodeOp
  , scalar        :: Maybe Scalar
  , varnode       :: Maybe VarNode
  , isVarRef      :: Bool
  } deriving (Eq, Ord, Show, Generic, Hashable)

data ClangOpTokenOpts = ClangOpTokenOpts
  { text      :: Text
  , addrRange :: Maybe AddrRange
  , pcodeOp   :: Maybe BarePcodeOp
  , opToken   :: OpToken
  } deriving (Eq, Ord, Show, Generic, Hashable)

data ClangReturnTypeOpts = ClangReturnTypeOpts
  { datatype  :: Maybe GhidraDataType
  , varnode   :: Maybe VarNode
  } deriving (Eq, Ord, Show, Generic, Hashable)

data ClangStatementOpts = ClangStatementOpts
  { addrRange :: Maybe AddrRange
  , pcodeOp   :: Maybe BarePcodeOp
  } deriving (Eq, Ord, Show, Generic, Hashable)

-- haven't tested the ClangToken aspect, but if it's like ClangOpTokenOpts, then it won't have a lot of information
data ClangSyntaxTokenOpts = ClangSyntaxTokenOpts
  { text      :: Text
  , isVarRef  :: Bool
  , pairId    :: Maybe Int32
  } deriving (Eq, Ord, Show, Generic, Hashable)

data ClangTokenOpts = ClangTokenOpts
  { text      :: Text
  , addrRange :: Maybe AddrRange
  } deriving (Eq, Ord, Show, Generic, Hashable)

newtype ClangTokenGroupOpts = ClangTokenGroupOpts
  { addrRange :: Maybe AddrRange 
  } deriving (Eq, Ord, Show, Generic)
    deriving anyclass Hashable

-- haven't tested the ClangToken aspect, but if it's like ClangOpTokenOpts, then it won't have a lot of information
data ClangTypeTokenOpts = ClangTypeTokenOpts
  { text     :: Text
  , datatype :: GhidraDataType
  , isVarRef :: Bool
  } deriving (Eq, Ord, Show, Generic, Hashable)

data ClangVariableDeclOpts = ClangVariableDeclOpts
  { datatype      :: GhidraDataType
  , highSymbol    :: Maybe HighSymbol
  , highVariable  :: Maybe HighVariable
  } deriving (Eq, Ord, Show, Generic, Hashable)

data ClangVariableTokenOpts = ClangVariableTokenOpts
  { text          :: Text
  , addrRange     :: Maybe AddrRange
  , highSymbol    :: Maybe HighSymbol
  , highVariable  :: Maybe HighVariable
  , pcodeOp       :: Maybe BarePcodeOp
  , scalar        :: Maybe Scalar
  , varnode       :: Maybe VarNode
  , isVarRef      :: Bool
  } deriving (Eq, Ord, Show, Generic, Hashable)

data ClangAST a = Leaf a | Branch a [ClangAST a]
  deriving (Eq, Ord, Show, Generic, Hashable, Functor, Foldable, Traversable)

data ClangNode 
  = ClangBreak          ClangBreakOpts 
  | ClangCaseToken      ClangCaseTokenOpts
  | ClangCommentToken   ClangCommentTokenOpts 
  | ClangFieldToken     ClangFieldTokenOpts 
  | ClangFuncNameToken  ClangFuncNameTokenOpts 
  | ClangFuncProto      ClangFuncProtoOpts 
  | ClangFunction       ClangFunctionOpts 
  | ClangLabelToken     ClangLabelTokenOpts 
  | ClangOpToken        ClangOpTokenOpts 
  | ClangReturnType     ClangReturnTypeOpts 
  | ClangStatement      ClangStatementOpts 
  | ClangSyntaxToken    ClangSyntaxTokenOpts 
  | ClangToken          ClangTokenOpts 
  | ClangTokenGroup     ClangTokenGroupOpts 
  | ClangTypeToken      ClangTypeTokenOpts 
  | ClangVariableDecl   ClangVariableDeclOpts 
  | ClangVariableToken  ClangVariableTokenOpts
    deriving (Eq, Ord, Show, Generic, Hashable)

-- | Standard C AST types, transformed from ClangAST ClangNode.
-- Catch-all variants (CRawStmt, CRawExpr) preserve unparsed Clang subtrees.

-- | Address range annotations linking C AST nodes back to binary addresses.
-- From an address you can look up PIL statements, pcode ops, or disassembly.
-- A list is used so compound expressions preserve the exact set of ranges
-- from their children without merging.
type CAnn = [AddrRange]

data CStmt
  = CExprStmt CAnn CExpr
  | CIf CAnn CExpr [CStmt]
  | CIfElse CAnn CExpr [CStmt] [CStmt]
  | CSwitch CAnn CExpr [CCase]
  | CWhile CAnn CExpr [CStmt]
  | CDoWhile CAnn [CStmt] CExpr
  | CFor CAnn CForInit (Maybe CExpr) (Maybe CExpr) [CStmt]  -- ann, init, cond, incr, body
  | CContinue CAnn
  | CBreak CAnn
  | CReturn CAnn (Maybe CExpr)
  | CVarDecl CAnn Text Text (Maybe CExpr)  -- ann, type, name, init
  | CComment CAnn Text
  | CBlock CAnn [CStmt]
  | CRawStmt CAnn [ClangAST ClangNode]  -- catch-all for unparsed statements
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | For-loop init clause: either a declaration or an expression (or empty)
data CForInit
  = CForInitExpr (Maybe CExpr)            -- for(expr; ...) or for(; ...)
  | CForInitDecl Text Text (Maybe CExpr)  -- for(type name = expr; ...)
  deriving (Eq, Ord, Show, Generic, Hashable)

data CCase
  = CCase CExpr [CStmt]
  | CDefault [CStmt]
  deriving (Eq, Ord, Show, Generic, Hashable)

data CExpr
  = CIdent CAnn Text
  | CLitInt CAnn Integer
  | CLitString CAnn Text
  | CBinaryOp CAnn Text CExpr CExpr
  | CUnaryOp CAnn Text CExpr
  | CPostfixOp CAnn Text CExpr
  | CFuncall CAnn Text [CExpr]
  | CAssign CAnn Text CExpr CExpr  -- ann, op, lhs, rhs
  | CIndex CAnn CExpr CExpr
  | CDot CAnn CExpr Text
  | CArrow CAnn CExpr Text
  | CCast CAnn Text CExpr
  | CCond CAnn CExpr CExpr CExpr
  | CRawExpr CAnn [ClangAST ClangNode]  -- catch-all for unparsed expressions
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | Get the annotation from a CExpr
exprAnn :: CExpr -> CAnn
exprAnn = \case
  CIdent a _         -> a
  CLitInt a _        -> a
  CLitString a _     -> a
  CBinaryOp a _ _ _  -> a
  CUnaryOp a _ _     -> a
  CPostfixOp a _ _   -> a
  CFuncall a _ _     -> a
  CAssign a _ _ _    -> a
  CIndex a _ _       -> a
  CDot a _ _         -> a
  CArrow a _ _       -> a
  CCast a _ _        -> a
  CCond a _ _ _      -> a
  CRawExpr a _       -> a

-- | Get the annotation from a CStmt
stmtAnn :: CStmt -> CAnn
stmtAnn = \case
  CExprStmt a _       -> a
  CIf a _ _           -> a
  CIfElse a _ _ _     -> a
  CSwitch a _ _       -> a
  CWhile a _ _        -> a
  CDoWhile a _ _      -> a
  CFor a _ _ _ _      -> a
  CContinue a         -> a
  CBreak a            -> a
  CReturn a _         -> a
  CVarDecl a _ _ _    -> a
  CComment a _        -> a
  CBlock a _          -> a
  CRawStmt a _        -> a

-- | Extract address range annotation from a ClangAST node
nodeAnn :: ClangAST ClangNode -> CAnn
nodeAnn (Leaf n) = nodeAddrRange n
nodeAnn (Branch n _) = nodeAddrRange n

-- | Extract address range from a ClangNode
nodeAddrRange :: ClangNode -> CAnn
nodeAddrRange = \case
  ClangBreak _              -> []
  ClangCaseToken opts       -> [opts ^. #addrRange]
  ClangCommentToken opts    -> maybeToList (opts ^. #addrRange)
  ClangFieldToken opts      -> maybeToList (opts ^. #addrRange)
  ClangFuncNameToken opts   -> maybeToList (opts ^. #addrRange)
  ClangFuncProto _          -> []
  ClangFunction opts        -> [opts ^. #addrRange]
  ClangLabelToken opts      -> maybeToList (opts ^. #addrRange)
  ClangOpToken opts         -> maybeToList (opts ^. #addrRange)
  ClangReturnType _         -> []
  ClangStatement opts       -> maybeToList (opts ^. #addrRange)
  ClangSyntaxToken _        -> []
  ClangToken opts           -> maybeToList (opts ^. #addrRange)
  ClangTokenGroup opts      -> maybeToList (opts ^. #addrRange)
  ClangTypeToken _          -> []
  ClangVariableDecl _       -> []
  ClangVariableToken opts   -> maybeToList (opts ^. #addrRange)

-- | Collect annotations from a list of ClangAST nodes
nodesAnn :: [ClangAST ClangNode] -> CAnn
nodesAnn = concatMap nodeAnn

-- | Extract text content from a ClangAST node (concatenates all leaf texts)
clangText :: ClangAST ClangNode -> Text
clangText (Leaf n) = nodeText n
clangText (Branch n children) =
  nodeText n <> mconcat (fmap clangText children)

-- | Extract text from a single ClangNode
nodeText :: ClangNode -> Text
nodeText = \case
  ClangBreak _              -> ""
  ClangCaseToken opts       -> opts ^. #text
  ClangCommentToken opts    -> opts ^. #text
  ClangFieldToken opts      -> opts ^. #text
  ClangFuncNameToken opts   -> opts ^. #text
  ClangFuncProto _          -> ""
  ClangFunction _           -> ""
  ClangLabelToken opts      -> opts ^. #text
  ClangOpToken opts         -> opts ^. #text
  ClangReturnType _         -> ""
  ClangStatement _          -> ""
  ClangSyntaxToken opts     -> opts ^. #text
  ClangToken opts           -> opts ^. #text
  ClangTokenGroup _         -> ""
  ClangTypeToken opts       -> opts ^. #text
  ClangVariableDecl _       -> ""
  ClangVariableToken opts   -> opts ^. #text

-- | Get the OpToken from a ClangAST node, if it's an OpToken
getOp :: ClangAST ClangNode -> Maybe OpToken
getOp (Leaf (ClangOpToken opts)) = Just (opts ^. #opToken)
getOp _ = Nothing

-- | Check if a ClangAST node is a syntax token with given text
isSyntaxText :: Text -> ClangAST ClangNode -> Bool
isSyntaxText t (Leaf (ClangSyntaxToken opts)) = opts ^. #text == t
isSyntaxText _ _ = False

-- | Check if a ClangAST node has the given text (any leaf token type)
isTokenText :: Text -> ClangAST ClangNode -> Bool
isTokenText t node = case node of
  Leaf n -> nodeText n == t
  _ -> False

-- | Check if a node is a ClangBreak
isBreak :: ClangAST ClangNode -> Bool
isBreak (Leaf (ClangBreak _)) = True
isBreak _ = False

-- | Check if a node is whitespace-like (break or space syntax)
isWhitespace :: ClangAST ClangNode -> Bool
isWhitespace (Leaf (ClangBreak _)) = True
isWhitespace (Leaf (ClangSyntaxToken opts)) = opts ^. #text == " " || opts ^. #text == ""
isWhitespace _ = False

-- | Check if node is a specific OpToken
isOpToken :: OpToken -> ClangAST ClangNode -> Bool
isOpToken tok (Leaf (ClangOpToken opts)) = opts ^. #opToken == tok
isOpToken _ _ = False

-- | Check if node is a ClangTokenGroup (Branch)
isTokenGroup :: ClangAST ClangNode -> Bool
isTokenGroup (Branch (ClangTokenGroup _) _) = True
isTokenGroup _ = False

-- | Get children of a Branch node, or empty for Leaf
astChildren :: ClangAST ClangNode -> [ClangAST ClangNode]
astChildren (Branch _ cs) = cs
astChildren (Leaf _) = []

-- | Find the first non-whitespace child's OpToken in a list
firstOpToken :: [ClangAST ClangNode] -> Maybe OpToken
firstOpToken = listToMaybe . mapMaybe getOp . filter (not . isWhitespace)

-- | Convert a whole ClangFunction AST to a list of C statements.
-- Expects the root to be a Branch ClangFunction with children:
--   [Branch ClangFuncProto [...], Branch ClangTokenGroup [...body...]]
convertFunction :: ClangAST ClangNode -> [CStmt]
convertFunction (Branch (ClangFunction _) children) =
  case filter isTokenGroup children of
    (Branch _ bodyChildren : _) -> convertBlock bodyChildren
    _ -> [CRawStmt (nodesAnn children) children]
convertFunction ast = [CRawStmt (nodeAnn ast) [ast]]

-- | Convert a list of ClangAST nodes (children of a TokenGroup) into C statements.
-- This is the main workhorse that identifies control flow structures.
convertBlock :: [ClangAST ClangNode] -> [CStmt]
convertBlock [] = []
convertBlock nodes = go (filter (not . isBreak) nodes)
  where
    go [] = []
    go (n : rest) = case n of
      -- A TokenGroup: check what kind of control structure it is
      Branch (ClangTokenGroup _) children ->
        case firstOpToken children of
          Just For       -> parseForLoop children : go rest
          Just While     -> parseWhileLoop children : go rest
          Just Do        -> parseDoWhile children : go rest
          Just If        -> parseIfChain children rest
          Just Switch    -> parseSwitchStmt children : go rest
          Just Return    -> let inner = convertBlock children in inner <> go rest
          _ -> case convertBlock children of
            []    -> go rest  -- skip empty blocks
            -- If block is only comments, inline them (don't wrap in { })
            inner | all isCommentStmt inner -> inner <> go rest
            inner -> CBlock (nodesAnn children) inner : go rest

      -- A Statement: expression statement
      Branch (ClangStatement _) children ->
        convertStatement children : go rest

      -- A VariableDecl
      Branch (ClangVariableDecl _) children ->
        parseVarDecl children : go rest

      -- Standalone if at block level (common in Ghidra's while(true) bodies)
      Leaf (ClangOpToken opts) | opts ^. #opToken == If ->
        let (ifNodes, remaining) = consumeIfBlock rest
        in parseIfFlat ifNodes remaining

      -- Standalone else at block level (follows an if)
      _ | isTokenText "else" n ->
        -- else without a preceding if in our parse — consume its body
        let (elseBody, remaining) = consumeBodyBlock rest
        in CBlock (nodesAnn elseBody) (convertBlock elseBody) : go remaining

      -- Standalone OpTokens at block level
      Leaf (ClangOpToken opts) | opts ^. #opToken == Return ->
        let (retNodes, remaining) = consumeUntilSemicolon rest
        in parseReturn retNodes : go remaining
      Leaf (ClangOpToken opts) | opts ^. #opToken == Break' ->
        CBreak (nodeAnn n) : go (dropSemicolon rest)
      Leaf (ClangOpToken opts) | opts ^. #opToken == Continue' ->
        CContinue (nodeAnn n) : go (dropSemicolon rest)

      -- Comment tokens — merge adjacent comments into one CComment
      Leaf (ClangCommentToken _) ->
        let (commentNodes, remaining) = span isCommentOrSyntax (n : rest)
            ann = nodesAnn commentNodes
            rawText = T.strip . mconcat $ fmap getCommentText commentNodes
            commentText = T.strip
              . stripSuffix' "*/"
              . T.strip
              . stripPrefix' "/*"
              . T.strip
              $ rawText
        in (if T.null commentText then identity else (CComment ann commentText :)) $ go remaining

      -- Syntax tokens and empty leaf token groups - skip
      Leaf (ClangSyntaxToken opts) | opts ^. #text `elem` ["{", "}", ";", " ", ""] ->
        go rest
      Leaf (ClangTokenGroup _) -> go rest

      -- Catch-all
      _ -> CRawStmt (nodeAnn n) [n] : go rest

-- | Convert a ClangStatement's children to a CStmt
convertStatement :: [ClangAST ClangNode] -> CStmt
convertStatement children =
  let ann = nodesAnn children
      clean = cleanExpr children
  in case clean of
    -- return expr
    (Leaf (ClangOpToken opts) : rest) | opts ^. #opToken == Return ->
      parseReturn rest
    -- break
    (Leaf (ClangOpToken opts) : _) | opts ^. #opToken == Break' ->
      CBreak ann
    -- continue
    (Leaf (ClangOpToken opts) : _) | opts ^. #opToken == Continue' ->
      CContinue ann
    _ -> CExprStmt ann (convertExpr children)

-- | Parse a for loop from the children of its TokenGroup.
-- Expected structure:
--   OpToken(For), "(", init..., ";", cond..., ";", incr..., ")", body...
parseForLoop :: [ClangAST ClangNode] -> CStmt
parseForLoop children =
  let ann = nodesAnn children
      noWS = filter (not . isWhitespace) children
      afterFor = drop 1 $ dropWhile (not . isOpToken For) noWS
      afterOpen = drop 1 $ dropWhile (not . isSyntaxText "(") afterFor
      (headerTokens, bodyTokens) = splitAtCloseParen afterOpen
      parts = splitBySemicolon headerTokens
      (initPart, condPart, incrPart) = case parts of
        [a, b, c] -> (a, b, c)
        [a, b]    -> (a, b, [])
        [a]       -> (a, [], [])
        _         -> ([], [], [])
      forInit = parseForInit initPart
      cond = if null condPart then Nothing else Just (convertExpr condPart)
      incr = if null incrPart then Nothing else Just (convertExpr incrPart)
      body = extractBody bodyTokens
  in CFor ann forInit cond incr body

-- | Parse the init clause of a for loop
parseForInit :: [ClangAST ClangNode] -> CForInit
parseForInit [] = CForInitExpr Nothing
parseForInit nodes =
  case findVarDecl nodes of
    Just (typeName, varName, initExpr) -> CForInitDecl typeName varName initExpr
    Nothing -> CForInitExpr (Just (convertExpr nodes))

-- | Try to find a variable declaration pattern in nodes:
-- TypeToken followed by VariableToken, possibly with "=" and init expression
findVarDecl :: [ClangAST ClangNode] -> Maybe (Text, Text, Maybe CExpr)
findVarDecl nodes =
  let clean = filter (not . isWhitespace) nodes
  in case clean of
    (Leaf (ClangTypeToken tOpts) : Leaf (ClangVariableToken vOpts) : rest) ->
      let typeName = tOpts ^. #text
          varName = vOpts ^. #text
          initExpr = case dropWhile (not . isAssign) rest of
            (_ : exprNodes) | not (null exprNodes) -> Just (convertExpr exprNodes)
            _ -> Nothing
      in Just (typeName, varName, initExpr)
    -- Handle VariableDecl Branch that contains type + variable
    (Branch (ClangVariableDecl _) declChildren : rest) ->
      case findTypeAndVar declChildren of
        Just (typeName, varName) ->
          let initExpr = case dropWhile (not . isAssign) rest of
                (_ : exprNodes) | not (null exprNodes) -> Just (convertExpr exprNodes)
                _ -> Nothing
          in Just (typeName, varName, initExpr)
        Nothing -> Nothing
    _ -> Nothing

-- | Find type and variable name in a VariableDecl's children
findTypeAndVar :: [ClangAST ClangNode] -> Maybe (Text, Text)
findTypeAndVar nodes =
  let types = [t | Leaf (ClangTypeToken opts) <- nodes, let t = opts ^. #text]
      vars = [v | Leaf (ClangVariableToken opts) <- nodes, let v = opts ^. #text]
  in case (types, vars) of
    (t : _, v : _) -> Just (t, v)
    _ -> Nothing

-- | Check if a node is an assignment operator
isAssign :: ClangAST ClangNode -> Bool
isAssign (Leaf (ClangOpToken opts)) = opts ^. #opToken == Assignment
isAssign _ = False

-- | Parse a while loop: While, "(", cond..., ")", body...
parseWhileLoop :: [ClangAST ClangNode] -> CStmt
parseWhileLoop children =
  let ann = nodesAnn children
      noWS = filter (not . isWhitespace) children
      afterWhile = drop 1 $ dropWhile (not . isOpToken While) noWS
      afterOpen = drop 1 $ dropWhile (not . isSyntaxText "(") afterWhile
      (condTokens, bodyTokens) = splitAtCloseParen afterOpen
      cond = convertExpr condTokens
      body = extractBody bodyTokens
  in CWhile ann cond body

-- | Parse a do-while loop: Do, body..., While, "(", cond..., ")"
parseDoWhile :: [ClangAST ClangNode] -> CStmt
parseDoWhile children =
  let noWS = filter (not . isWhitespace) children
      afterDo = drop 1 $ dropWhile (not . isOpToken Do) noWS
      (bodyTokens, whilePart) = span (not . isOpToken While) afterDo
      body = extractBody bodyTokens
      afterWhile = drop 1 whilePart
      afterOpen = drop 1 $ dropWhile (not . isSyntaxText "(") afterWhile
      (condTokens, _) = splitAtCloseParen afterOpen
      cond = convertExpr condTokens
  in CDoWhile (nodesAnn children) body cond

-- | Parse an if/else chain. Handles if, if-else, and if-elseif-else.
-- The 'rest' parameter is the remaining siblings after this TokenGroup,
-- needed to consume an else branch that may follow.
parseIfChain :: [ClangAST ClangNode] -> [ClangAST ClangNode] -> [CStmt]
parseIfChain children rest =
  let ann = nodesAnn children
      noWS = filter (not . isWhitespace) children
      afterIf = drop 1 $ dropWhile (not . isOpToken If) noWS
      afterOpen = drop 1 $ dropWhile (not . isSyntaxText "(") afterIf
      (condTokens, bodyTokens) = splitAtCloseParen afterOpen
      cond = convertExpr condTokens
      thenBody = extractBody bodyTokens
  in case rest of
    (Branch (ClangTokenGroup _) elseChildren : rest')
      | any (isTokenText "else") elseChildren ->
          let afterElse = drop 1 $ dropWhile (not . isTokenText "else") (filter (not . isWhitespace) elseChildren)
          in case filter (not . isWhitespace) afterElse of
            (n' : _) | isTokenText "if" n' ->
              let elseStmts = [parseIfOnly afterElse]
              in CIfElse ann cond thenBody elseStmts : convertBlock rest'
            _ ->
              let elseBody = extractBody afterElse
              in CIfElse ann cond thenBody elseBody : convertBlock rest'
    _ -> CIf ann cond thenBody : convertBlock rest

-- | Parse a standalone if (within an else-if chain)
parseIfOnly :: [ClangAST ClangNode] -> CStmt
parseIfOnly nodes =
  let ann = nodesAnn nodes
      noWS = filter (not . isWhitespace) nodes
      afterIf = drop 1 $ dropWhile (not . isOpToken If) noWS
      afterOpen = drop 1 $ dropWhile (not . isSyntaxText "(") afterIf
      (condTokens, bodyTokens) = splitAtCloseParen afterOpen
      cond = convertExpr condTokens
      body = extractBody bodyTokens
  in CIf ann cond body

-- | Parse a switch statement
parseSwitchStmt :: [ClangAST ClangNode] -> CStmt
parseSwitchStmt children =
  let noWS = filter (not . isWhitespace) children
      afterSwitch = drop 1 $ dropWhile (not . isOpToken Switch) noWS
      afterOpen = drop 1 $ dropWhile (not . isSyntaxText "(") afterSwitch
      (condTokens, bodyTokens) = splitAtCloseParen afterOpen
      cond = convertExpr condTokens
      cases = parseCases bodyTokens
  in CSwitch (nodesAnn children) cond cases

-- | Parse case/default labels from switch body
parseCases :: [ClangAST ClangNode] -> [CCase]
parseCases [] = []
parseCases nodes =
  let clean = filter (not . isWhitespace) nodes
      -- Remove outer { }
      inner = case clean of
        (n : ns) | isSyntaxText "{" n ->
          case reverse ns of
            (l : ls) | isSyntaxText "}" l -> reverse ls
            _ -> ns
        _ -> clean
  in parseCaseList inner

parseCaseList :: [ClangAST ClangNode] -> [CCase]
parseCaseList [] = []
parseCaseList nodes = case break isCaseOrDefault nodes of
  (_, []) -> []
  (_, caseNode : afterLabel) ->
    let -- Find the colon after case label
        (labelParts, afterColon) = break (isSyntaxText ":") afterLabel
        bodyAndRest = drop 1 afterColon  -- drop the ":"
        -- Take until next case/default
        (bodyNodes, remaining) = break isCaseOrDefault bodyAndRest
        bodyStmts = convertBlock bodyNodes
    in case getOp caseNode of
      Just Default' -> CDefault bodyStmts : parseCaseList remaining
      Just Case ->
        let caseExpr = convertExpr labelParts
        in CCase caseExpr bodyStmts : parseCaseList remaining
      _ -> parseCaseList remaining

isCaseOrDefault :: ClangAST ClangNode -> Bool
isCaseOrDefault n = isOpToken Case n || isOpToken Default' n

-- | Parse a variable declaration
parseVarDecl :: [ClangAST ClangNode] -> CStmt
parseVarDecl children =
  let ann = nodesAnn children
  in case findTypeAndVar children of
    Just (typeName, varName) -> CVarDecl ann typeName varName Nothing
    Nothing -> CRawStmt ann children

-- | Parse a standalone if at block level (not wrapped in a TokenGroup).
-- Consumes: "(" cond ")" body, and optionally "else" body from siblings.
parseIfFlat :: [ClangAST ClangNode] -> [ClangAST ClangNode] -> [CStmt]
parseIfFlat ifNodes remaining =
  let ann = nodesAnn ifNodes
      noWS = filter (not . isWhitespace) ifNodes
      afterOpen = drop 1 $ dropWhile (not . isSyntaxText "(") noWS
      (condTokens, afterCond) = splitAtCloseParen afterOpen
      cond = convertExpr condTokens
      thenBody = extractBody afterCond
  in case remaining of
    (n' : elseRest) | isTokenText "else" n' ->
      case filter (not . isWhitespace) elseRest of
        (n'' : ifRest) | isTokenText "if" n'' ->
          let (innerIfNodes, remaining') = consumeIfBlock ifRest
              elseStmts = parseIfFlat innerIfNodes remaining'
          in [CIfElse ann cond thenBody elseStmts]
        _ -> let (elseBodyNodes, remaining') = consumeBodyBlock elseRest
                 elseBody = convertBlock elseBodyNodes
             in CIfElse ann cond thenBody elseBody : convertBlock remaining'
    _ -> CIf ann cond thenBody : convertBlock remaining

-- | Consume tokens for an if block: everything from "(" to matching ")" then the body
consumeIfBlock :: [ClangAST ClangNode] -> ([ClangAST ClangNode], [ClangAST ClangNode])
consumeIfBlock nodes =
  let (condAndBody, remaining) = consumeCondAndBody nodes
  in (condAndBody, remaining)

-- | Consume a parenthesized condition and then a body block (braced or single statement)
consumeCondAndBody :: [ClangAST ClangNode] -> ([ClangAST ClangNode], [ClangAST ClangNode])
consumeCondAndBody nodes =
  -- Find everything up through the closing ")" of the condition
  let (beforeParen, fromParen) = break (isSyntaxText "(") nodes
  in case fromParen of
    (openP : afterOpen) ->
      let (condInner, afterClose) = splitAtCloseParen afterOpen
          condPart = beforeParen <> [openP] <> condInner <> [Leaf (ClangSyntaxToken ClangSyntaxTokenOpts { text = ")", isVarRef = False, pairId = Nothing })]
          (bodyPart, remaining) = consumeBodyBlock afterClose
      in (condPart <> bodyPart, remaining)
    _ -> (nodes, [])

-- | Consume a body block: either a { ... } braced block or a single TokenGroup/Statement
consumeBodyBlock :: [ClangAST ClangNode] -> ([ClangAST ClangNode], [ClangAST ClangNode])
consumeBodyBlock nodes =
  let clean = dropWhile isWhitespace nodes
  in case clean of
    -- Braced block: consume { ... }
    (n : rest) | isSyntaxText "{" n ->
      let (body, afterClose) = consumeUntilCloseBrace rest
      in (body, afterClose)
    -- TokenGroup as body
    (n@(Branch (ClangTokenGroup _) _) : rest) -> ([n], rest)
    -- Statement as body
    (n@(Branch (ClangStatement _) _) : rest) -> ([n], rest)
    -- Single statement up to semicolon
    _ -> let (body, remaining) = consumeUntilSemicolon clean
         in (body, remaining)

-- | Consume tokens until a matching close brace, tracking nesting
consumeUntilCloseBrace :: [ClangAST ClangNode] -> ([ClangAST ClangNode], [ClangAST ClangNode])
consumeUntilCloseBrace = go (0 :: Int) []
  where
    go _ acc [] = (reverse acc, [])
    go depth acc (n : rest)
      | isSyntaxText "{" n = go (depth + 1) (n : acc) rest
      | isSyntaxText "}" n =
          if depth == 0 then (reverse acc, rest)
          else go (depth - 1) (n : acc) rest
      | otherwise = go depth (n : acc) rest

-- | Parse a return statement from tokens after "return"
parseReturn :: [ClangAST ClangNode] -> CStmt
parseReturn [] = CReturn [] Nothing
parseReturn nodes =
  let ann = nodesAnn nodes
      clean = filter (not . isWhitespace) nodes
  in if null clean then CReturn ann Nothing
     else CReturn ann (Just (convertExpr clean))

-- | Check if a CStmt is a comment
isCommentStmt :: CStmt -> Bool
isCommentStmt (CComment _ _) = True
isCommentStmt _ = False

-- | Check if a node is a comment token
isComment :: ClangAST ClangNode -> Bool
isComment (Leaf (ClangCommentToken _)) = True
isComment _ = False

-- | Check if a node is a comment or syntax token (for merging comment runs)
isCommentOrSyntax :: ClangAST ClangNode -> Bool
isCommentOrSyntax (Leaf (ClangCommentToken _)) = True
isCommentOrSyntax (Leaf (ClangSyntaxToken _)) = True
isCommentOrSyntax (Leaf (ClangBreak _)) = True
isCommentOrSyntax _ = False

-- | Extract text from a comment or syntax token for merging
getCommentText :: ClangAST ClangNode -> Text
getCommentText (Leaf (ClangCommentToken opts)) = opts ^. #text
getCommentText (Leaf (ClangSyntaxToken opts)) = opts ^. #text
getCommentText _ = ""

-- | Strip a prefix if present, otherwise return unchanged
stripPrefix' :: Text -> Text -> Text
stripPrefix' pfx t = fromMaybe t (T.stripPrefix pfx t)

-- | Strip a suffix if present, otherwise return unchanged
stripSuffix' :: Text -> Text -> Text
stripSuffix' sfx t = fromMaybe t (T.stripSuffix sfx t)

-- | Clean expression tokens: remove whitespace and comments
cleanExpr :: [ClangAST ClangNode] -> [ClangAST ClangNode]
cleanExpr = filter (\n -> not (isWhitespace n) && not (isComment n))


-- | Convert ClangAST nodes into a CExpr.
-- Uses structural pattern matching with RawExpr as fallback.
convertExpr :: [ClangAST ClangNode] -> CExpr
convertExpr [] = CRawExpr [] []
convertExpr nodes =
  let ann = nodesAnn nodes
      clean = cleanExpr nodes
  in case clean of
    [] -> CRawExpr ann []

    -- Single variable
    [Leaf (ClangVariableToken opts)] -> CIdent (nodeAnn (Leaf (ClangVariableToken opts))) (opts ^. #text)

    -- Single token (might be a literal or identifier)
    [Leaf (ClangToken opts)] -> CIdent (nodeAnn (Leaf (ClangToken opts))) (opts ^. #text)

    -- Single func name
    [Leaf (ClangFuncNameToken opts)] -> CIdent (nodeAnn (Leaf (ClangFuncNameToken opts))) (opts ^. #text)

    -- Single type token (used as cast or type reference)
    [Leaf (ClangTypeToken opts)] -> CIdent [] (opts ^. #text)

    -- Single field token
    [Leaf (ClangFieldToken opts)] -> CIdent (nodeAnn (Leaf (ClangFieldToken opts))) (opts ^. #text)

    -- Single syntax token as value (Ghidra uses SyntaxToken for some numeric constants)
    [Leaf (ClangSyntaxToken opts)] | not (isSyntaxPunctuation (opts ^. #text)) ->
      CIdent (nodeAnn (Leaf (ClangSyntaxToken opts))) (opts ^. #text)

    -- Single statement: recurse into its children
    [Branch (ClangStatement _) children] -> convertExpr children

    -- Single token group: recurse
    [Branch (ClangTokenGroup _) children] -> convertExpr children

    -- Single variable decl: recurse
    [Branch (ClangVariableDecl _) children] -> convertExpr children

    -- Parenthesized expression: ( expr ) possibly followed by more
    (p : rest) | isSyntaxText "(" p ->
      let (inner, after) = splitAtCloseParen rest
          cleanAfter = cleanExpr after
      in case cleanAfter of
        -- Cast: (type)expr or (type *)expr — isCastContents confirms inner is a type,
        -- so operators like & (address-of) and * (dereference) after the cast are fine
        _ | isCastContents inner && not (null cleanAfter) ->
          let typeName = T.strip $ mconcat (fmap clangText inner)
          in convertExprWithLeading (CCast ann typeName (convertExpr cleanAfter)) []
        -- Parenthesized then trailing stuff (field access, indexing, etc.)
        _ | not (null cleanAfter) ->
          convertExprWithLeading (convertExpr inner) after
        -- Just unwrap parens
        _ -> convertExpr inner

    -- Function call: FuncNameToken followed by parens
    (Leaf (ClangFuncNameToken fOpts) : rest)
      | any (isSyntaxText "(") rest ->
          let afterOpen = drop 1 $ dropWhile (not . isSyntaxText "(") rest
              (argTokens, afterCall) = splitAtCloseParen afterOpen
              args = if null argTokens then []
                     else fmap convertExpr (splitByCommaBalanced (flattenForArgs argTokens))
              call = CFuncall ann (fOpts ^. #text) args
          in if null (cleanExpr afterCall) then call
             else convertExprWithLeading call afterCall

    -- Arrow/dot field access: expr->field or expr.field
    _ | Just (lhsNodes, fieldName, isArrow, remaining) <- findFieldAccess clean ->
          let base = convertExpr lhsNodes
              fieldExpr = if isArrow
                          then CArrow ann base fieldName
                          else CDot ann base fieldName
          in if null remaining then fieldExpr
             else convertExprWithLeading fieldExpr remaining

    -- Array indexing: expr [ index ]
    _ | Just (arrExpr, idxExpr) <- findArrayIndex clean ->
          CIndex ann arrExpr idxExpr

    -- Look for assignment operators (require non-empty LHS to avoid matching prefix ops)
    _ | Just (lhs, opTxt, rhs) <- findAssignOp clean, not (null lhs) ->
          CAssign ann opTxt (convertExpr lhs) (convertExpr rhs)

    -- Look for binary operators (require non-empty LHS — prefix & or * handled below)
    _ | Just (lhs, opTxt, rhs) <- findBinaryOp clean, not (null lhs) ->
          CBinaryOp ann opTxt (convertExpr lhs) (convertExpr rhs)

    -- Pointer dereference: *expr (only when Mult appears as prefix, not binary)
    (Leaf (ClangOpToken opts) : rest) | opts ^. #opToken == Mult && not (null rest) ->
      CUnaryOp ann "*" (convertExpr rest)

    -- Prefix unary op
    (Leaf (ClangOpToken opts) : rest) | not (null rest) ->
      CUnaryOp ann (opts ^. #text) (convertExpr rest)

    -- Postfix unary op (like i++)
    _ | (lastNode : revRest) <- reverse clean, not (null revRest) ->
        case lastNode of
          Leaf (ClangOpToken opts)
            | opts ^. #opToken `elem` [Increment, Decrement] ->
                CPostfixOp ann (opts ^. #text) (convertExpr (reverse revRest))
          -- Trailing close paren/bracket from broken pointer arithmetic — strip and retry
          _ | isTrailingSyntax lastNode ->
                convertExpr (reverse revRest)
          _ -> CRawExpr ann nodes

    -- Catch-all
    _ -> CRawExpr ann nodes

-- | Handle trailing operators/indexing after a leading expression
convertExprWithLeading :: CExpr -> [ClangAST ClangNode] -> CExpr
convertExprWithLeading lhs rest =
  let clean = cleanExpr rest
      ann = exprAnn lhs <> nodesAnn rest
  in case clean of
    [] -> lhs
    -- Array index: [expr]
    (b : more) | isSyntaxText "[" b ->
      let (idxTokens, after) = splitAtCloseBracket more
      in convertExprWithLeading (CIndex ann lhs (convertExpr idxTokens)) after
    -- Arrow field access: ->field
    (s : Leaf (ClangFieldToken fOpts) : more) | isTokenText "->" s ->
      convertExprWithLeading (CArrow ann lhs (fOpts ^. #text)) more
    -- Dot field access: .field
    (s : Leaf (ClangFieldToken fOpts) : more) | isTokenText "." s ->
      convertExprWithLeading (CDot ann lhs (fOpts ^. #text)) more
    -- Arrow/dot with variable token (Ghidra sometimes uses VariableToken for fields)
    (s : Leaf (ClangVariableToken vOpts) : more) | isTokenText "->" s ->
      convertExprWithLeading (CArrow ann lhs (vOpts ^. #text)) more
    (s : Leaf (ClangVariableToken vOpts) : more) | isTokenText "." s ->
      convertExprWithLeading (CDot ann lhs (vOpts ^. #text)) more
    -- Arrow/dot with syntax token (Ghidra internal field names like _60_4_)
    (s : Leaf (ClangSyntaxToken sOpts) : more) | isTokenText "->" s ->
      convertExprWithLeading (CArrow ann lhs (sOpts ^. #text)) more
    (s : Leaf (ClangSyntaxToken sOpts) : more) | isTokenText "." s ->
      convertExprWithLeading (CDot ann lhs (sOpts ^. #text)) more
    -- Binary op continues
    (Leaf (ClangOpToken opts) : more) ->
      CBinaryOp ann (opts ^. #text) lhs (convertExpr more)
    _ -> lhs  -- give up on trailing

-- | Check if a node looks like an operator (to distinguish casts from parenthesized exprs)
isOpLike :: ClangAST ClangNode -> Bool
isOpLike (Leaf (ClangOpToken _)) = True
isOpLike n = isSyntaxText ")" n || isSyntaxText ";" n

-- | Check if a node is trailing syntax that can be stripped (stray close parens/brackets)
isTrailingSyntax :: ClangAST ClangNode -> Bool
isTrailingSyntax n = isTokenText ")" n || isTokenText "]" n

-- | Check if a syntax token text is structural punctuation (not a value)
isSyntaxPunctuation :: Text -> Bool
isSyntaxPunctuation t = t `elem` ["(", ")", "[", "]", "{", "}", ";", ",", ":", "->", "."]

-- | Check if parenthesized contents look like a type cast: all type tokens and syntax (* for pointers)
isCastContents :: [ClangAST ClangNode] -> Bool
isCastContents nodes =
  let clean = cleanExpr nodes
  in not (null clean) && all isCastToken clean
  where
    isCastToken (Leaf (ClangTypeToken _)) = True
    isCastToken (Leaf (ClangOpToken opts)) = opts ^. #opToken == Mult  -- pointer: type *
    isCastToken n = isSyntaxText "*" n

-- | Find arrow (->) or dot (.) field access, splitting into (lhs, fieldName, isArrow, remaining)
findFieldAccess :: [ClangAST ClangNode] -> Maybe ([ClangAST ClangNode], Text, Bool, [ClangAST ClangNode])
findFieldAccess nodes = goArrow nodes <|> goDot nodes
  where
    -- Find last -> to handle chained access like a->b->c (split at last for left-assoc)
    goArrow ns = findLastFieldSep "->" True ns
    goDot ns = findLastFieldSep "." False ns

    findLastFieldSep sep isArrow ns =
      let indexed = zip [0 :: Int ..] ns
          sepPositions = [i | (i, n) <- indexed, isTokenText sep n]
      in case sepPositions of
        [] -> Nothing
        _ -> let pos = last' sepPositions  -- use the LAST one for left-associativity
             in case splitAt pos ns of
                  (before, _ : after) -> case cleanExpr after of
                    (Leaf (ClangFieldToken fOpts) : more) ->
                      Just (before, fOpts ^. #text, isArrow, more)
                    (Leaf (ClangVariableToken vOpts) : more) ->
                      Just (before, vOpts ^. #text, isArrow, more)
                    (Leaf (ClangSyntaxToken sOpts) : more) | not (isSyntaxPunctuation (sOpts ^. #text)) ->
                      Just (before, sOpts ^. #text, isArrow, more)
                    _ -> Nothing
                  _ -> Nothing

    last' [] = 0
    last' xs = foldl' (\_ x -> x) 0 xs

-- | Find array indexing pattern: tokens [ index ]
-- Looks for "[" and splits around it
findArrayIndex :: [ClangAST ClangNode] -> Maybe (CExpr, CExpr)
findArrayIndex nodes =
  let (before, fromBracket) = break (isSyntaxText "[") nodes
  in case fromBracket of
    (_ : afterOpen) | not (null before) ->
      let (idxTokens, _) = splitAtCloseBracket afterOpen
      in Just (convertExpr before, convertExpr idxTokens)
    _ -> Nothing

-- | Split at matching close bracket "]", tracking nesting
splitAtCloseBracket :: [ClangAST ClangNode] -> ([ClangAST ClangNode], [ClangAST ClangNode])
splitAtCloseBracket = go (0 :: Int) []
  where
    go _ acc [] = (reverse acc, [])
    go depth acc (n : rest)
      | isSyntaxText "[" n = go (depth + 1) (n : acc) rest
      | isSyntaxText "]" n =
          if depth == 0 then (reverse acc, rest)
          else go (depth - 1) (n : acc) rest
      | otherwise = go depth (n : acc) rest

-- | Find an assignment operator and split around it
findAssignOp :: [ClangAST ClangNode] -> Maybe ([ClangAST ClangNode], Text, [ClangAST ClangNode])
findAssignOp = findOpBy isAssignOp
  where
    isAssignOp (Leaf (ClangOpToken opts)) =
      opts ^. #opToken `elem` [Assignment, AddAssign, SubAssign, MulAssign, DivAssign, ModAssign,
                                AndAssign, OrAssign, XorAssign, ShlAssign, ShrAssign]
    isAssignOp _ = False

-- | Find a binary operator and split around it (finds the LAST one for left-associativity)
findBinaryOp :: [ClangAST ClangNode] -> Maybe ([ClangAST ClangNode], Text, [ClangAST ClangNode])
findBinaryOp = findOpBy isBinOp
  where
    isBinOp (Leaf (ClangOpToken opts)) =
      opts ^. #opToken `elem` [Or, And, BitOr, BitXor, BitAnd, Equal, Neq,
                                Lt, Gt, Le, Ge, Shl, Shr, Plus, Minus, Mult, Div, Mod]
    isBinOp _ = False

-- | Generic helper to find an operator in a token list and split around it
findOpBy :: (ClangAST ClangNode -> Bool) -> [ClangAST ClangNode] -> Maybe ([ClangAST ClangNode], Text, [ClangAST ClangNode])
findOpBy predicate nodes = go [] nodes
  where
    go _ [] = Nothing
    go acc (n : rest)
      | predicate n =
          let opTxt = case n of
                Leaf (ClangOpToken opts) -> opts ^. #text
                _ -> ""
          in Just (reverse acc, opTxt, rest)
      | otherwise = go (n : acc) rest

-- | Split a token list at the matching close paren, tracking nesting depth
splitAtCloseParen :: [ClangAST ClangNode] -> ([ClangAST ClangNode], [ClangAST ClangNode])
splitAtCloseParen = go (0 :: Int) []
  where
    go _ acc [] = (reverse acc, [])
    go depth acc (n : rest)
      | isSyntaxText "(" n = go (depth + 1) (n : acc) rest
      | isSyntaxText ")" n =
          if depth == 0 then (reverse acc, rest)
          else go (depth - 1) (n : acc) rest
      | otherwise = go depth (n : acc) rest

-- | Split a list of ClangAST nodes by semicolons
-- | Split by semicolon, respecting parenthesis/bracket nesting.
-- This matters for for-loop headers where init/cond/incr may contain
-- nested expressions with semicolons (e.g., comma expressions or macros).
splitBySemicolon :: [ClangAST ClangNode] -> [[ClangAST ClangNode]]
splitBySemicolon = finish . foldl' step (0 :: Int, [], [])
  where
    step (depth, curr, parts) n
      | isTokenText "(" n || isTokenText "[" n = (depth + 1, n : curr, parts)
      | isTokenText ")" n || isTokenText "]" n = (max 0 (depth - 1), n : curr, parts)
      | isSyntaxText ";" n && depth == 0 = (0, [], reverse curr : parts)
      | otherwise = (depth, n : curr, parts)
    finish (_, curr, parts) =
      let final = reverse curr
      in reverse (if null final then parts else final : parts)

-- | Split by comma (for function arguments)
splitByComma :: [ClangAST ClangNode] -> [[ClangAST ClangNode]]
splitByComma [] = []
splitByComma nodes =
  let (chunk, rest) = break (isTokenText ",") nodes
  in chunk : case rest of
    [] -> []
    (_ : more) -> splitByComma more

-- | Flatten all branch nodes into a flat list of leaves for argument splitting.
-- Ghidra wraps function arguments in nested branch nodes, hiding commas.
flattenForArgs :: [ClangAST ClangNode] -> [ClangAST ClangNode]
flattenForArgs [] = []
flattenForArgs (Branch _ children : rest) = flattenForArgs (children ++ rest)
flattenForArgs (n : rest) = n : flattenForArgs rest

-- | Split by comma respecting parenthesization and bracketing.
-- Only splits on commas at depth 0 (outside any () or [] pairs).
-- Uses isTokenText to match commas regardless of Ghidra's token type.
splitByCommaBalanced :: [ClangAST ClangNode] -> [[ClangAST ClangNode]]
splitByCommaBalanced = finish . foldl' step (0 :: Int, [], [])
  where
    step (depth, curr, args) n
      | isTokenText "(" n || isTokenText "[" n = (depth + 1, n : curr, args)
      | isTokenText ")" n || isTokenText "]" n = (max 0 (depth - 1), n : curr, args)
      | isTokenText "," n && depth == 0 = (0, [], reverse curr : args)
      | otherwise = (depth, n : curr, args)
    finish (_, curr, args) =
      let final = reverse curr
      in reverse (if null final then args else final : args)

-- | Extract body statements from tokens after a condition (handles both braced and unbraced bodies)
extractBody :: [ClangAST ClangNode] -> [CStmt]
extractBody nodes =
  let clean = filter (not . isWhitespace) nodes
  in case clean of
    -- Braced body: find the TokenGroup
    _ | any isTokenGroup clean ->
        concatMap (\n -> if isTokenGroup n then convertBlock (astChildren n) else []) clean
    -- Single statement after condition (no braces)
    [Branch (ClangStatement _) children] -> [convertStatement children]
    -- Remaining tokens form a block
    [] -> []
    _ -> convertBlock clean

-- | Consume tokens until a semicolon (for return statements etc.)
consumeUntilSemicolon :: [ClangAST ClangNode] -> ([ClangAST ClangNode], [ClangAST ClangNode])
consumeUntilSemicolon = go []
  where
    go acc [] = (reverse acc, [])
    go acc (n : rest)
      | isSyntaxText ";" n = (reverse acc, rest)
      | otherwise = go (n : acc) rest

-- | Drop leading semicolon if present
dropSemicolon :: [ClangAST ClangNode] -> [ClangAST ClangNode]
dropSemicolon (n : rest) | isSyntaxText ";" n = rest
dropSemicolon nodes = nodes

-- * Pretty-printing

-- | Render a list of C statements as text (for debugging)
renderStmts :: Int -> [CStmt] -> Text
renderStmts indent stmts = mconcat (fmap (renderStmt indent) stmts)

-- | Render a single C statement
renderStmt :: Int -> CStmt -> Text
renderStmt n stmt =
  let pad = T.replicate (n * 2) " "
  in case stmt of
    CExprStmt _ expr -> pad <> renderExpr expr <> ";\n"
    CIf _ cond body ->
      pad <> "if (" <> renderExpr cond <> ") {\n"
      <> renderStmts (n + 1) body
      <> pad <> "}\n"
    CIfElse _ cond thenB elseB ->
      pad <> "if (" <> renderExpr cond <> ") {\n"
      <> renderStmts (n + 1) thenB
      <> pad <> "} else {\n"
      <> renderStmts (n + 1) elseB
      <> pad <> "}\n"
    CSwitch _ cond cases ->
      pad <> "switch (" <> renderExpr cond <> ") {\n"
      <> mconcat (fmap (renderCase (n + 1)) cases)
      <> pad <> "}\n"
    CWhile _ cond body ->
      pad <> "while (" <> renderExpr cond <> ") {\n"
      <> renderStmts (n + 1) body
      <> pad <> "}\n"
    CDoWhile _ body cond ->
      pad <> "do {\n"
      <> renderStmts (n + 1) body
      <> pad <> "} while (" <> renderExpr cond <> ");\n"
    CFor _ initC cond incr body ->
      pad <> "for (" <> renderForInit initC <> "; "
      <> maybe "" renderExpr cond <> "; "
      <> maybe "" renderExpr incr <> ") {\n"
      <> renderStmts (n + 1) body
      <> pad <> "}\n"
    CContinue _ -> pad <> "continue;\n"
    CBreak _ -> pad <> "break;\n"
    CReturn _ Nothing -> pad <> "return;\n"
    CReturn _ (Just expr) -> pad <> "return " <> renderExpr expr <> ";\n"
    CVarDecl _ typeName varName initExpr ->
      pad <> typeName <> " " <> varName
      <> maybe "" (\e -> " = " <> renderExpr e) initExpr <> ";\n"
    CComment _ txt -> pad <> "/* " <> T.strip txt <> " */\n"
    CBlock _ stmts ->
      pad <> "{\n" <> renderStmts (n + 1) stmts <> pad <> "}\n"
    CRawStmt _ nodes ->
      pad <> "/* raw: " <> mconcat (fmap clangText nodes) <> " */\n"

renderForInit :: CForInit -> Text
renderForInit (CForInitExpr Nothing) = ""
renderForInit (CForInitExpr (Just e)) = renderExpr e
renderForInit (CForInitDecl t v Nothing) = t <> " " <> v
renderForInit (CForInitDecl t v (Just e)) = t <> " " <> v <> " = " <> renderExpr e

renderCase :: Int -> CCase -> Text
renderCase n (CCase expr stmts) =
  let pad = T.replicate (n * 2) " "
  in pad <> "case " <> renderExpr expr <> ":\n" <> renderStmts (n + 1) stmts
renderCase n (CDefault stmts) =
  let pad = T.replicate (n * 2) " "
  in pad <> "default:\n" <> renderStmts (n + 1) stmts

-- | Render an expression
renderExpr :: CExpr -> Text
renderExpr = \case
  CIdent _ name -> name
  CLitInt _ i -> show i
  CLitString _ s -> "\"" <> s <> "\""
  CBinaryOp _ op l r -> renderExpr l <> " " <> op <> " " <> renderExpr r
  CUnaryOp _ op e -> op <> renderExpr e
  CPostfixOp _ op e -> renderExpr e <> op
  CFuncall _ name args -> name <> "(" <> T.intercalate ", " (fmap renderExpr args) <> ")"
  CAssign _ op l r -> renderExpr l <> " " <> op <> " " <> renderExpr r
  CIndex _ arr idx -> renderExpr arr <> "[" <> renderExpr idx <> "]"
  CDot _ expr fld -> renderExpr expr <> "." <> fld
  CArrow _ expr fld -> renderExpr expr <> "->" <> fld
  CCast _ typeName expr -> "(" <> typeName <> ")" <> renderExpr expr
  CCond _ c t e -> renderExpr c <> " ? " <> renderExpr t <> " : " <> renderExpr e
  CRawExpr _ nodes -> "/* raw: " <> mconcat (fmap clangText nodes) <> " */"
