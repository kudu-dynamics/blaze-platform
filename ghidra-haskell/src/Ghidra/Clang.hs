{-# LANGUAGE DerivingStrategies #-}

module Ghidra.Clang
  ( module Ghidra.Clang
  ) where

import Ghidra.Prelude hiding (replace, DataType)
import Data.Text (replace)
import qualified Language.Java as Java
import qualified Ghidra.Types as J
import Ghidra.Types.Internal (Ghidra, runIO)
import Foreign.JNI.Types (JClass)
import Ghidra.Pcode (getBarePcodeOp)
import Ghidra.Types.Pcode (BarePcodeOp)
import Ghidra.Types.Variable (DataType(..), VarNode(..), HighVariable(..), HighSymbol(..))
import Ghidra.Variable (mkVarNode, mkHighVariable, getHighSymbol)
import Ghidra.Types.Address (Address)
import Ghidra.Address (mkAddress)
import Language.Java (J)
import Ghidra.Util (isJNull)
import Data.Type.Equality

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

getOpToken :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra OpToken
getOpToken node = do
  tokStr :: Text <- runIO $ Java.call node "getText" >>= Java.reify
  let tok = matchToken tokStr
  if tok == Unimplemented then do
    runIO $ pprint tokStr
    return tok
  else return tok
  --return $ matchToken tokStr



matchToken :: Text -> OpToken
matchToken t
  | t == "if"     = If
  | t == "=="     = Equal
  | t == "!="     = Neq
  | t == "+"      = Plus
  | t == "*"      = Mult
  | t == "switch" = Switch
  | t == "return" = Return
  | t == "while"  = While
  | t == "case"   = Case
  | t == "for"    = For
  | t == "||"     = Or
  | t == "="      = Assignment
  | otherwise     = Unimplemented

data OpToken
  = If
  | Equal
  | Neq 
  | Plus
  | Mult
  | Switch
  | Return
  | While
  | Case
  | For
  | Or
  | Assignment
  | Unimplemented
  deriving(Eq, Ord, Show, Generic, Hashable)
  


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

-- gets the structure datatype associated with this field token
getDataType :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra (Maybe DataType)
getDataType node = do
  dt :: J.DataType <- runIO $ Java.call node "getDataType"
  if isJNull dt then
    return Nothing
  else do
    name :: Text <- runIO $ Java.call dt "getName" >>= Java.reify
    return $ Just DataType { name = name }

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

getHighVariable :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra (Maybe HighVariable)
getHighVariable node = do
  highVar :: J.HighVariable <- runIO $ Java.call node "getHighVariable"
  if isJNull highVar then return Nothing
  else Just <$> mkHighVariable highVar

getHighSymbol' :: (ty ~ Java.Ty a, Java.IsReferenceType ty, Java.Coercible a, Coercible a (J ty)) => a -> Ghidra (Maybe HighSymbol)
getHighSymbol' node = do
  highVar :: J.HighVariable <- runIO $ Java.call node "getHighVariable"
  if isJNull highVar then return Nothing
  else getHighSymbol highVar


buildClangAST :: J.ClangNode -> Ghidra (ClangAST ClangNode)
buildClangAST node = do
  children <- getChildren node
  childrenAST <- mapM buildClangAST children
  node' <- constructClangNode node
  return $
    if null childrenAST then Leaf node'
    else Branch node' childrenAST


constructClangNode :: J.ClangNode -> Ghidra ClangNode
constructClangNode node = do
  className <- replace "ghidra.app.decompiler." "" <$> getClassName node
  addrRange <- getAddressRange node
  case className of
    "ClangBreak" -> do
      let node' :: J.ClangBreak = coerce node
      indent <- getIndent node'
      return $ ClangBreak ClangBreakOpts { indent = indent }
    "ClangCaseToken"      ->  do
      let node' :: J.ClangCaseToken = coerce node
      opTok :: OpToken <- getOpToken node'
      _ <- runIO $ pprint opTok
      highSymbol    <- fromJust <$> getHighSymbol'  node'
      highVariable  <- fromJust <$> getHighVariable node'
      pcodeOp       <- fromJust <$> getPcodeOp      node'
      scalar        <- fromJust <$> getScalar       node'
      switchOp      <- fromJust <$> getSwitchOp     node'
      varnode       <- fromJust <$> getVarNode      node'
      isVarRef      <- getIsVarRef node'
      return $ ClangCaseToken ClangCaseTokenOpts { addrRange = fromJust addrRange, highSymbol = highSymbol, highVariable = highVariable
                                                 , pcodeOp = pcodeOp, scalar = scalar, switchOp = switchOp, varnode = varnode, isVarRef = isVarRef }
    "ClangCommentToken"   -> do
      let node' :: J.ClangCommentToken = coerce node
      isVarRef <- getIsVarRef node'
      return $ ClangCommentToken ClangCommentTokenOpts { addrRange = addrRange, isVarRef = isVarRef }
    "ClangFieldToken"     -> do
      let node' :: J.ClangFieldToken = coerce node
      datatype <- fromJust <$> getDataType node'
      offset <- getOffset node'
      pcodeOp <- fromJust <$> getPcodeOp node'
      return $ ClangFieldToken ClangFieldTokenOpts { addrRange = addrRange, datatype = datatype, offset = offset, pcodeOp = pcodeOp }
    "ClangFuncNameToken"  ->  do
      let node' :: J.ClangFuncNameToken = coerce node
      pcodeOp <- getPcodeOp node'
      return $ ClangFuncNameToken ClangFuncNameTokenOpts { addrRange = addrRange, pcodeOp = pcodeOp }
    "ClangFuncProto"      ->  return $ ClangFuncProto ClangFuncProtoOpts
    "ClangFunction"       ->  return $ ClangFunction  ClangFunctionOpts       { addrRange = fromJust addrRange }
    "ClangLabelToken"     -> do
      let node' :: J.ClangLabelToken = coerce node
      highSymbol    <- getHighSymbol'  node'
      highVariable  <- getHighVariable node'
      pcodeOp       <- getPcodeOp      node'
      scalar        <- getScalar       node'
      varnode       <- getVarNode      node'
      isVarRef      <- getIsVarRef node'
      return $ ClangLabelToken ClangLabelTokenOpts { addrRange = addrRange, highSymbol = highSymbol, highVariable = highVariable
                                                   , pcodeOp = pcodeOp, scalar = scalar, varnode = varnode, isVarRef = isVarRef }
    "ClangOpToken"        ->  do
      let node' :: J.ClangOpToken = coerce node
      pcodeOp       <- getPcodeOp      node'
      tok :: OpToken <- getOpToken node'
      --runIO $ pprint tok
      return $ ClangOpToken ClangOpTokenOpts { addrRange = addrRange, pcodeOp = pcodeOp, opToken = tok }
    "ClangReturnType"     ->  do
      let node' :: J.ClangReturnType = coerce node
      dt <- getDataType node'
      varnode <- getVarNode node'
      return $ ClangReturnType ClangReturnTypeOpts { datatype = dt, varnode = varnode }
    "ClangStatement"      ->  do
      let node' :: J.ClangStatement = coerce node
      barePCodeOp <- getPcodeOp node'
      return $ ClangStatement ClangStatementOpts { addrRange = addrRange, pcodeOp = barePCodeOp }
    "ClangSyntaxToken"    -> do
      let node' :: J.ClangSyntaxToken = coerce node
      isVarRef <- getIsVarRef node'
      pairId <- getPairId node'
      return $ ClangSyntaxToken ClangSyntaxTokenOpts { isVarRef = isVarRef, pairId = pairId }
    "ClangToken"          ->  return $ ClangToken ClangTokenOpts { addrRange = addrRange }
    "ClangTokenGroup"     ->  return $ ClangTokenGroup ClangTokenGroupOpts { addrRange = addrRange }
    "ClangTypeToken"      -> do
      let node' :: J.ClangTypeToken = coerce node
      isVarRef <- getIsVarRef node'
      datatype <- fromJust <$> getDataType node'
      return $ ClangTypeToken ClangTypeTokenOpts { datatype = datatype, isVarRef = isVarRef }
    "ClangVariableDecl"   -> do
      let node' :: J.ClangVariableDecl = coerce node
      datatype <- fromJust <$> getDataType node'
      highSymbol <- getHighSymbol' node'
      highVariable <- getHighVariable node'
      return $ ClangVariableDecl ClangVariableDeclOpts { datatype = datatype, highSymbol = highSymbol, highVariable = highVariable }
    "ClangVariableToken"  -> do
      let node' :: J.ClangVariableToken = coerce node
      highSymbol <- getHighSymbol' node'
      highVariable <- getHighVariable node'
      pcodeOp <- getPcodeOp node'
      scalar <- getScalar node'
      varnode <- getVarNode node'
      isVarRef <- getIsVarRef node'
      return $ ClangVariableToken ClangVariableTokenOpts { addrRange = addrRange, highSymbol = highSymbol, highVariable = highVariable
                                                         , pcodeOp = pcodeOp, scalar = scalar, varnode = varnode, isVarRef = isVarRef }

    _ -> error "unrecognized ClangNode"


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
  { addrRange     :: AddrRange
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
  { addrRange :: Maybe AddrRange
  , isVarRef  :: Bool
  } deriving (Eq, Ord, Show, Generic, Hashable)

-- not tested (add more information from ClangToken when testing)
data ClangFieldTokenOpts = ClangFieldTokenOpts
  { addrRange :: Maybe AddrRange
  , datatype  :: DataType
  , offset    :: Int32
  , pcodeOp   :: BarePcodeOp
  } deriving (Eq, Ord, Show, Generic, Hashable)

data ClangFuncNameTokenOpts = ClangFuncNameTokenOpts
  { addrRange :: Maybe AddrRange
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
  { addrRange     :: Maybe AddrRange
  , highSymbol    :: Maybe HighSymbol
  , highVariable  :: Maybe HighVariable
  , pcodeOp       :: Maybe BarePcodeOp
  , scalar        :: Maybe Scalar
  , varnode       :: Maybe VarNode
  , isVarRef      :: Bool
  } deriving (Eq, Ord, Show, Generic, Hashable)

data ClangOpTokenOpts = ClangOpTokenOpts
  { addrRange :: Maybe AddrRange
  , pcodeOp   :: Maybe BarePcodeOp
  , opToken     :: OpToken
  } deriving (Eq, Ord, Show, Generic, Hashable)

data ClangReturnTypeOpts = ClangReturnTypeOpts
  { datatype  :: Maybe DataType
  , varnode   :: Maybe VarNode
  } deriving (Eq, Ord, Show, Generic, Hashable)

data ClangStatementOpts = ClangStatementOpts
  { addrRange :: Maybe AddrRange
  , pcodeOp   :: Maybe BarePcodeOp
  } deriving (Eq, Ord, Show, Generic, Hashable)

-- haven't tested the ClangToken aspect, but if it's like ClangOpTokenOpts, then it won't have a lot of information
data ClangSyntaxTokenOpts = ClangSyntaxTokenOpts
  { isVarRef  :: Bool
  , pairId    :: Maybe Int32
  } deriving (Eq, Ord, Show, Generic, Hashable)

newtype ClangTokenOpts = ClangTokenOpts
  { addrRange :: Maybe AddrRange
  } deriving (Eq, Ord, Show, Generic)
    deriving anyclass Hashable

newtype ClangTokenGroupOpts = ClangTokenGroupOpts
  { addrRange :: Maybe AddrRange 
  } deriving (Eq, Ord, Show, Generic)
    deriving anyclass Hashable

-- haven't tested the ClangToken aspect, but if it's like ClangOpTokenOpts, then it won't have a lot of information
data ClangTypeTokenOpts = ClangTypeTokenOpts
  { datatype :: DataType
  , isVarRef :: Bool
  } deriving (Eq, Ord, Show, Generic, Hashable)

data ClangVariableDeclOpts = ClangVariableDeclOpts
  { datatype      :: DataType
  , highSymbol    :: Maybe HighSymbol
  , highVariable  :: Maybe HighVariable
  } deriving (Eq, Ord, Show, Generic, Hashable)

data ClangVariableTokenOpts = ClangVariableTokenOpts
  { addrRange     :: Maybe AddrRange
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

{-
newtype Statement = Statement ClangStatementOpts

data SwitchOpts = SwitchOpts
  { condition :: Statement
  , cases     :: [CaseOpts] 
  }

data CaseOpts = CaseOpts
  { condition :: Statement
  , body      :: Statement
  }

data IfOpts = IfOpts
  { condition :: Statement
  , body      :: Statement
  }


data ElseIFOpts = ElseIfOpts
  { condition :: Statement
  , body      :: Statement
  }

data ForOpts = ForOpts
  { init      :: Statement
  , loopCond  :: Statement
  , incCond   :: Statement
  , body      :: Statement
  }

data While = WhileOpts
  { loopCond  :: Statement
  , body      :: Statement
  }

data ASTNode
  = For'    ForOpts
  | Switch' SwitchOpts
  | If'     IfOpts
  | ElseIf' ElseIfOpts

-}