{-# LANGUAGE DataKinds #-}
module Ghidra.PcodeBlock
  ( module Ghidra.PcodeBlock
  ) where

import Ghidra.Prelude hiding (toList)

import Ghidra.Types.PcodeBlock (PcodeBlock(PcodeBlock), PcodeBlockGraph(PcodeBlockGraph), BranchType(..), PcodeBlockType(..))
import qualified Language.Java as Java
import Ghidra.Address (mkAddress)
import qualified Ghidra.Types as J
import Ghidra.Types.Address (Address)
import Ghidra.Types.Internal (Ghidra, runIO)


toBlockBasic :: J.PcodeBlock -> J.PcodeBlockBasic
toBlockBasic = coerce

mkPcodeBlock :: J.PcodeBlockBasic -> Ghidra PcodeBlock
mkPcodeBlock jblock = do
  index <- runIO $ Java.call (coerce jblock :: J.PcodeBlock) "getIndex"
  return $ PcodeBlock jblock index

getBlocksFromHighFunction :: J.HighFunction -> Ghidra [PcodeBlock]
getBlocksFromHighFunction hfunc = do
  blocks :: [J.PcodeBlockBasic] <- runIO (Java.call (coerce hfunc :: J.PcodeSyntaxTree) "getBasicBlocks") >>= J.arrayListToList
  -- blocks :: [J.PcodeBlockBasic] <- Java.call (coerce hfunc :: J.PcodeSyntaxTree) "getBasicBlocks" >>= Java.reify
  traverse mkPcodeBlock blocks

getStart :: PcodeBlock -> Ghidra Address
getStart pb = runIO (Java.call (pb ^. #handle) "getStart") >>= mkAddress

getStop :: PcodeBlock -> Ghidra Address
getStop pb = runIO (Java.call (pb ^. #handle) "getStop") >>= mkAddress

-- This will error if Ghidra ever adds a new block type
getType :: PcodeBlock -> Ghidra PcodeBlockType
getType bb = do
  let (pb :: J.PcodeBlock) = coerce $ bb ^. #handle
  n :: Int32 <- runIO $ Java.call pb "getType"
  return . toEnum $ fromIntegral n

getOut :: PcodeBlock -> Int32 -> Ghidra PcodeBlock
getOut bb outIndex = runIO (Java.call pb "getOut" outIndex) >>= mkPcodeBlock . toBlockBasic
  where
    pb :: J.PcodeBlock
    pb = coerce $ bb ^. #handle

getOutgoingEdges :: PcodeBlock -> Ghidra [(BranchType, (PcodeBlock, PcodeBlock))]
getOutgoingEdges bb = getType bb >>= \case
  PLAIN -> getEdges $ Just UnknownOneOfManyBranch
  BASIC -> getEdges $ Just UnknownOneOfManyBranch
  GRAPH -> getEdges $ Just UnknownOneOfManyBranch
  COPY -> getEdges $ Just UnknownOneOfManyBranch
  GOTO -> getEdges Nothing
  MULTIGOTO -> getEdges $ Just UnconditionalBranch -- assuming this is for parallel computation
  LIST -> getEdges $ Just UnknownOneOfManyBranch
  CONDITION -> getCondEdges'
  PROPERIF -> getCondEdges'
  IFELSE -> getCondEdges'
  IFGOTO -> getCondEdges'
  WHILEDO -> getCondEdges'
  DOWHILE -> getCondEdges'
  SWITCH -> getEdges $ Just SwitchBranch
  INFLOOP -> getEdges $ Just UnknownOneOfManyBranch
  where
    getCondEdges' = getCondEdges >>= maybe (error "getCondEdges: unexpected failure") return
    getEdges :: Maybe BranchType -> Ghidra [(BranchType, (PcodeBlock, PcodeBlock))]
    getEdges edgeTypeIfMany = do
      outSize :: Int32 <- runIO $ Java.call pb "getOutSize"
      case outSize of
        0 -> return []
        1 -> do
          dest <- getOut bb 0
          return [(UnconditionalBranch, (bb, dest))]

        -- This is here because `getType` semms to always return 'BASIC'.
        -- so if the outgoing edges are 2, we attempt to getCondEdges, else
        -- treat them like unconditional edges (like for a MULTIGOTO?)
        2 -> getCondEdges >>= \case
          Nothing -> handleMultipleEdges 2
          Just x -> return x
         
        n -> handleMultipleEdges n
        where
          handleMultipleEdges n = case edgeTypeIfMany of
            Nothing -> error $ "Expected 0 or 1 edges, got " <> show n
            Just manyEdgeType -> do
              outs <- mapM (getOut bb) [0..(n-1)]
              return $ (manyEdgeType,) . (bb,) <$> outs
    
    pb :: J.PcodeBlock
    pb = coerce $ bb ^. #handle

    getCondEdges :: Ghidra (Maybe [(BranchType, (PcodeBlock, PcodeBlock))])
    getCondEdges = do
      n :: Int32 <- runIO $ Java.call pb "getOutSize"
      case n of
        2 -> do
          er <- runIO . try $ (,) <$> Java.call pb "getFalseOut" <*> Java.call pb "getTrueOut"
          case er of
            Left (_ :: SomeException) -> return Nothing
            Right (falseOut, trueOut) -> do
              fbb <- mkPcodeBlock . toBlockBasic $ falseOut
              tbb <- mkPcodeBlock . toBlockBasic $ trueOut
              return $ Just [(TrueBranch, (bb, tbb)), (FalseBranch, (bb, fbb))]
        _ -> error $ "Expected 2 outgoing edges to conditional block, got " <> show n


getPcodeBlockGraph :: J.HighFunction -> Ghidra (PcodeBlockGraph PcodeBlock)
getPcodeBlockGraph hfunc = do
  nodes <- getBlocksFromHighFunction hfunc
  edges <- concatMapM getOutgoingEdges nodes
  return $ PcodeBlockGraph nodes edges
