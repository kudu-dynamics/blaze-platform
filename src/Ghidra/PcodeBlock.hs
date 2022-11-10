{-# LANGUAGE DataKinds #-}
module Ghidra.PcodeBlock
  ( module Ghidra.PcodeBlock
  ) where

import Ghidra.Prelude hiding (toList)

import Ghidra.State (GhidraState)
import qualified Ghidra.State as State
import Ghidra.Types.PcodeBlock (PcodeBlockGraph(PcodeBlockGraph), BranchType(..), PcodeBlockType(..))
import qualified Language.Java as Java
import qualified Ghidra.Address as Addr
import Ghidra.Address (mkAddress)
import qualified Foreign.JNI as JNI
import qualified Ghidra.Types as J
import Ghidra.Types.Address (Address)
import Ghidra.Types.PcodeBlock (PcodeBlock(PcodeBlock))
import qualified Data.Set as Set

toBlockBasic :: J.PcodeBlock -> J.PcodeBlockBasic
toBlockBasic = coerce

mkPcodeBlock :: J.PcodeBlockBasic -> IO PcodeBlock
mkPcodeBlock jblock = do
  index <- Java.call (coerce jblock :: J.PcodeBlock) "getIndex"
  return $ PcodeBlock jblock index

getBlocksFromHighFunction :: J.HighFunction -> IO [PcodeBlock]
getBlocksFromHighFunction hfunc = do
  blocks :: [J.PcodeBlockBasic] <- Java.call (coerce hfunc :: J.PcodeSyntaxTree) "getBasicBlocks" >>= J.arrayListToList
  -- blocks :: [J.PcodeBlockBasic] <- Java.call (coerce hfunc :: J.PcodeSyntaxTree) "getBasicBlocks" >>= Java.reify
  traverse mkPcodeBlock blocks

getStart :: PcodeBlock -> IO Address
getStart pb = Java.call (pb ^. #handle) "getStart" >>= mkAddress

getStop :: PcodeBlock -> IO Address
getStop pb = Java.call (pb ^. #handle) "getStop" >>= mkAddress

-- This will error if Ghidra ever adds a new block type
getType :: PcodeBlock -> IO PcodeBlockType
getType bb = do
  let (pb :: J.PcodeBlock) = coerce $ bb ^. #handle
  n :: Int32 <- Java.call pb "getType"
  return . toEnum $ fromIntegral n

getOut :: PcodeBlock -> Int32 -> IO PcodeBlock
getOut bb outIndex = Java.call pb "getOut" outIndex >>= mkPcodeBlock . toBlockBasic
  where
    pb :: J.PcodeBlock
    pb = coerce $ bb ^. #handle

getOutgoingEdges :: PcodeBlock -> IO [(BranchType, (PcodeBlock, PcodeBlock))]
getOutgoingEdges bb = getType bb >>= \case
  PLAIN -> getEdges $ Just UnknownOneOfManyBranch
  BASIC -> getEdges $ Just UnknownOneOfManyBranch
  GRAPH -> getEdges $ Just UnknownOneOfManyBranch
  COPY -> getEdges $ Just UnknownOneOfManyBranch
  GOTO -> getEdges Nothing
  MULTIGOTO -> getEdges $ Just UnconditionalBranch -- assuming this is for parallel computation
  LIST -> getEdges $ Just UnknownOneOfManyBranch
  CONDITION -> getCondEdges
  PROPERIF -> getCondEdges
  IFELSE -> getCondEdges
  IFGOTO -> getCondEdges
  WHILEDO -> getCondEdges
  DOWHILE -> getCondEdges
  SWITCH -> getEdges $ Just SwitchBranch
  INFLOOP -> getEdges $ Just UnknownOneOfManyBranch
  where
    getEdges :: Maybe BranchType -> IO [(BranchType, (PcodeBlock, PcodeBlock))]
    getEdges edgeTypeIfMany = do
      outSize :: Int32 <- Java.call pb "getOutSize"
      case outSize of
        0 -> return []
        1 -> do
          dest <- getOut bb 0
          return [(UnconditionalBranch, (bb, dest))]

        -- This is here because `getType` semms to always return 'BASIC'.
        -- so if the outgoing edges are 2, we attempt to getCondEdges, else
        -- treat them like unconditional edges (like for a MULTIGOTO?)
        2 -> catch getCondEdges (\ (_ :: SomeException) -> handleMultipleEdges 2)
         
        n -> handleMultipleEdges n
        where
          handleMultipleEdges n = case edgeTypeIfMany of
            Nothing -> error $ "Expected 0 or 1 edges, got " <> show n
            Just manyEdgeType -> do
              outs <- mapM (getOut bb) [0..(n-1)]
              return $ (manyEdgeType,) . (bb,) <$> outs
    
    pb :: J.PcodeBlock
    pb = coerce $ bb ^. #handle

    getCondEdges :: IO [(BranchType, (PcodeBlock, PcodeBlock))]
    getCondEdges = do
      n :: Int32 <- Java.call pb "getOutSize"
      case n of
        2 -> do
          fbb <- Java.call pb "getFalseOut" >>= mkPcodeBlock . toBlockBasic
          tbb <- Java.call pb "getTrueOut" >>= mkPcodeBlock . toBlockBasic
          return [(TrueBranch, (bb, tbb)), (FalseBranch, (bb, fbb))]
        _ -> error $ "Expected 2 outgoing edges to conditional block, got " <> show n


getPcodeBlockGraph :: J.HighFunction -> IO (PcodeBlockGraph PcodeBlock)
getPcodeBlockGraph hfunc = do
  nodes <- getBlocksFromHighFunction hfunc
  edges <- concatMapM getOutgoingEdges nodes
  return $ PcodeBlockGraph nodes edges
