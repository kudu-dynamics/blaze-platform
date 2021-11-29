module Blaze.PathDemo where

-- import Blaze.Prelude

-- import qualified Binja.Core as BN
-- import Blaze.Function ( CallSite(CallSite)
--                       , CallDest(DestFunc)
--                       , CallInstruction(CallInstruction)
--                       , CallOperation(SYSCALL)
--                       )
-- import Binja.Core (BNBinaryView, InstructionIndex, BNFunction)
-- import qualified Binja.MLIL as BnMlil
-- import qualified Blaze.Graph          as Graph
-- import qualified Binja.Function as Func
-- import Binja.Function (Function(Function), MLILSSAFunction)
-- import qualified Binja.BasicBlock as BB
-- import Blaze.Types.Pil (Stmt)
-- import qualified Data.Map as Map
-- import qualified Blaze.Path as P
-- import Blaze.Path ( Path
--                   , FastPath
--                   , SubBlockNode(SubBlockNode)
--                   , CallNode(CallNode)
--                   , RetNode(RetNode)
--                   , AbstractCallNode(AbstractCallNode)
--                   , Node ( SubBlock
--                          , Call
--                          , Ret
--                          , AbstractCall )
--                   )
-- import qualified Blaze.Import.Source.BinaryNinja.Pil.Path as PP
-- import Blaze.Types.Graph.Alga (AlgaGraph)
-- import qualified Binja.MLIL as MLIL

-- import Data.UUID as UUID
-- import Foreign.Ptr (nullPtr)
-- import Foreign.ForeignPtr (newForeignPtr_, ForeignPtr)
-- import System.IO.Unsafe (unsafePerformIO)


-- type F = MLILSSAFunction

-- nullForeignPtr :: IO (ForeignPtr a)
-- nullForeignPtr = newForeignPtr_ nullPtr

-- {-# NOINLINE nullFuncPtr #-}
-- nullFuncPtr :: BNFunction
-- nullFuncPtr = unsafePerformIO $ BN.BNFunction <$> nullForeignPtr

-- func :: Text -> Function
-- func fname = Function nullFuncPtr fname (fromIntegral . hash $ fname)

-- uuid :: Hashable a => a -> UUID
-- uuid x = UUID.fromWords h h h h
--   where
--     h = fromIntegral $ hash x

-- subBlockNode :: Hashable a
--          => Function -> InstructionIndex F -> InstructionIndex F -> InstructionIndex F
--          -> a
--          -> SubBlockNode
-- subBlockNode fn blockStart start end uuidBase = SubBlockNode fn blockStart start end (uuid uuidBase)

-- callInstruction :: InstructionIndex F -> CallInstruction
-- callInstruction ix =
--   CallInstruction (BnMlil.Instruction 0 0 0 BnMlil.NOP) 0 ix 0 [] Nothing [] callOp
--   where
--     callOp = SYSCALL sysExpr
--     sysExpr = MLIL.SyscallOp [] []

-- callSite :: Function -> Function -> InstructionIndex F -> CallSite
-- callSite caller callee ix = CallSite caller (callInstruction ix) (DestFunc callee)

-- callNode :: Hashable a => Function -> Function -> InstructionIndex F -> a -> CallNode
-- callNode caller callee ix uuidBase = CallNode caller (callSite caller callee ix) (twaddleUUID P.callTwaddle $ uuid uuidBase)

-- retNode :: Hashable a => Function -> Function -> InstructionIndex F -> a -> RetNode
-- retNode caller callee ix uuidBase = RetNode caller (callSite caller callee ix) (twaddleUUID P.retTwaddle $ uuid uuidBase)

-- abstractCallNode :: Hashable a => Function -> Function -> InstructionIndex F -> a
--                  -> AbstractCallNode
-- abstractCallNode caller callee ix uuidBase = AbstractCallNode caller (callSite caller callee ix) (uuid uuidBase)

-- diveBin :: FilePath
-- diveBin = "res/test_bins/Dive_Logger/Dive_Logger.bndb"

-- diveBv :: IO BNBinaryView
-- diveBv = do
--   (Right bv) <- BN.getBinaryView diveBin
--   return bv

-- testBnAndFunc :: IO (BNBinaryView, Function)
-- testBnAndFunc = do
--   (Right bv) <- BN.getBinaryView diveBin
--   (Just addDive) <- Func.getFunctionStartingAt bv Nothing 0x804c7d0
--   return (bv, addDive)

-- testPath :: Path p => IO p
-- testPath = do
--   (bv, fn) <- testBnAndFunc
--   paths <- P.allSimpleFunctionPaths bv fn
--   return $ head paths

-- addDivePathsConvertTest :: IO [Stmt]
-- addDivePathsConvertTest = do
--   path <- testPath
--   (bv, _fn) <- testBnAndFunc
--   PP.convertPath bv path
  
-- -- TODO: BasicBlock `end` instr index is +1 actual last instruction

-- testbb :: IO FastPath
-- testbb = do
--   (bv, fn) <- testBnAndFunc
--   mlilFn <- Func.getMLILSSAFunction fn
--   (Just bb) <- BB.getBasicBlockForInstruction mlilFn 0
--   bbg <- Graph.constructBasicBlockGraph mlilFn :: IO (AlgaGraph (BB.BlockEdge F) (BB.BasicBlock F))
--   P.pathFromBasicBlockList bv bbg [bb]

-- pathsForAllFunctions :: BNBinaryView -> IO (Map Function [FastPath])
-- pathsForAllFunctions bv = do
--   fns <- Func.getFunctions bv
--   Map.fromList <$> traverse g fns
--   where
--     g :: Function -> IO (Function, [FastPath])
--     g fn = (fn,) <$> P.allSimpleFunctionPaths bv fn


-- testExpand :: Bool
-- testExpand = P.expandAbstractCall acn (fromJust . P.mkInsertablePath $ p2) p1
--              == pr
--   where
--     func1 = func "func1"
--     func2 = func "func2"
--     acn = abstractCallNode func1 func2 5 ("call" :: Text)
--     p1 :: FastPath
--     p1 = P.fromList
--          [ SubBlock $ subBlockNode func1 0 0 5 ("caller block1" :: Text)
--          , AbstractCall acn
--          , SubBlock $ subBlockNode func1 0 6 10 ("caller block2" :: Text)
--          ]
--     p2 :: FastPath
--     p2 = P.fromList
--          [ SubBlock $ subBlockNode func2 0 0 20 ("callee block1" :: Text)
--          ]
--     pr :: FastPath
--     pr = P.fromList
--          [ SubBlock $ subBlockNode func1 0 0 5 ("caller block1" :: Text)
--          , Call $ callNode func1 func2 5 ("call" :: Text)
--          , SubBlock $ subBlockNode func2 0 0 20 ("callee block1" :: Text)
--          , Ret $ retNode func1 func2 5 ("call" :: Text)
--          , SubBlock $ subBlockNode func1 0 6 10 ("caller block2" :: Text)
--          ]
