{- HLINT ignore "Redundant do" -}

module Blaze.PathSpec where

-- import Binja.Core
--   ( BNFunction,
--     InstructionIndex,
--   )
-- import qualified Binja.Core as BN
-- import Binja.Function
--   ( Function (Function),
--     MLILSSAFunction,
--   )
-- import qualified Binja.MLIL as MLIL
-- import Blaze.Path
--   ( AbstractCallNode (AbstractCallNode),
--     CallNode (CallNode),
--     FastPath,
--     Node
--       ( AbstractCall,
--         Call,
--         Ret,
--         SubBlock
--       ),
--     RetNode (RetNode),
--     SubBlockNode (SubBlockNode),
--   )
-- import qualified Blaze.Path as Path
import Blaze.Prelude
-- import Blaze.Types.Function
--   ( CallDest (DestFunc),
--     CallInstruction (CallInstruction),
--     CallOperation (CALL),
--     CallSite (CallSite),
--   )
-- import qualified Data.Map as Map
-- import Data.UUID as UUID
-- import Foreign.ForeignPtr
--   ( ForeignPtr,
--     newForeignPtr_,
--   )
-- import Foreign.Ptr (nullPtr)
-- import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec

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

-- subBlockNode ::
--   Hashable a =>
--   Function ->
--   InstructionIndex F ->
--   InstructionIndex F ->
--   InstructionIndex F ->
--   a ->
--   SubBlockNode
-- subBlockNode fn blockStart start end uuidBase = SubBlockNode fn blockStart start end (uuid uuidBase)

-- callInstruction :: InstructionIndex F -> CallInstruction
-- callInstruction ix =
--   CallInstruction
--     mlilCallInstr
--     (mlilCallInstr ^. MLIL.address)
--     ix
--     (mlilCallInstr ^. MLIL.size)
--     []
--     Nothing
--     []
--     (CALL mlilCallOp)
--  where
--   mlilCallDest = MLIL.Expression 0x4141 42 4 (MLIL.CONST $ MLIL.ConstOp 0x100)
--   mlilCallOp = MLIL.CallOp [] mlilCallDest []
--   mlilCallOperation = MLIL.CALL mlilCallOp
--   mlilCallInstr = MLIL.Instruction 0xcafe ix 4 mlilCallOperation

-- callSite :: Function -> Function -> InstructionIndex F -> CallSite
-- callSite caller callee ix = CallSite caller (callInstruction ix) (DestFunc callee)

-- callNode :: Hashable a => Function -> Function -> InstructionIndex F -> a -> CallNode
-- callNode caller callee ix uuidBase = CallNode caller (callSite caller callee ix) (twaddleUUID Path.callTwaddle $ uuid uuidBase)

-- retNode :: Hashable a => Function -> Function -> InstructionIndex F -> a -> RetNode
-- retNode caller callee ix uuidBase = RetNode caller (callSite caller callee ix) (twaddleUUID Path.retTwaddle $ uuid uuidBase)

-- abstractCallNode ::
--   Hashable a =>
--   Function ->
--   Function ->
--   InstructionIndex F ->
--   a ->
--   AbstractCallNode
-- abstractCallNode caller callee ix uuidBase = AbstractCallNode caller (callSite caller callee ix) (uuid uuidBase)

-- diveBin :: FilePath
-- diveBin = "res/test_bins/Dive_Logger/Dive_Logger.bndb"

spec :: Spec
spec = describe "Blaze.Path" $ do
  context "when avoiding" $ do
    it "placeholder" $
      True `shouldBe` True

--   context "Load All Function Paths" $ do
--     ebv <- runIO $ BN.getBinaryView diveBin
--     let (Right bv) = ebv
--     paths <- runIO (Path.pathsForAllFunctions bv :: IO (Map Function [FastPath]))
--     it "should load paths for all functions" $ do
--       (sort . fmap (length . snd) . Map.toList $ paths)
--         `shouldBe` [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 6, 7, 7, 7, 12, 18, 32, 32, 33, 68, 108, 224]
--     return ()
--   context "Expand Abstract Call Nodes" $ do
--     let func1 = func "func1"
--         func2 = func "func2"
--         acn = abstractCallNode func1 func2 5 ("call" :: Text)
--         p1 :: FastPath
--         p1 =
--           Path.fromList
--             [ SubBlock $ subBlockNode func1 0 0 5 ("caller block1" :: Text),
--               AbstractCall acn,
--               SubBlock $ subBlockNode func1 0 6 10 ("caller block2" :: Text)
--             ]
--         p2 :: FastPath
--         p2 =
--           Path.fromList
--             [ SubBlock $ subBlockNode func2 0 0 20 ("callee block1" :: Text)
--             ]
--         pr :: FastPath
--         pr =
--           Path.fromList
--             [ SubBlock $ subBlockNode func1 0 0 5 ("caller block1" :: Text),
--               Call $ callNode func1 func2 5 ("call" :: Text),
--               SubBlock $ subBlockNode func2 0 0 20 ("callee block1" :: Text),
--               Ret $ retNode func1 func2 5 ("call" :: Text),
--               SubBlock $ subBlockNode func1 0 6 10 ("caller block2" :: Text)
--             ]
--     it "should expandAbstractCallNode" $ do
--       Path.expandAbstractCall acn (fromJust . Path.mkInsertablePath $ p2) p1 `shouldBe` pr
--     return ()
