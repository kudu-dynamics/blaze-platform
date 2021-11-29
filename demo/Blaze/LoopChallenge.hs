module Blaze.LoopChallenge where

import Blaze.Prelude
import Data.SBV
import qualified Data.SBV.String as S
import qualified Data.SBV.Maybe as M
import Data.SBV.Maybe (sNothing, sJust)
import qualified Data.SBV.Tuple as T
import qualified Data.SBV.Char as C
import Data.SBV.Control


matchedCharsCount :: SString -> SString -> SInteger
matchedCharsCount s1 s2 = ite
  (S.null s1 .|| S.null s2 .|| S.head s1 ./= S.head s2)
  0
  (1 + matchedCharsCount (S.tail s1) (S.tail s2))


add5 :: SInteger -> Symbolic ()
add5 y = do
  x <- sInteger "r"
  constrain $ x .== y + 5
  return ()
  -- return $ x .== y + 5

repeatUntilNothing :: (SymVal a)
                   => (SBV a -> SMaybe a) -> SBV a -> SBV a
repeatUntilNothing f env = M.maybe env
  (repeatUntilNothing f)
  $ f env

jim :: Symbolic ()
jim = do
  x <- sInteger "x"
  y <- sInteger "y"
  constrain $ T.tuple (x, y) .==
    repeatUntilNothing
    ( \tup -> ite (T._1 tup .== 10)
              sNothing
              (sJust $ T.tuple (T._1 tup + 1, T._2 tup - 1)))
    (T.tuple (0, 7))
  return ()

nulChar :: SChar
nulChar = C.chr 0

newlineChar :: SChar
newlineChar = C.chr 10

fgets :: SString -> SInteger -> Symbolic ()
fgets s i = do
  constrain $ S.length s .>= i .&& i .> 0
  x <- sInteger "fgets_newline_loc"
  constrain $ reachedEndOfString .|| gotNewLine x
  where
    reachedEndOfString =
      (S.strToCharAt s (i - 1) .== nulChar)
      .&& C.notElem nulChar ss
      .&& C.notElem newlineChar ss
      where
        ss = S.subStr s 0 (i - 1)
    gotNewLine x = (x .>= 0)
      .&& (x + 1 .< i)
      .&& (x .< S.length s - 1)
      .&& (S.strToCharAt s x .== newlineChar)
      .&& (S.strToCharAt s (x + 1) .== nulChar)
      .&& C.notElem nulChar ss
      .&& C.notElem newlineChar ss
      where
        ss = S.subStr s 0 (x - 1)

      
fgets2 :: SString -> SInteger -> Symbolic ()
fgets2 s i = do
  constrain $ S.length s .> i
  constrain $ i .> 0
  constrain $ fgets_ s (i - 1)

fgets_ :: SString -> SInteger -> SBool
fgets_ s i = ite (i .== 0)
  (c0 .== nulChar)
   -- (c0 ./= nulChar .&& (fgets_ (S.tail s) i (n + 1)))
  (ite (c0 .== newlineChar)
    (c1 .== nulChar)
    (c0 ./= nulChar .&& fgets_ (S.tail s) (i - 1)))
  where
    c0 = S.head s
    c1 = S.head . S.tail $ s

forLoop :: SymVal a
        => SInteger -> SInteger -> (SInteger -> SInteger)
        -- folding function to make new env
        -- Nothing means "Break"
        -> (SInteger -> SBV a -> SMaybe a) 
        -> SBV a -- env
        -> SBV a
forLoop i end strider f env = ite (i .== end) env $
  M.maybe env (forLoop (strider i) end strider f) $ f i env

main :: Symbolic ()
main = do
  symbol <- sString "symbol"
  constrain $ S.length symbol .== 10
  --fgets symbol 9
  key <- sString "key"
  constrain $ key .== "deadbeef\NUL"
  result <- sInteger "result"
  let f i r = let a = S.strToCharAt symbol i
                  b = S.strToCharAt key i
              in
                ite (a ./= b) sNothing (sJust $ r + 1)
  constrain $ result .== forLoop 0 8 (+1) f 0
  constrain $ result .== 5
  return ()

stringArrayDemoFails :: Symbolic ()
stringArrayDemoFails = do
  a <- newArray "a" Nothing :: Symbolic (SArray String String)
  let a' = writeArray a "0" "dummy"
  constrain $ readArray a' "0" .== "test"
  return ()

stringArrayDemo :: Symbolic ()
stringArrayDemo = do
  a <- newArray "a" Nothing :: Symbolic (SArray String String)
  dummy <- sString "dummyVar"
  let a' = writeArray a "0" dummy
  constrain $ readArray a' "0" .== "dummy"
  return ()


stringArrayDemo2 :: Symbolic ()
stringArrayDemo2 = do
  a <- newArray "a" Nothing :: Symbolic (SArray String String)
  dummy <- sString "dummyVar"
  test2 <- sString "test2"
  constrain $ S.isInfixOf "Contains?" test2
  let a' = writeArray a "0" dummy
      a'' = writeArray a' "x" test2
  constrain $ readArray a' "0" .== "dummy"
  constrain $ readArray a'' "x" .== test2
  return ()


--- loop test...

-- x = 0
-- if (x == 30) then B else A
-- A: x = x + 1
-- B:


test :: Symbolic (Maybe (Integer, Integer))
test = do
  x <- sInteger "x"   -- a free variable named "x"
  y <- sInteger "y"   -- a free variable named "y"

  -- require the sum to be 10
  constrain $ x + y .== 10

  -- Go into the Query mode
  query $ do
    -- Query the solver: Are the constraints satisfiable?
    print =<< getAssertionStackDepth
    checkSat >>= \case
      Unk   -> error "Solver said unknown!"
      Unsat -> return Nothing -- no solution!
      DSat _ -> return Nothing
      Sat   -> -- Query the values:
        do xv <- getValue x
           yv <- getValue y
    
           io $ putStrLn $ "Solver returned: " ++ show (xv, yv)

           -- We can now add new constraints,
           -- Or perform arbitrary computations and tell
           -- the solver anything we want!
           constrain $ x .> literal xv + literal yv
           print =<< getAssertionStackDepth
           -- call checkSat again
           checkSat >>= \case
             Unk   -> error "Solver said unknown!"
             Unsat -> return Nothing
             DSat _ -> return Nothing
             Sat   -> do xv2 <- getValue x
                         yv2 <- getValue y
                         return $ Just (xv2, yv2)


pair :: IO (Maybe (Integer, Integer))
pair = runSMT test
