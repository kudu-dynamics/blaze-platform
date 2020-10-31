module Blaze.Pil.Checker.OriginMap where

import Blaze.Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Blaze.Types.Pil.Checker

-- | Creates a map of "origins" that vars are equal to.
-- The "origin" for vars remains the same, i.e. if you add (a, b)
-- to a map where multiple vars map to `a`, it just adds (b, a) to map
-- instead of adding (a, b) and updating all the `a`s to `b`.
-- returns updated map and "origin" var that 'a' and 'b' are pointing to
addToOriginMap :: (Hashable a, Eq a)
               => a -> a -> HashMap a a -> (a, Maybe a, HashMap a a)
addToOriginMap a b m = case (HashMap.lookup a m, HashMap.lookup b m) of
  (Nothing, Nothing) -> (b, Nothing, HashMap.insert a b (HashMap.insert b b m))
  (Just c, Nothing) -> (c, Nothing, HashMap.insert b c m)
  (Nothing, Just c) -> (c, Nothing, HashMap.insert a c m)
  (Just c, Just d)
    | c == d -> (c, Nothing, m)
    | otherwise -> (d, Just c, fmap (\x -> if x == c then d else x) m)

addToOriginMap_ :: (Hashable a, Eq a)
                => a -> a -> HashMap a a -> HashMap a a
addToOriginMap_ a b m = addToOriginMap a b m ^. _3


-- | Adds new var equality, returning the origin sym.
-- If the equality merges two groups, it picks the origin associated
-- with the second symbol and changes the origins of the first group to
-- the second origin. It also removes the solution associated with the
-- first origin and adds it as constraint to be later merged as solution.
addVarEq :: MonadState UnifyState m => Sym -> Sym -> m Sym
addVarEq a b = do
  m <- use originMap
  let (v, mr, m') = addToOriginMap a b m
  case mr of
    Nothing -> return ()
    Just retiredSym -> do
      sols <- use solutions
      case HashMap.lookup retiredSym sols of
        Nothing -> return ()
        Just rt -> do
          addConstraint_ retiredSym $ SType rt
          solutions %= HashMap.delete retiredSym
  originMap .= m'
  return v

originMapToGroupMap :: HashMap Sym Sym -> HashMap Sym (HashSet Sym) 
originMapToGroupMap = foldr f HashMap.empty . HashMap.toList
  where
    f (a, b) m = HashMap.alter g b m
      where
        g Nothing = Just $ HashSet.singleton a
        g (Just s) = Just $ HashSet.insert a s

originMapToGroups :: HashMap Sym Sym -> HashSet (HashSet Sym)
originMapToGroups = HashSet.fromList . HashMap.elems . originMapToGroupMap
