module HChecker where


import Blaze.Prelude hiding (Type, list, modify)
import qualified Prelude as P
import qualified Data.List as List
import qualified Data.Map as Map

type CheckerError = Text

type Id = Text

tUnit :: Type
tUnit = TCon $ Tycon "()" Star

tChar :: Type
tChar = TCon $ Tycon "Char" Star

tInt :: Type
tInt = TCon $ Tycon "Int" Star

tInteger :: Type
tInteger = TCon $ Tycon "Integer" Star

tFloat :: Type
tFloat = TCon $ Tycon "Float" Star

tDouble :: Type
tDouble = TCon $ Tycon "Double" Star

tList :: Type
tList = TCon $ Tycon "[]" (Kfun Star Star)

tArrow :: Type
tArrow = TCon $ Tycon "(->)" (Kfun Star (Kfun Star Star))

tTuple2 :: Type
tTuple2 = TCon $ Tycon "(,)" (Kfun Star (Kfun Star Star))

f1 :: Type
f1 = TAp (TAp tArrow tInt) (TAp tList (TVar $ Tyvar "a" Star))

list :: Type -> Type
list = TAp tList

infixr 4 `fn`

fn :: Type -> Type -> Type
fn a b = TAp (TAp tArrow a) b

pair :: Type -> Type -> Type
pair a b = TAp (TAp tTuple2 a) b


data Kind = Star
          | Kfun Kind Kind
          deriving (Eq, Show)

data Type = TVar Tyvar
          | TCon Tycon
          | TAp Type Type
          | TGen Int
          deriving (Eq, Show)

data Tyvar = Tyvar Id Kind
  deriving (Eq, Show)

data Tycon = Tycon Id Kind
  deriving (Eq, Show)

class HasKind t where
  kind :: t -> Kind

instance HasKind Tyvar where
  kind (Tyvar _ k) = k

instance HasKind Tycon where
  kind (Tycon _ k) = k

instance HasKind Type where
  kind (TVar t) = kind t
  kind (TCon t) = kind t
  kind (TAp t _) = case kind t of
    (Kfun _ k) -> k
    _ -> P.error "TAp's first kind cannot be *"
  kind (TGen _) = P.error "Can't determine kind of TGen"

type Subst = [(Tyvar, Type)]

nullSubst :: Subst
nullSubst = []

(+->) :: Tyvar -> Type -> Subst
tyv +-> ty = [(tyv, ty)]

class Types t where
  apply :: Subst -> t -> t
  tv :: t -> [Tyvar]

instance Types Type where
  apply subs t@(TVar tyv) = maybe t identity $ P.lookup tyv subs
  apply subs (TAp t1 t2) = TAp (apply subs t1) (apply subs t2)
  apply _ t@(TCon _) = t
  apply _ t@(TGen _) = t

  tv (TVar tyv) = [tyv]
  tv (TAp t1 t2) = List.union (tv t1) (tv t2)
  tv (TGen _) = []
  tv (TCon _) = []

instance Types a => Types [a] where
  apply subs = fmap $ apply subs
  tv = List.nub . concatMap tv

infixr 4 @@

(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = (f <$> s2) <> s1 where
  f (u, t) = (u, apply s1 t)

--- todo: replace with his lame definition, just in case
-- his def doesn't rely on underlying structure of Subst
merge :: MonadError CheckerError m => Subst -> Subst -> m Subst
merge s1 s2 = if agree then return (s1 ++ s2) else throwError "merge fails"
  where
    agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v))
      (map fst s1 `List.intersect` map fst s2)

-- merge s1 s2 = case r of
--   [] -> return . List.nub $ s1 <> s2
--   ((tyv,(t1, t2)):_) -> throwError $ "TVar types disagree for "
--     <> show tyv <> " : " <> show t1 <> " : " <> show t2
--   where
--     r = do
--       (tv1, t1) <- s1
--       (tv2, t2) <- s2
--       guard $ tv1 == tv2 && t1 /= t2
--       return (tv1, (t1, t2))

mgu :: MonadError CheckerError m => Type -> Type -> m Subst
mgu (TAp l r) (TAp l' r') = do
  s1 <- mgu l l'
  s2 <- mgu (apply s1 r) (apply s1 r')
  return $ s1 @@ s2
mgu (TVar tv1) t = varBind tv1 t
mgu t (TVar tv2) = varBind tv2 t
mgu (TCon a) (TCon b)
  | a == b = return nullSubst
  | otherwise = throwError "mgu can't unify types"
mgu _ _ = throwError "mgu can't unify types"

varBind :: MonadError CheckerError m => Tyvar -> Type -> m Subst
varBind u t
  | t == (TVar u) = return nullSubst
  | u `elem` tv t = throwError "occurs check"
  | kind u /= kind t = throwError "different kinds"
  | otherwise = return $ u +-> t

match :: MonadError CheckerError m => Type -> Type -> m Subst
match (TVar u) t
  | kind u == kind t = return $ u +-> t
  | otherwise = throwError "kinds don't match"
match (TAp l r) (TAp l' r') = do
  s1 <- match l l'
  s2 <- match r r'
  merge s1 s2
match t1 t2
  | t1 == t2 = return nullSubst
  | otherwise = throwError "can't match types"


--------- type classes

data Qual t = [Pred] :=> t
  deriving (Eq, Show)

data Pred = IsIn Id Type
  deriving (Eq, Show)

instance Types t => Types (Qual t) where
  apply subs (preds :=> t) = apply subs preds :=> apply subs t
  tv (preds :=> t) = tv preds `List.union` tv t

instance Types Pred where
  apply subs (IsIn id' t) = IsIn id' (apply subs t)
  tv (IsIn _ t) = tv t

type Class = ([Id], [Inst])

type Inst = Qual Pred

data ClassEnv = ClassEnv { classes_ :: Map Id Class
  -- classes :: Id -> Maybe Class
                         
                         , defaults :: [Type]
                         } deriving Show
-- instance Show ClassEnv where
--   show _ = "ClassEnv"

classes :: ClassEnv -> Id -> Maybe Class
classes ce id = Map.lookup id $ classes_ ce

type EnvTransformer = ClassEnv -> Either CheckerError ClassEnv

infixr 5 <:>
(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(<:>) = (>=>)

initialEnv :: ClassEnv
initialEnv = ClassEnv { classes_ = Map.empty
                      , defaults = [tInteger, tDouble] }


mguPred :: MonadError CheckerError m => Pred -> Pred -> m Subst
mguPred = predIdsCheck mgu

matchPred :: MonadError CheckerError m => Pred -> Pred -> m Subst
matchPred = predIdsCheck match

predIdsCheck :: MonadError CheckerError m
             => (Type -> Type -> m Subst) -> Pred -> Pred -> m Subst
predIdsCheck f (IsIn id1 t1) (IsIn id2 t2)
  | id1 /= id2 = throwError "classes differ"
  | otherwise = f t1 t2

-- (Num a) => a -> Int

ff :: Qual Type
ff = [IsIn "Num" (TVar $ Tyvar "a" Star)]
     :=> ((TVar $ Tyvar "a" Star) `fn` tInt)

ord :: Class
ord = ( ["Eq"]
      , [ [] :=> IsIn "Ord" tInt
        , [] :=> IsIn "Ord" tUnit
        , [] :=> IsIn "Ord" tChar
        , [ IsIn "Ord" (TVar $ Tyvar "a" Star)
          , IsIn "Ord" (TVar $ Tyvar "b" Star)
          ] :=> IsIn "Ord" (pair (TVar $ Tyvar "a" Star) (TVar $ Tyvar "b" Star))
        ])

super :: MonadError CheckerError m => ClassEnv -> Id -> m [Id]
super ce = maybe (throwError "class not found") (return . fst) . classes ce

insts :: MonadError CheckerError m => ClassEnv -> Id -> m [Inst]
insts ce = maybe (throwError "class not found") (return . snd) . classes ce

instId :: Inst -> Id
instId (_ :=> (IsIn id' _)) = id'

defined :: Maybe a -> Bool
defined = isJust

modify :: ClassEnv -> Id -> Class -> ClassEnv
modify ce id' c = ce { classes_ = Map.insert id' c (classes_ ce) }
-- modify :: ClassEnv -> Id -> Class -> ClassEnv
-- modify ce id' c = ce { classes = \j -> if j == id' then
--                                          Just c
--                                        else
--                                          classes ce j }

addClass :: Id -> [Id] -> EnvTransformer
addClass id' supers ce
  | defined (classes ce id') = throwError "already defined"
  | any (not . defined . classes ce) supers = throwError $ "Error adding class " <> id' <> ": superclass not yet defined"
  | otherwise = return $ modify ce id' (supers, [])

addInst :: [Pred] -> Pred -> EnvTransformer
addInst qs p@(IsIn id' _) ce = case classes ce id' of
  Nothing -> throwError $ "Class " <> show id' <> " not defined."
  Just (supers, insts') -> case find (\(_ :=> q) -> overlap p q) insts' of
    Just _ -> throwError $ "Overlapping instances for " <> show id'
    Nothing -> return . modify ce id'
      $ (supers, (qs :=> p):insts')

overlap :: Pred -> Pred -> Bool
overlap p q = either (const False) (const True) $ mguPred p q

addPreludeClasses :: EnvTransformer
addPreludeClasses = addCoreClasses <:> addNumClasses

addCoreClasses :: EnvTransformer
addCoreClasses = addClass "Eq" [ ]
  <:> addClass "Ord" ["Eq"]
  <:> addClass "Show" [ ]
  <:> addClass "Read" [ ]
  <:> addClass "Bounded" [ ]
  <:> addClass "Enum" [ ]
  <:> addClass "Functor" [ ]
  <:> addClass "Monad" [ ]

addNumClasses :: EnvTransformer
addNumClasses = addClass "Num" ["Eq", "Show"]
  <:> addClass "Real" ["Num", "Ord"]
  <:> addClass "Fractional" ["Num"]
  <:> addClass "Integral" ["Real", "Enum"]
  <:> addClass "RealFrac" ["Real", "Fractional"]
  <:> addClass "Floating" ["Fractional"]
  <:> addClass "RealFloat" ["RealFrac", "Floating"]

exampleInsts :: EnvTransformer
exampleInsts = addInst [] (IsIn "Ord" tUnit)
  <:> addInst [] (IsIn "Ord" tChar)
  <:> addInst [] (IsIn "Ord" tInt)
  <:> addInst [ IsIn "Ord" (TVar (Tyvar "a" Star ))
              , IsIn "Ord" (TVar (Tyvar "b" Star ))]
              (IsIn "Ord" (pair (TVar (Tyvar "a" Star))
                                (TVar (Tyvar "b" Star ))))

bySuper :: MonadError CheckerError m => ClassEnv -> Pred -> m [Pred]
bySuper ce p@(IsIn id t) = do
  preds <- fmap (flip IsIn t) <$> super ce id
  sups <- concat <$> traverse (bySuper ce) preds
  return $ p:sups


--TODO make sure it works
byInst :: MonadError CheckerError m => ClassEnv -> Pred -> m [Pred]
byInst ce p@(IsIn classId _) = do
  xs <- insts ce classId
  case headMay . rights $ (\(ps:=>p') -> (ps,) <$> matchPred p p') <$> xs of
    Nothing -> throwError $ "No instance for " <> show p
    Just (qualPreds, sub)-> return $ apply sub <$> qualPreds
  

entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = undefined
