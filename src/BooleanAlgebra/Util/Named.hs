
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections          #-}

-- | Associating names with variables the hard way
module BooleanAlgebra.Util.Named where

import Prelude hiding ((!!), lookup)
import Data.Kind (Type)
import Data.Tuple (swap)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Const (Const(..))
--import Control.Monad.Reader
import Control.Monad.State.Lazy

import Container
import Data.Hashable
import Data.List (elemIndex)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

{-# ANN module "HLint: ignore Use camelCase" #-}

{-----------------------------------------------------------------------------}

class (Eq a, Ord a, Hashable a, Monoid a) => Name a where
    prefixList :: [a]
    mkSuffix :: Int -> a

instance Name String where
    prefixList = gen where
        gen = flip (:) <$> "" : gen <*> ['z'..'a']
    mkSuffix = show

dropOldNames :: Name a => HashSet a -> [a] -> [a]
dropOldNames taken = dropWhile (`HashSet.member` taken)

{-----------------------------------------------------------------------------}

-- | Structures with nameable items (which can just be Int)
class (Eq n, Ord n, Hashable n) => Nameable t n | t -> n
    where

    {- | Traverse all items ("variables") that are associated with names.

    This is a valid "Traversal", see Control.Monad.Lens
    -}
    traverseNamed :: forall f. Applicative f => (n -> f n) -> t -> f t

    foldNamed :: forall m. Monoid m => (n -> m) -> t -> m
    foldNamed f = getConst . traverseNamed (Const . f)

    mapNamed :: (n -> n) -> t -> t
    mapNamed f = runIdentity . traverseNamed (Identity . f)

-- | Get the set of all items (free variables)
nameableSet :: Nameable t n => t -> HashSet n
nameableSet = foldNamed csingleton

{-----------------------------------------------------------------------------}

-- FIXME: Doesn't have to be String now!

-- | A list of variable names.
-- Used to map from integer variable indices to variable names
type NameMap = [String]

-- | A term @t@ with attached variable names
type WithNames t = (NameMap, t)

instance Nameable t Int => Nameable (WithNames t) String where
    traverseNamed :: forall f. Applicative f => (String -> f String) -> WithNames t -> f (WithNames t)
    traverseNamed f (nm, t) = (nm,) <$> traverseNamed f' t where
        f' :: Int -> f Int
        f' i = let
            rebind :: String -> Int
            rebind n = case elemIndex n nm of
                Just i -> i
                Nothing -> error "meh"      -- FIXME: This is not ok, we can't rename stuff
            in rebind <$> f (nm !! i)

{-----------------------------------------------------------------------------}
-- Monad for naming variables

-- | Monad that resolves and provides fresh names
class (Name n, Monad m) => MonadName n m | m -> n
    where
    -- | Resolve a name by index
    askName :: Int -> m n

    -- | Create a fresh name
    freshName :: m n

-- | NameT monad transformer for creating monads that provide MonadName
newtype NameT n m a = NameT { unNameT :: StateT (FreshState n) m a }
    deriving (Functor, Applicative, Monad, MonadFail)

{-----------------------------------------------------------------------------}

data FreshState n = FreshState
    { fs_index :: [n]
    , fs_taken :: HashSet n
    , fs_fresh :: [n]
    }

fs_mk :: (Name n, Nameable t n) => t -> FreshState n
fs_mk t = FreshState
    {   fs_index = HashSet.toList ns
    ,   fs_taken = ns
    ,   fs_fresh = prefixList
    }   where
        ns = nameableSet t

fs_freshName :: Name n => FreshState n -> (n, FreshState n)
fs_freshName fs@FreshState { fs_taken = taken, fs_fresh = fresh } = let
    (n : ns) = dropOldNames taken fresh
    taken' = cinsert n taken
    in (n, fs { fs_taken = taken', fs_fresh = ns })

instance (Name n, Monad m) => MonadName n (NameT n m) where
    askName i = NameT $ do
        FreshState { fs_index = resolver } <- get
        return $ resolver !! i  -- FIXME
    freshName = NameT $ state fs_freshName

instance MonadTrans (NameT n) where
    lift   = liftNameT
    {-# INLINE lift #-}

-- "Passthrough": Doesn't affect another state monad
instance MonadState s m => MonadState s (NameT n m) where
    get     = lift get
    put     = lift . put
    state   = lift . state

-- fs_mk :: (Name n, Nameable t n) => t -> FreshState n

evalNameT :: forall m n t a. (Monad m, Name n, Nameable t n) => t -> NameT n m a -> m a
evalNameT t nt = evalStateT (unNameT nt) (fs_mk t)

evalNameT' :: forall m n t a. (Name n, Nameable t n) => t -> NameT n Identity a -> a
evalNameT' = (runIdentity.) . evalNameT

withNames :: forall m n t a. (Monad m, Nameable t Int)
    => WithNames t -> (t -> NameT n m a) -> m (WithNames a)
withNames (nm, t) f = do
    (a', s') <- runStateT (unNameT (f t)) freshState
    return (fs_index s', a')
    where
        freshState :: FreshState String
        freshState = FreshState
            {   fs_index = nm
            ,   fs_taken = HashSet.fromList nm
            ,   fs_fresh = prefixList
            }
-- mapNameT :: forall m m' n a b. (m a -> m' b) -> NameT n m a -> NameT n m' b
-- mapNameT f m = NameT $ mapStateT f' (unNameT m) where
--     f' :: m (a, FreshState n) -> m' (b, FreshState n)
--     f' arg = _
-- {-# INLINE mapNameT #-}

liftNameT :: Monad m => m a -> NameT n m a
liftNameT m = NameT $ lift m
{-# INLINE liftNameT #-}

-- withNames :: (f -> NameT n m a) -> WithNames f -> m a
-- withNames f (nm, t) = runNameT (f t) (nm !!)

-- withoutNames :: NameT n m a -> m a
-- withoutNames a = runNameT a show

{-----------------------------------------------------------------------------}


{- | Build a map to re-number indices to avoid clashes.

If the same name maps to different numbers in @nma@ and @nmb@, those are "re-indexed"
to map to the same number. If two different names map to the same number, the number
is re-indexed in the right term.
-}
-- clashMap' :: [(Int, String)] -> NameMap -> HashMap Int Int
-- clashMap' nma nmb = foldMap go nma where
--     -- Reverse map of nmb
--     reverse :: HashMap String Int
--     reverse = (fromList . fmap swap . listElements) nmb

--     -- FIXME: What it both happens?
--     go :: (Int, String) -> HashMap Int Int
--     go (i, name) = (case lookup i nmb of
--             Just name2  | name /= name2 ->          -- same number, different name
--                 csingleton (i, freshNum)            -- !! argh
--             _ -> cempty                             -- No clash
--         ) `mappend` (case lookup name reverse of
--             Just i2     | i /= i2       ->          -- same name, different number
--                 csingleton (i2, i)
--             _ -> cempty
--         )

{- | Resolve number clashes
-}
-- mergeNames :: Nameable t => NameMap -> NameMap -> t -> t -> (NameMap, t, t)
-- mergeNames nma nmb ta tb = undefined
