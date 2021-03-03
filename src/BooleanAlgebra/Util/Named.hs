
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Associating names with variables the hard way
module BooleanAlgebra.Util.Named where

import Prelude hiding ((!!), lookup)
import Data.Tuple (swap)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Const (Const(..))
import Control.Monad.Reader

import Container
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

{-----------------------------------------------------------------------------}

class (Eq a, Ord a, Hashable a, Monoid a) => Name a where
    prefixList :: [a]
    mkSuffix :: Int -> a

instance Name String where
    prefixList = gen where
        gen = flip (:) <$> "" : gen <*> ['z'..'a']
    mkSuffix = show

freshNames :: Name a => HashSet a -> [a] -> [a]
freshNames taken = dropWhile (`HashSet.member` taken)

type NameMap = [String]

{-----------------------------------------------------------------------------}

-- FIXME: Doesn't have to be String now!

-- | A map from integer variable indices to variable names
-- type NameMap = HashMap Int String

-- | A term @t@ with attached variable names
type WithNames t = (NameMap, t)

{-----------------------------------------------------------------------------}
-- Monad for naming variables

-- TODO make it a state monad so we can rename on the fly?

type Resolver = Int -> String

class Monad m => MonadName m where
    askName :: Int -> m String
    -- local :: (Resolver -> Resolver) -> m a -> m a

newtype NameT m a = NameT { unNameT :: ReaderT Resolver m a }
    deriving (Functor, Applicative, Monad, MonadFail)

instance Monad m => MonadName (NameT m) where
    askName i = NameT $ do
        resolver <- ask
        return $ resolver i

instance MonadTrans NameT where
    lift   = liftNameT
    {-# INLINE lift #-}

-- "Passthrough"
instance MonadReader r m => MonadReader r (NameT m) where
    ask   = lift ask
    local = mapNameT . local
    reader = lift . reader

runNameT :: forall m a. NameT m a -> Resolver -> m a
runNameT = runReaderT . unNameT

mapNameT :: (m a -> n b) -> NameT m a -> NameT n b
mapNameT f m = NameT $ mapReaderT f (unNameT m)
{-# INLINE mapNameT #-}

liftNameT :: m a -> NameT m a
liftNameT m = NameT $ ReaderT (const m)
{-# INLINE liftNameT #-}

withNames :: (f -> NameT m a) -> WithNames f -> m a
withNames f (nm, t) = runNameT (f t) (nm !!)

withoutNames :: NameT m a -> m a
withoutNames a = runNameT a show

{-----------------------------------------------------------------------------}

-- FIME: Direly needed
-- A monad to generate fresh names

{-----------------------------------------------------------------------------}

-- | Structures with nameable items
class Nameable t where
    {- | Traverse all items ("variables") that are associated with names.

    This is a valid "Traversal", see Control.Monad.Lens
    -}
    traverseNamed :: forall f. Applicative f => (Int -> f Int) -> t -> f t

    foldNamed :: forall m. Monoid m => (Int -> m) -> t -> m
    foldNamed f = getConst . traverseNamed (Const . f)

    mapNamed :: (Int -> Int) -> t -> t
    mapNamed f = runIdentity . traverseNamed (Identity . f)

-- | Get the set of all items (free variables)
nameableSet :: Nameable t => t -> HashSet Int
nameableSet = foldNamed csingleton

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
mergeNames :: Nameable t => NameMap -> NameMap -> t -> t -> (NameMap, t, t)
mergeNames nma nmb ta tb = undefined
