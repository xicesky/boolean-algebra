
{- |
Description     : Bijective map
Stability       : experimental

Implements 'Bimap', a bijective map that has decently fast lookup
in both directions (key to value, value to key) and maintains the
invariant that those mappings are unique.

The interface is (supposed to be) similar to "Data.Map" from the
containers package.
-}
module Missing.Bimap where

import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)
import Data.Functor.Const (Const(..))
import Data.Monoid (All(..))

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

{-----------------------------------------------------------------------------}

{- I don't want to add another unmaintained library or give
a very specialized implementation, so we are going to do something
simple but stupid, and pack two Maps together

TODO: The same thing but using a HashMap.

-}

data Bimap a b = Bimap { biTo :: !(Map a b), biFrom :: !(Map b a) }
    deriving (Show, Eq, Ord)

swapMap :: Bimap a b -> Bimap b a
swapMap (Bimap l r) = Bimap r l

{- | Check the central invariant of bimap

Useful for testing.
-}
isValid :: forall a b. (Ord a, Ord b) => Bimap a b -> Bool
isValid (Bimap mab mba) = let
    mabValueIsKey :: forall a b. (Ord a, Ord b) => Map a b -> Map b a -> Bool
    mabValueIsKey mab mba = getAll $ getConst $ Map.traverseWithKey chk0 mab where
        -- mba maps the other way
        chk0 :: a -> b -> Const All b
        chk0 va vb = Const $ All $ case Map.lookup vb mba of
            Nothing -> False
            Just v  -> v == va
    in mabValueIsKey mab mba && mabValueIsKey mba mab

{-----------------------------------------------------------------------------}

empty :: Bimap a b
empty = Bimap Map.empty Map.empty

singleton :: a -> b -> Bimap a b
singleton a b = Bimap (Map.singleton a b) (Map.singleton b a)

lookup :: Ord k => k -> Bimap k v -> Maybe v
lookup k (Bimap mkv _) = Map.lookup k mkv

lookupR :: Ord k => k -> Bimap v k -> Maybe v
lookupR k (Bimap _ mkv) = Map.lookup k mkv

lookupUnsafe :: Ord k => k -> Bimap k v -> v
lookupUnsafe k m = fromMaybe (error "lookupUnsafe failed") $
    lookup k m

lookupUnsafeR :: Ord k => k -> Bimap v k -> v
lookupUnsafeR k m = fromMaybe (error "lookupUnsafeR failed") $
    lookupR k m

member :: Ord k => k -> Bimap k a -> Bool
member k (Bimap mkv _) = Map.member k mkv

memberR :: Ord k => k -> Bimap a k -> Bool
memberR k (Bimap _ mkv) = Map.member k mkv

notMember :: Ord k => k -> Bimap k a -> Bool
notMember k (Bimap mkv _) = Map.notMember k mkv

notMemberR :: Ord k => k -> Bimap a k -> Bool
notMemberR k (Bimap _ mkv) = Map.notMember k mkv

-- internal
{-# INLINE liftMap #-}
liftMap :: (Map k v -> a) -> Bimap k v -> a
liftMap f (Bimap mkv _) = f mkv

null :: Bimap a b -> Bool
null = liftMap Map.null

size :: Bimap a b -> Int
size = liftMap Map.size

elems :: Bimap a b -> [b]
elems = liftMap Map.elems

keys :: Bimap a b -> [a]
keys = liftMap Map.keys

keysSet :: Bimap a b -> Set a
keysSet = liftMap Map.keysSet

toList :: Bimap a b -> [(a,b)]
toList = liftMap Map.toList

toMap :: Bimap a b -> Map a b
toMap = liftMap id

toMapR :: Bimap a b -> Map b a
toMapR = liftMap id . swapMap

{-----------------------------------------------------------------------------}

data BiMapError = KeyExistsError | ValueExistsError

{-|

Note: This function will /fail/ by returning a 'BiMapError' if either the key
or the value alread exist.
-}
insert :: (Ord a, Ord b) => a -> b -> Bimap a b -> Either BiMapError (Bimap a b)
insert va vb (Bimap mab mba) = let
    kb = Map.lookup va mab
    ka = Map.lookup vb mba
    in case (ka, kb) of
        (Nothing, Nothing)  -> Right $ Bimap (Map.insert va vb mab) (Map.insert vb va mba)
        (Just _,  _      )  -> Left KeyExistsError
        (_,       Just _ )  -> Left ValueExistsError

{-|

Note: This function will /fail/ by returning 'Nothing' if either the key
or the value alread exist.
-}
insertMaybe :: (Ord a, Ord b) => a -> b -> Bimap a b -> Maybe (Bimap a b)
insertMaybe va vb bm = case insert va vb bm of
    Left _  -> Nothing
    Right m -> Just m

insertUnsafe :: (Show a, Show b, Ord a, Ord b) => a -> b -> Bimap a b -> Bimap a b
insertUnsafe va vb bm = case insert va vb bm of
    Left KeyExistsError     -> error $ "Index " ++ show va ++ " already exists"
    Left ValueExistsError   -> error $ "Value " ++ show vb ++ " already exists"
    Right m                 -> m

delete :: (Ord a, Ord b) => a -> Bimap a b -> Bimap a b
delete va m@(Bimap mab mba) = case Map.lookup va mab of
    Just vb     -> Bimap (Map.delete va mab) (Map.delete vb mba)
    Nothing     -> m

deleteR :: (Ord a, Ord b) => b -> Bimap a b -> Bimap a b
deleteR vb = swapMap . delete vb . swapMap

{-----------------------------------------------------------------------------}
{- TODO: NYI

filter :: (a -> Bool) -> Map k a -> Map k a
filterWithKey :: (k -> a -> Bool) -> Map k a -> Map k a

-- Something like 'alter' but we need to signal failure in some way
alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alterF :: (Functor f, Ord k) => (Maybe a -> f (Maybe a)) -> k -> Map k a -> f (Map k a)

-}

{-----------------------------------------------------------------------------}
-- Other utilities

{- | Build a Bimap for numbering a set of names.

Each name is assigned a unique @Int@ from the given list (if you don't supply
duplicates).
-}
setToIndexMap :: Ord k => [Int] -> Set k -> Bimap Int k
setToIndexMap indices names = let
    nList = Set.toList names
    in Bimap (Map.fromList $ zip indices nList) (Map.fromList $ zip nList indices)
