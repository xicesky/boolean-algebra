
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- FIXME: Most of this is not specific to BooleanAlgebra, move to Term
module BooleanAlgebra.Transform.Variable
    (   HasNames(..)
    ,   MappedNames(..)
    ,   Context(..)
    ,   getNameMap
    ,   getIndexMap
    ,   buildContext
    ,   destroyContext
    ,   findFreshName
    ,   FreshState
    ,   MonadFresh(..)
    ,   FreshT
    ,   runFreshT
    ) where

import Data.Kind (Type)

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- mtl / transformers
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import Term.Term

import BooleanAlgebra.Base.Expression

{-----------------------------------------------------------------------------}
-- Simple tools

class HasNames t where
    type NameT t :: Type
    variableNames :: t -> Set (NameT t)

instance Ord name => HasNames (Term op val name) where
    type NameT (Term op val name) = name
    variableNames :: Term op val name -> Set name
    variableNames = foldMap Set.singleton

instance Ord name => HasNames (TermLit op val name) where
    type NameT (TermLit op val name) = name
    variableNames :: TermLit op val name -> Set name
    variableNames = foldMap Set.singleton

instance Ord name => HasNames (CNF name) where
    type NameT (CNF name) = name
    variableNames :: CNF name -> Set name
    variableNames = foldMap Set.singleton

{-----------------------------------------------------------------------------}
-- Handling mapped variables

type MappedNames name = (Map Int name, Map name Int)

{- The variables numbers HAVE TO start with 1, because we will
    use signs in CNF to indicate negation
-}
mkMappedNames :: Ord name => Set name -> MappedNames name
mkMappedNames nSet = let
    nList = Set.toList nSet
    in (Map.fromList $ zip [1..] nList, Map.fromList $ zip nList [1..])

mappedHasName :: Ord name => MappedNames name -> name -> Bool
mappedHasName (_, m) name = Map.member name m

mappedInsert :: (Ord name, Show name) => Int -> name -> MappedNames name -> MappedNames name
mappedInsert i name (iton, ntoi)
    | Map.member i iton     = error $ "Index " ++ show i ++ " already exists"
    | Map.member name ntoi  = error $ "Name " ++ show name ++ " already exists"
    | otherwise             = (Map.insert i name iton, Map.insert name i ntoi)

data Context f name = Context
    {   getMappedNames :: MappedNames name
    ,   getTerm :: f Int
    }

getNameMap :: Context f name -> Map Int name
getNameMap = fst . getMappedNames

getIndexMap :: Context f name ->  Map name Int
getIndexMap = snd . getMappedNames

deriving instance (Show (f Int), Show name) => Show (Context f name)
deriving instance (Eq (f Int), Eq name) => Eq (Context f name)

-- FIXME: Wrong for: Term op val (Bool, String)
-- It would build a name map from LITERALS to Ints
-- Demo:
-- >>> buildContext $ simplify demo1b
buildContext :: forall f name. (Traversable f, Ord name)
    => f name -> Context f name
buildContext t = let
    mappedNames :: MappedNames name
    mappedNames = mkMappedNames (foldMap Set.singleton t)
    indexNames :: name -> Int
    indexNames = (Map.!) (snd mappedNames)
    in Context mappedNames (fmap indexNames t)

destroyContext :: forall f name. (Traversable f, Ord name)
    => Context f name -> f name
destroyContext (Context mappedNames t) = let
    remapNames :: Int -> name
    remapNames = (Map.!) (fst mappedNames)
    in fmap remapNames t

-- TODO: Could be much more efficient than rebuilding the whole context
cRenameVars :: (Traversable f, Ord a, Ord b) => (a -> b) -> Context f a -> Context f b
cRenameVars rename = buildContext . fmap rename . destroyContext

{-----------------------------------------------------------------------------}

findFreshName :: MappedNames String -> String -> String
findFreshName m prefix = head $ dropWhile exists' varnames where
    suffixes :: [String]
    suffixes = [replicate k ['a'..'z'] | k <- [1..]] >>= sequence
    --suffixes = "" :([replicate k ['a'..'z'] | k <- [1..]] >>= sequence)
    varnames :: [String]
    varnames = (prefix++) <$> suffixes
    exists' :: String -> Bool
    exists' = mappedHasName m

{-----------------------------------------------------------------------------}
-- Monad for fresh names

-- Not "splitable" for now
data FreshState = FreshState
    {   fsNextIndex :: Int
    ,   fsPrefix :: String
    ,   fsNames :: MappedNames String
    }
    deriving (Show, Eq)

fsMkFreshName :: String -> FreshState -> (Int, FreshState)
fsMkFreshName prefix (FreshState index p names) = let
    name :: String
    name = findFreshName names prefix
    in (index, FreshState (index+1) p (mappedInsert index name names))

newtype FreshT m a = FreshT { toStateT :: StateT FreshState m a }
    --deriving (Functor, Monad, MonadTrans, MonadIO)

deriving instance Functor m => Functor (FreshT m)
deriving instance Monad m => Applicative (FreshT m)
deriving instance Monad m => Monad (FreshT m)
deriving instance MonadTrans FreshT

deriving instance MonadIO m => MonadIO (FreshT m)
-- ... and so on... implement as we need it

class MonadFresh m where
    freshName :: String -> m Int

instance Monad m => MonadFresh (FreshT m) where
    freshName prefix = FreshT $ do
        state (fsMkFreshName prefix)

-- FIXME: get some proper names for those functions, these here are horrible

runFreshT :: Monad m => FreshT m a -> MappedNames String -> m (a, MappedNames String)
runFreshT fresh mapped = let
    maxI :: Int
    maxI = maximum $ 0 : Map.keys (fst mapped)
    initState :: FreshState
    initState = FreshState (maxI+1) "" mapped
    in do
        (a, FreshState _ _ mapped') <- runStateT (toStateT fresh) initState
        return (a, mapped')

-- FIXME: Useless, works only on terms, not on CNF and literals
withFreshT :: forall m op val. Monad m
    => Context (Term op val) String
    -> (Term op val Int -> FreshT m (Term op val Int))
    -> m (Context (Term op val) String)
withFreshT (Context mapped term) f = let
    result :: m (Term op val Int, MappedNames String)
    result = runFreshT (f term) mapped
    in do
        (term', mapped') <- result
        return $ Context mapped' term'

-- FIXME: Useless, works only on terms, not on CNF and literals
withFresh :: Context (Term op val) String
    -> (Term op val Int -> FreshT Identity (Term op val Int))
    -> Context (Term op val) String
withFresh ctx f = runIdentity (withFreshT ctx f)

-- FIXME: Doesn't work on literals
newFresh :: FreshT Identity (Term op val Int) -> Context (Term op val) String
newFresh f = runIdentity $ do
    (term, mapped) <- runFreshT f (mkMappedNames Set.empty)
    return $ Context mapped term

{-----------------------------------------------------------------------------}
-- Idea: compare terms for α-Equivalence
