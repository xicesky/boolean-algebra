
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Description     : Fresh name monad
Stability       : experimental

A monad (monad tranformer actually) and utilities to create
fresh variable names "on-the-fly" when building terms.
-}
module Term.Variable.FreshT
    (   -- * FreshT
        MonadFresh(..)
    ,   FreshT(..)
    ,   runFreshT

    ,   -- * Implementation details
        FreshState(..)
    ,   findFreshName
    ) where

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
import Term.Variable.Names
import Term.Variable.Context

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
