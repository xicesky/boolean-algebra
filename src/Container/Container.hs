
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-}

-- Type magic
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Container.Container
    ( module Container.Definition
    , module Container.Indexed.Definition
    , module Container.Instances
    ) where

{-
    This module tries to present a consistent interface for containers, especially considering
    operators (e.g. (!), (!!)) which often clash for different container types.
    Special consideration is given to indexed containers and the following packages:
        https://hackage.haskell.org/package/containers
        https://hackage.haskell.org/package/unordered-containers
        https://hackage.haskell.org/package/array
    
    So that code can - for example - be written for a generic "Map" type and work with
    either TreeMap or HashMap.

    Some functions from the prelude are redefined, so best import it like this:
        import Prelude hiding (lookup, (!!))
        import Sky.Util.NewContainer
-}

import Container.Definition
import Container.Indexed.Definition
import Container.Instances

{-----------------------------------------------------------------------------}
-- Demo for odd instances: Pair

newtype Pair a = Pair (a, a)
    deriving (Show, Eq, Ord, Functor)

-- Pair is NOT a semigroup
-- Pair is NOT a monoid

-- Pair is NOT Applicative
-- Pair is NOT Alternative
-- Pair is NOT Monad

instance Foldable Pair where
    foldMap f (Pair (a, b)) = f a <> f b

-- But...
instance ElementContainer (Pair v) where
    type ElementT (Pair v) = (Bool, v)

    -- traverseElement :: Applicative f => (ElementT c -> f (ElementT c)) -> c -> f c
    traverseElement :: Applicative f => ((Bool, v) -> f (Bool, v)) -> Pair v -> f (Pair v)
    traverseElement p (Pair (a, b)) = mk <$> p (True, a) <*> p (False, a) where
        mk (True, a) (False, b) = Pair (a, b)

    foldMapElement :: forall m. Monoid m => ((Bool, v) -> m) -> Pair v -> m
    foldMapElement p (Pair (a, b)) = p (True, a) <> p (False, a)

instance IndexedContainer (Pair v) where
    type IndexT (Pair v) = Bool
    type ValueT (Pair v) = v

    foldMapIV :: Monoid m => (Bool -> v -> m) -> Pair v -> m
    foldMapIV = foldMapElement . Prelude.uncurry
