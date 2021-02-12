
{-  Some structures we consider to be containers do not
    generalize to arbitrary value types (like Functors do).
    The classes implemented here intend to reflect the
    "container" property, without requiring your container
    to be able to store any value.

-}
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-}

-- Type magic
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Container.Definition where

import              Prelude         (Bool (..), ($), (.), const
                                    )
import qualified    Prelude

import Data.Functor
import Control.Applicative ( Applicative(..) )

import Data.Functor.Identity
import Data.Functor.Const
import Data.Monoid
import Data.Foldable
import Data.Traversable
import Numeric.Natural

-- -- TODO: Temporary. Remove
-- import              Prelude         (Functor(..), Applicative(..), Monad(..), (+))
-- import              Control.Applicative (Alternative(..))
-- import              Data.List       (findIndex)

{- Required laws:
    Laws of traverse apply analogously to traverseElement.
    mapElement id = id

    If your container is a parameterized type f :: * -> *, with
        ElementT (f a) = a
    it should implement FunctorialContainer.
-}
class ElementContainer c where
    {- ElemT is not the value type, but the type a container would "toList" to.
    E.g.: In a map "k -> v" this is (k,v).
    -}
    type ElementT c :: *

    -- Traverseable
    -- FIXME: Remove - reordering keys doesn't make sense here
    traverseElement :: Applicative f => (ElementT c -> f (ElementT c)) -> c -> f c

    -- Foldable
    foldMapElement :: Monoid m => (ElementT c -> m) -> c -> m
    foldMapElement f = getConst . traverseElement (Const . f)

    -- Functor
    -- mapElement :: (ElementT c -> ElementT c) -> c -> c
    -- mapElement f = runIdentity . traverseElement (Identity . f)

    -- And all the other nice things we get for free --

    collapseElementMonoid :: Monoid m => m -> c -> m
    collapseElementMonoid = foldMapElement . const

    size :: c -> Natural
    size = getSum . collapseElementMonoid (Sum 1)

    elementList :: c -> [ElementT c]
    elementList = foldMapElement pure

    isEmpty :: c -> Bool
    isEmpty = getAny . collapseElementMonoid (Any True)

    findElement :: (ElementT c -> Bool) -> c -> Bool
    findElement p = getAny . foldMapElement (Any . p)

    {-# MINIMAL traverseElement #-}

{- Required laws:
    mapElement = fmap
    foldMapElement = foldMap
    traverseElement = traverse

-}
class
    ( Functor (BaseT c)
    , Foldable (BaseT c)
    , Traversable (BaseT c)
    , ElementContainer c
    )
    => FunctorialContainer c where
    type BaseT c :: * -> *
