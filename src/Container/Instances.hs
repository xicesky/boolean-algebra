
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

module Container.Instances where

import              Prelude         (Bool (..), Eq (..), Int, Ord (..), String
                                    , error, flip, fst, head, length, snd, tail
                                    , undefined, ($), (++), (.), (==)
                                    )
import qualified    Prelude

import              Data.Foldable   hiding (toList)
import              Data.Maybe
import              Data.Monoid
import              Data.Map        as DataMap

-- TODO: Temporary. Remove
import              Prelude         (Functor(..), Applicative(..), Monad(..), (+))
import              Control.Applicative (Alternative(..))
import              Data.List       (findIndex)

import Container.Definition
import Container.Indexed.Definition

{-----------------------------------------------------------------------------}
-- List

instance ElementContainer [v] where
    type ElementT [v] = (Int, v)    -- FIXME: Natural

    -- foldMapElement :: forall m. Monoid m => ((Int, v) -> m) -> [v] -> m
    -- foldMapElement p = foldWithIndex 0 where
    --     foldWithIndex :: Int -> [v] -> m
    --     foldWithIndex _ []      = mempty
    --     foldWithIndex i (x:xs)  = p (i,x) <> foldWithIndex (i+1) xs

instance IndexedContainer [v] where
    type IndexT [v] = Int           -- FIXME: Natural
    type ValueT [v] = v

    -- foldMapIV :: Monoid m => (Int -> v -> m) -> [v] -> m
    -- foldMapIV = foldMapElement . Prelude.uncurry

{-----------------------------------------------------------------------------}
-- Data.Map

instance ElementContainer (DataMap.Map k v) where
    type ElementT (DataMap.Map k v) = (k, v)

instance IndexedContainer (DataMap.Map k v) where
    type IndexT (DataMap.Map k v) = k
    type ValueT (DataMap.Map k v) = v
