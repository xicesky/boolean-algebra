
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-}

-- Type magic
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Container.Indexed.Definition where

import Data.Monoid

import Container.Definition


class (Ord (IndexT c), ElementContainer c) => IndexedContainer c where
    type IndexT c :: *
    type ValueT c :: *
    -- Usually ElementT c = (IndexT c, ValueT c)

    -- Foldable
    foldMapIV :: Monoid m => (IndexT c -> ValueT c -> m) -> c -> m

    foldMapValue :: Monoid m => (ValueT c -> m) -> c -> m
    foldMapValue p = foldMapIV (Prelude.const p)

    -- findValue :: (ValueT c -> Bool) -> c -> Bool

    -- What you usually want
    lookup :: IndexT c -> c -> Maybe (ValueT c)
    lookup i = getFirst . foldMapIV match where
        match :: IndexT c -> ValueT c -> First (ValueT c)
        match j v | i == j  = First (Just v)
        match _ _           = mempty
    --find :: ValueT c -> c -> Maybe (IndexT c)
