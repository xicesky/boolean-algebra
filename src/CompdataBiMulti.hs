
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-} 

-- Extensions for compdata usage
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

-- Mu
{- # LANGUAGE PolyKinds              #-}
{-# LANGUAGE KindSignatures         #-}
{- # LANGUAGE UndecidableInstances   #-}
{- # LANGUAGE GADTs                  #-}

module CompdataBiMulti where

import Data.Comp.Multi.Term
import Data.Comp.Multi.Ops
import Data.Comp.Multi.Sum (inject, split)
import Data.Comp.Multi.Algebra
    (   Alg, Coalg, RAlg, RCoalg
    ,   cata, ana, para, apo
    )

import Data.Comp.Multi.Derive
--import Data.Comp.Derive.Show
import Data.Comp.Multi.Show ()            -- for the Show instance
import Data.Comp.Multi.Equality ()        -- for the Eq instance

data BooleanValue (e :: * -> *) v = BooleanValue Bool
    deriving (Show, Functor)

data BooleanVariable (e :: * -> *) v = BVariable v
    deriving Functor

data BooleanAnd (e :: * -> *) v = BAnd (e v) (e v)
    deriving Functor

-- data BooleanValue (e :: * -> *) v where
--     BooleanValue :: Bool -> BooleanValue e v

-- data BooleanVariable (e :: * -> *) v where
--     BooleanVariable :: v -> BooleanVariable e v

-- data BooleanAnd (e :: * -> *) v where
--     BAnd :: e v -> e v -> BooleanAnd e v

{- FIXME: This just fails on every conceivable level...
    If we use Data.Comp.*, subsumption checks contain ambiguos variables.
    If we use Data.Comp.Multi.*, show and equality fails for variables, because
    the library makes the assumption that the second parameter is just an INDEX
    type, but never actually used.
-}

$(derive
    [ makeHFunctor
    , makeHFoldable
    , makeHTraversable
    --, makeShowHF
    --, makeEqHF
    ]
    [''BooleanValue
    ,''BooleanVariable
    -- ,''BooleanNot
    ,''BooleanAnd
    -- ,''BooleanOr
    ])

-- Base functor (called "Signature" in compdata)
type BooleanBaseF
    = BooleanValue
    :+: BooleanVariable
    -- :+: BooleanNot
    :+: BooleanAnd
    -- :+: BooleanOr

-- "Standard" boolean expressions
-- use ⊤, ⊥, ¬, ∧, ∨ and variables
--type BooleanExpr v = Term (BooleanBaseF v)
