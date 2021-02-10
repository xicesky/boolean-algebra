
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-} 

-- Extensions for compdata usage
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module CompdataBi where

import Data.Comp.Term
import Data.Comp.Ops
import Data.Comp.Sum (inject, split)
import Data.Comp.Algebra
    (   Alg, Coalg, RAlg, RCoalg
    ,   cata, ana, para, apo
    )

import Data.Comp.Derive
--import Data.Comp.Derive.Show
import Data.Comp.Show ()            -- for the Show instance
import Data.Comp.Equality ()        -- for the Eq instance

data BooleanValue v e = BooleanValue Bool
    deriving (Show, Functor)

data BooleanVariable v e = BVariable v
    deriving Functor

data BooleanAnd v e = BAnd e e
    deriving Functor

{-
    • Could not deduce (Subsume
                          (compdata-0.12.1:Data.Comp.SubsumeCommon.ComprEmb
                             (Elem (BooleanAnd v0) f))
                          (BooleanAnd v0)
                          f)
      from the context: Subsume
                          (compdata-0.12.1:Data.Comp.SubsumeCommon.ComprEmb
                             (Elem (BooleanAnd v) f))
                          (BooleanAnd v)
                          f
        bound by the inferred type for ‘iBAnd’:
                   forall v (f :: * -> *) h a.
                   Subsume
                     (compdata-0.12.1:Data.Comp.SubsumeCommon.ComprEmb
                        (Elem (BooleanAnd v) f))
                     (BooleanAnd v)
                     f =>
                   Cxt h f a -> Cxt h f a -> Cxt h f a
        at src/CompdataBi.hs:(37,3)-(49,5)
      The type variable ‘v0’ is ambiguous
    • In the ambiguity check for the inferred type for ‘iBAnd’
      To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
      When checking the inferred type
        iBAnd :: forall v (f :: * -> *) h a.
                 Subsume
                   (compdata-0.12.1:Data.Comp.SubsumeCommon.ComprEmb
                      (Elem (BooleanAnd v) f))
                   (BooleanAnd v)
                   f =>
                 Cxt h f a -> Cxt h f a -> Cxt h f a
-}

$(derive
    [ makeTraversable
    , makeFoldable
    , makeEqF
    , makeShowF
    --, smartConstructors
    ]
    [''BooleanValue
    ,''BooleanVariable
    -- ,''BooleanNot
    ,''BooleanAnd
    -- ,''BooleanOr
    ])

-- Base functor (called "Signature" in compdata)
type BooleanBaseF v
    = BooleanValue v
    :+: BooleanVariable v
    -- :+: BooleanNot
    :+: BooleanAnd v
    -- :+: BooleanOr

-- "Standard" boolean expressions
-- use ⊤, ⊥, ¬, ∧, ∨ and variables
type BooleanExpr v = Term (BooleanBaseF v)
