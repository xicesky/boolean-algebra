
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-} 

-- Extensions for compdata usage
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module BooleanAlgebra.Aggregate where

import Data.Comp.Term
import Data.Comp.Ops
import Data.Comp.Sum (inject, split)
import Data.Comp.Algebra
    (   Alg, Coalg, RAlg, RCoalg
    ,   cata, ana, para, apo
    )

import Data.Comp.Derive
--import Data.Comp.Derive.Show
--import Data.Comp.Show ()            -- for the Show instance
--import Data.Comp.Equality ()        -- for the Eq instance

import BooleanAlgebra.THUtil
import BooleanAlgebra.Base
import BooleanAlgebra.Simplify

{-----------------------------------------------------------------------------}
-- Aggregator   (Helper)

{- 
Repeated conjunctions are aggregated into lists (using associativity)
    a ∧ b ∧ c ∧ d = Conj [a,b,c,d]
    Conj [] = True

Repeated disjunctions are aggregated into lists (using associativity)
    a ∨ b ∨ c ∨ d = Disj [a,b,c,d]
    Disj [] = False
-}

-- unCDLit gives us a guaranteed CD term
unCDLit :: BooleanExprCDLit -> BooleanCD BooleanExprCDLit
unCDLit = split unCD unLit where
    unCD :: BooleanCD BooleanExprCDLit -> BooleanCD BooleanExprCDLit
    unCD = id
    unLit :: BooleanLit BooleanExprCDLit -> BooleanCD BooleanExprCDLit
    unLit = BooleanCD . pure . pure . inject

-- aggregateCD aggregates into conjunctions over disjunctions
class Functor f => AggregateCD f where
    aggregateCD :: Alg f BooleanExprCDLit

-- Lift aggregateCD over sums of functors
$(deriveLiftSum [''AggregateCD])

instance AggregateCD BooleanValue where
    aggregateCD :: BooleanValue BooleanExprCDLit -> BooleanExprCDLit
    aggregateCD BTrue = iBooleanCD []       -- Conjunction of 0 terms
    aggregateCD BFalse = iBooleanCD [[]]    -- Disjunction of 0 terms

instance AggregateCD BooleanLit where
    aggregateCD :: BooleanLit BooleanExprCDLit -> BooleanExprCDLit
    aggregateCD lit = inject lit

instance AggregateCD BooleanAnd where
    aggregateCD :: BooleanAnd BooleanExprCDLit -> BooleanExprCDLit
    aggregateCD (BAnd cda cdb) = iBooleanCD $ cdas ++ cdbs where
        BooleanCD cdas = unCDLit cda
        BooleanCD cdbs = unCDLit cdb

instance AggregateCD BooleanOr where
    aggregateCD :: BooleanOr BooleanExprCDLit -> BooleanExprCDLit
    aggregateCD (BOr cda cdb) = let
        BooleanCD cdas = unCDLit cda
        BooleanCD cdbs = unCDLit cdb
        in case (cdas, cdbs) of
            -- Both are just disjunctions
            ([disja], [disjb]) -> iBooleanCD [disja ++ disjb]
            -- All other cases require an outer CD term
            _ -> iBooleanCD [[cda, cdb]]

-- This is the actual aggregation function, using a catamorphism
aggregateConjDisj :: AggregateCD f => Term f -> BooleanExprCDLit
aggregateConjDisj = cata aggregateCD

-- Tiny helper for our old "Either" problem
aggregateConjDisj' :: AggregateCD f => MaybeTrivial (Term f) -> BooleanExprCDLit
aggregateConjDisj' (Left BTrue) = iBooleanCD []
aggregateConjDisj' (Left BFalse) = iBooleanCD [[]]  -- TODO: this should be a function, also BVal -> Boolean
aggregateConjDisj' (Right e) = aggregateConjDisj e
