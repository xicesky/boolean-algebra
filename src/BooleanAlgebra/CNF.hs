
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-} 

-- Extensions for compdata usage
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module BooleanAlgebra.CNF where

import Data.Comp.Term
import Data.Comp.Ops
import Data.Comp.Algebra
    (   Alg, Coalg, RAlg, RCoalg
    ,   cata, ana, para, apo
    )

import Data.Comp.Derive
--import Data.Comp.Derive.Show
import Data.Comp.Show ()            -- for the Show instance
import Data.Comp.Equality ()        -- for the Eq instance

import Control.Monad (join)

import BooleanAlgebra.THUtil
import BooleanAlgebra.Base
import BooleanAlgebra.Simplify
import BooleanAlgebra.Aggregate

{-----------------------------------------------------------------------------}
-- Distributor  (Step 3 of toCNF)
-- Distributes disjunctions over conjunctions:
--      a ∨ (b ∧ c) = (a ∨ b) ∧ (a ∨ c)

-- a ∨ (x0 ∧ x1 ∧ ...) = (a ∨ x0) ∧ (a ∨ x1) ∧ ...
distrLit :: BooleanLit a -> [[BooleanLit a]] -> [[BooleanLit a]]
distrLit a = fmap (a :)

-- (a0 ∨ ...) ∨ (x0 ∧ ...) = (a0 ∨ ... ∨ x0) ∧ ...
distrDisj :: [BooleanLit a] -> [[BooleanLit a]] -> [[BooleanLit a]]
distrDisj as = fmap (as ++)

-- (as0 ∧ as1 ∧ ...) ∨ (x0 ∧ ...) = (as0 ∨ (x0 ...)) ∧ (as1 ∨ (x0 ...)) ∧ ...
distrConj :: [[BooleanLit a]] -> [[BooleanLit a]] -> [[BooleanLit a]]
distrConj ass xs = ass >>= (`distrDisj` xs)

-- Disjunction of two CNFs
distrCNF :: CNF -> CNF -> CNF
distrCNF (BooleanCD a) (BooleanCD x) = BooleanCD $ distrConj a x

-- Conjunction of two CNFs
joinCNF :: CNF -> CNF -> CNF
joinCNF (BooleanCD a) (BooleanCD x) = BooleanCD $ a ++ x

-- Distribute Disjunctions over Conjunctions (limited to BooleanCD)
class Functor f => DistributeDoC f where
    distributeDoC :: Alg f CNF

-- Lift aggregateCD over sums of functors
$(deriveLiftSum [''DistributeDoC])

instance DistributeDoC BooleanLit where
    distributeDoC :: BooleanLit CNF -> CNF
    distributeDoC lit = BooleanCD [[fmap undefined lit]]

instance DistributeDoC BooleanCD where
    distributeDoC :: BooleanCD CNF -> CNF
    distributeDoC (BooleanCD conjs) = foldr1 joinCNF $ fmap distr conjs where
        distr :: [CNF] -> CNF
        distr [x]       = x
        distr (x:xs)    = distr $ fmap (distrCNF x) xs

distributeToCNF :: DistributeDoC f => Term f -> CNF
distributeToCNF = cata distributeDoC

{-----------------------------------------------------------------------------}
-- Basic conversion to CNF
-- doesn't add any variables

toCNF :: SimpBool f => Term f -> CNF
toCNF = distributeToCNF . aggregateConjDisj' . simplify
