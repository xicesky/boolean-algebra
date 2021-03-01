
module BooleanAlgebra.Aggregate where

import Control.Applicative (Alternative(..))

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
-- Aggregator   (Flattens repeated operators)

{- 
Repeated conjunctions are aggregated into lists (using associativity)
    a ∧ b ∧ c ∧ d = Conj [a,b,c,d]
    Conj [] = True

Repeated disjunctions are aggregated into lists (using associativity)
    a ∨ b ∨ c ∨ d = Disj [a,b,c,d]
    Disj [] = False
-}

-- unCDLit gives us a guaranteed CD term
-- unCDLit :: BooleanExprCDLit -> BooleanCD BooleanExprCDLit
-- unCDLit = split unCD unLit where
--     unCD :: BooleanCD BooleanExprCDLit -> BooleanCD BooleanExprCDLit
--     unCD = id
--     unLit :: BooleanLit BooleanExprCDLit -> BooleanCD BooleanExprCDLit
--     unLit = BooleanCD . pure . pure . inject

-- | Internal representation: Flattened structure with known head
data Flattened f
    = Conj (Conjunction f)
    | Disj (Disjunction f)
    | Other f

-- | Internal helper: Inject the head back into the term
injF :: (Conjunction :<: f, Disjunction :<: f) => Flattened (Term f) -> Term f
injF (Conj c) = inject c
injF (Disj c) = inject c
injF (Other t) = t


-- aggregateOps aggregates into conjunctions over disjunctions
class Functor f => AggregateOps f where
    aggregateOps :: Alg f (Flattened BooleanExprFlatLit)

-- Lift aggregateOps over sums of functors
$(deriveLiftSum [''AggregateOps])

instance AggregateOps BooleanValue where
    aggregateOps :: Alg BooleanValue (Flattened BooleanExprFlatLit)
    aggregateOps BTrue  = Conj empty    -- Conjunction of 0 terms
    aggregateOps BFalse = Disj empty    -- Disjunction of 0 terms

instance AggregateOps BooleanLit where
    aggregateOps :: Alg BooleanLit (Flattened BooleanExprFlatLit)
    aggregateOps lit = Other $ inject $ fmap undefined lit  -- FIXME undefined

-- FIXME: Remove
type FlatStuff = Flattened BooleanExprFlatLit

instance AggregateOps BooleanAnd where
    aggregateOps :: Alg BooleanAnd (Flattened BooleanExprFlatLit)
    aggregateOps (BAnd l r) = flat l r where
        flat :: FlatStuff -> FlatStuff -> FlatStuff
        flat (Conj l) (Conj r)  = Conj $ l <|> r
        flat (Conj l) rhs       = Conj $ l <|> pure (injF rhs)
        flat lhs      (Conj r)  = Conj $ pure (injF lhs) <|> r
        flat lhs      rhs       = Conj $ pure (injF lhs) <|> pure (injF rhs)

instance AggregateOps BooleanOr where
    aggregateOps :: Alg BooleanOr (Flattened BooleanExprFlatLit)
    aggregateOps (BOr l r) = flat l r where
        flat :: FlatStuff -> FlatStuff -> FlatStuff
        flat (Disj l) (Disj r)  = Disj $ l <|> r
        flat (Disj l) rhs       = Disj $ l <|> pure (injF rhs)
        flat lhs      (Disj r)  = Disj $ pure (injF lhs) <|> r
        flat lhs      rhs       = Disj $ pure (injF lhs) <|> pure (injF rhs)

-- This is the actual aggregation function, using a catamorphism
aggregateConjDisj :: AggregateOps f => Term f -> BooleanExprFlatLit
aggregateConjDisj = injF . cata aggregateOps

-- Tiny helper for our old "Either" problem
aggregateConjDisj' :: AggregateOps f => MaybeTrivial (Term f) -> BooleanExprFlatLit
aggregateConjDisj' (Left BTrue) = iConjunction empty
aggregateConjDisj' (Left BFalse) = iDisjunction empty
aggregateConjDisj' (Right e) = aggregateConjDisj e
