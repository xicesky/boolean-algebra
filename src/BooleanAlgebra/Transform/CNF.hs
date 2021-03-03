
module BooleanAlgebra.Transform.CNF where

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

import BooleanAlgebra.Util.THUtil
import BooleanAlgebra.Util.Util
import BooleanAlgebra.Base.Expression
import BooleanAlgebra.Transform.IntermediateForms
import BooleanAlgebra.Transform.Simplify
import BooleanAlgebra.Transform.Aggregate

{-----------------------------------------------------------------------------}
-- Distributor  (Step 3 of toCNF)
-- Distributes disjunctions over conjunctions:
--      a ∨ (b ∧ c) = (a ∨ b) ∧ (a ∨ c)

distributeCNF :: Disjunction (Conjunction e) -> Conjunction (Disjunction e)
distributeCNF = sequenceA       -- Well, isn't this easy.

joinConjunction :: Conjunction (Conjunction e) -> Conjunction e
joinConjunction (Conjunction xs) = -- Monad.join
    Conjunction [y | Conjunction x <- xs, y <- x]

joinDisjunction :: Disjunction (Disjunction e) -> Disjunction e
joinDisjunction (Disjunction xs) = -- Monad.join
    Disjunction [y | Disjunction x <- xs, y <- x]

-- Distribute Disjunctions over Conjunctions (limited to BooleanCD)
class Functor f => DistributeDoC f where
    distributeDoC :: Alg f CNF

-- Lift aggregateCD over sums of functors
$(deriveLiftSum [''DistributeDoC])

instance DistributeDoC BooleanLit where
    distributeDoC :: BooleanLit CNF -> CNF
    distributeDoC = pure . pure . constmap

instance DistributeDoC Conjunction where
    distributeDoC :: Conjunction CNF -> CNF
    distributeDoC = joinConjunction

instance DistributeDoC Disjunction where
    distributeDoC :: Disjunction CNF -> CNF
    distributeDoC = fmap joinDisjunction . distributeCNF

distributeToCNF :: DistributeDoC f => Term f -> CNF
distributeToCNF = cata distributeDoC

{-----------------------------------------------------------------------------}
-- Basic conversion to CNF
-- doesn't add any variables

toCNF :: forall f. (SimpBool f
    , Aggregate ([String], MaybeTrivial BooleanExprLit) ([String], BooleanExprFlatLit))
    => Term f -> ([String], CNF)
toCNF e = let
    simplified :: ([String], MaybeTrivial BooleanExprLit)
    simplified = simplify e
    aggregated :: ([String], BooleanExprFlatLit)
    aggregated = aggregateConjDisj simplified
    in fmap distributeToCNF aggregated

{-----------------------------------------------------------------------------}
-- Stats of CNF clauses

data CNFStats = CNFStats
    {   nClauses :: Int
    ,   minClauseLength :: Int
    ,   maxClauseLength :: Int
    ,   averageClauseLength :: Float
    }
    deriving (Show, Eq)

-- | Collect some statistics of a CNF representation.
-- Does not work on the empty CNF!
cnfStats :: CNF -> CNFStats
cnfStats (Conjunction xs) = CNFStats
    {   nClauses = length xs
    ,   minClauseLength = minimum clens
    ,   maxClauseLength = maximum clens
    ,   averageClauseLength = average clens
    } where
    clens :: [Int]
    clens = fmap length xs
    average :: (Real a, Fractional b) => [a] -> b
    average xs = realToFrac (sum xs) / realToFrac (length xs)
