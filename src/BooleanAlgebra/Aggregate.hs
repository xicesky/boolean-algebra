
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
import Data.Comp.Show ()            -- for the Show instance
import Data.Comp.Equality ()        -- for the Eq instance

import BooleanAlgebra.THUtil
import BooleanAlgebra.Base
import BooleanAlgebra.Pretty
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

-- This will result in a single (non-nested) CD
exampleExpr03 :: BooleanExpr
exampleExpr03 = (iBVar "a" `iBOr` iBVar "b") `iBAnd` (iBVar "c" `iBOr` iBVar "d")

-- This will result in nested CDs, bc. of disjunctions over conjunctions
exampleExpr04 :: BooleanExpr
exampleExpr04 = (iBVar "a" `iBAnd` iBVar "b") `iBOr` (iBVar "c" `iBAnd` iBVar "d")

-- exampleExprCD :: BooleanExprCDLit
-- exampleExprCD = iBooleanCD [ [ iPos "a", iNeg "b" ], [ iNeg "c", iPos "d" ] ]

{- TODO: Fix bugs in compdata

Bug #1:
    show-ing terms of BooleanCD is wrong and puts quotes where none belong:
        putStrLn $ show exampleExpr03
        (BooleanCD [["(BooleanLit True \"a\")","(BooleanLit False \"b\")"],["(BooleanLit False \"c\")","(BooleanLit True \"d\")"]])
    
    Data.Comp.Derive.Show handles arguments this way:
        mkShow :: (Bool, ExpQ) -> ExpQ
        mkShow (isFArg, var)
            | isFArg = var
            | otherwise = [| show $var |]
    Apparently [[e]] is not a functor argument - this can lead to other bugs!

    This one will be hard to get right, but should accept nested functors:
        data Meh e = Meh e          deriving Functor
        data Muh e = Muh (Meh e)    deriving Functor

Bug #2:
    compdata should use showsPrec instead of show
    1. Performance (maybe not that relevant, ghc rewrites a lot of that stuff)
    2. Precedence is important, don't put parens everywhere

-}

-- Custom instance of ShowF - workaround for a bug in compdata
instance ShowF BooleanCD where
    showF (BooleanCD xs) = let
        ccList :: [ShowS] -> ShowS
        ccList = ccShowList "[" ", " "]"
        strCDs :: ShowS
        strCDs = ccList . fmap (ccList . fmap (++)) $ xs
        in (showCon "BooleanCD") [strCDs ""]

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
aggregateConjDisj :: BooleanExprLit -> BooleanExprCDLit
aggregateConjDisj e = cata aggregateCD e

-- Tiny helper for our old "Either" problem
aggregateConjDisj' :: Either (BooleanValue ()) BooleanExprLit -> BooleanExprCDLit
aggregateConjDisj' (Left BTrue) = iBooleanCD []
aggregateConjDisj' (Left BFalse) = iBooleanCD [[]]  -- TODO: this should be a function, also BVal -> Boolean
aggregateConjDisj' (Right e) = aggregateConjDisj e
