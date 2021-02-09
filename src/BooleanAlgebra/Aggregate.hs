
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

import Data.List (intersperse)
import Control.Monad (join)

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

{- BooleanCD: Conjunction over Disjunctions
    BooleanCD [[a,b],[c,d]] ≅ (a ∨ b) ∧ (c ∨ d)
-}
data BooleanCD e = BooleanCD [[e]]
    --deriving Functor
    deriving (Show, Eq, Functor)

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

$(deriveNoShow [''BooleanCD])

-- Utility function: Concatenate a list of ShowS using a seperator
ccShowList :: String -> String -> String -> [ShowS] -> ShowS
ccShowList begin sep end list = let
    sList = foldr (.) id $ intersperse (showString sep) list
    in showString begin . sList . showString end

-- Copied over from Data.Comp.Derive.Show
showCon :: String -> [String] -> String
showCon con [] = con
showCon con args = "(" ++ con ++ " " ++ unwords args ++ ")"

-- Custom instance of ShowF - workaround for a bug in compdata
instance ShowF BooleanCD where
    showF (BooleanCD xs) = let
        ccList :: [ShowS] -> ShowS
        ccList = ccShowList "[" ", " "]"
        strCDs :: ShowS
        strCDs = ccList . fmap (ccList . fmap (++)) $ xs
        in (showCon "BooleanCD") [strCDs ""]

{- Utility function:
Concatenate lists of precedence-dependent arguments with an operator
-}
ccListOp :: (Int, String) -> (Int -> ShowS) -> [Int -> ShowS] -> Int -> ShowS
ccListOp _          empty []    = empty
ccListOp _          _     [e]   = e
ccListOp (prec, op) _     es    = \d -> let
    listOfShowS :: [ShowS]
    listOfShowS = fmap ($ prec+1) es
    in showParen (d > prec) $ ccShowList "" op "" listOfShowS

-- Pretty-printer for BooleanCD
instance PrettyBool BooleanCD where
    prettyPrintBoolAlg :: BooleanCD (Int -> ShowS) -> Int -> ShowS
    prettyPrintBoolAlg (BooleanCD cds) d
        = showCDs cds d where
            showDisjs :: [Int -> ShowS] -> Int -> ShowS
            showDisjs = ccListOp (3, "∨") empty where
                empty = prettyPrintAB BFalse

            showConjs :: [Int -> ShowS] -> Int -> ShowS
            showConjs = ccListOp (6, "∧") empty where
                empty = prettyPrintAB BTrue

            showCDs :: [[Int -> ShowS]] -> Int -> ShowS
            showCDs = showConjs . fmap showDisjs

-- BooleanExprLit where BooleanCD replaces (BooleanAnd, BooleanOr)
type BooleanExprCDLitF
    =   BooleanLit
    :+: BooleanCD

type BooleanExprCDLit = Term BooleanExprCDLitF

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

-- exampleExprCD :: BooleanExprCDLit
-- exampleExprCD = iBooleanCD [ [ iPos "a", iNeg "b" ], [ iNeg "c", iPos "d" ] ]

-- This will result in a single (non-nested) CD
exampleExpr03 :: BooleanExpr
exampleExpr03 = (iBVar "a" `iBOr` iBVar "b") `iBAnd` (iBVar "c" `iBOr` iBVar "d")

-- This will result in nested CDs, bc. of disjunctions over conjunctions
exampleExpr04 :: BooleanExpr
exampleExpr04 = (iBVar "a" `iBAnd` iBVar "b") `iBOr` (iBVar "c" `iBAnd` iBVar "d")
