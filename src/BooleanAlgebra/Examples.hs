
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-} 

-- Extensions for compdata usage
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module BooleanAlgebra.Examples where

import Data.Comp.Term
import Data.Comp.Ops

import BooleanAlgebra.Base
import BooleanAlgebra.Variable
import BooleanAlgebra.Pretty
import BooleanAlgebra.Simplify
import BooleanAlgebra.Aggregate
import BooleanAlgebra.CNF

{-----------------------------------------------------------------------------}
-- Example boolean expressions in varying forms

exampleExpr01 :: BooleanExpr
exampleExpr01 = iBNot (iBVar "x" `iBAnd` iBVar "y") `iBAnd` iBVar "z"

exampleExpr02 :: BooleanExpr
exampleExpr02 = iBNot (iBNot iBTrue `iBAnd` iBNot iBFalse)


{- Examples for aggregations:
    >>> aggregateConjDisj' $ simplify $ exampleExpr03
will result in a single (non-nested) CD
    >>> aggregateConjDisj' $ simplify $ exampleExpr04
will result in nested CDs, bc. of disjunctions over conjunctions
-}
 
exampleExpr03 :: BooleanExpr
exampleExpr03 = (iBVar "a" `iBOr` iBVar "b") `iBAnd` (iBVar "c" `iBOr` iBVar "d")

exampleExpr04 :: BooleanExpr
exampleExpr04 = (iBVar "a" `iBAnd` iBVar "b") `iBOr` (iBVar "c" `iBAnd` iBVar "d")

-- exampleExprCD :: BooleanExprCDLit
-- exampleExprCD = iBooleanCD [ [ iPos "a", iNeg "b" ], [ iNeg "c", iPos "d" ] ]

{-  This is a good example for toCNF:
    >>> printBool $ toCNF $ exampleExpr05
-}
exampleExpr05 :: BooleanExpr
exampleExpr05 = iBNot $ 
    (iBNot (iBVar "a") `iBOr` iBVar "b")
    `iBAnd` iBNot (iBVar "c" `iBAnd` iBVar "d")

exampleCNF :: CNF
exampleCNF = BooleanCD [ [ lPos "a", lNeg "b" ], [ lNeg "c", lPos "d" ] ]

{-----------------------------------------------------------------------------}
-- Example substitutions on variables

-- Renames variables (by prepending 'z')
exampleSubst01 :: BooleanExpr -> BooleanExpr
exampleSubst01 = substVar hom where
    hom :: BooleanVariable a -> Context BooleanBaseF a
    hom (BVariable s) = iBVar $ 'z' : s

-- | Base terms without variables
type BooleanBaseNoVarsF
    = BooleanValue
    :+: BooleanNot
    :+: BooleanAnd
    :+: BooleanOr

type BooleanExprNoVars = Term BooleanBaseNoVarsF

{- | Eliminate variables by substituting "true" for all of them
    >>> printBool $ exampleSubst02 $ exampleExpr05
    >>> printBool $ simplify $ exampleSubst02 $ exampleExpr05
-}
exampleSubst02 :: BooleanExpr -> BooleanExprNoVars
exampleSubst02 = substVar hom where
    hom :: BooleanVariable a -> Context BooleanBaseNoVarsF a
    hom (BVariable s) = iBTrue

{-----------------------------------------------------------------------------}
