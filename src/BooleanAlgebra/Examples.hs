
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

import Control.Monad.Except

import Data.Comp.Term
import Data.Comp.Ops

import Container

import BooleanAlgebra.Base
import BooleanAlgebra.Variable
import BooleanAlgebra.Pretty
import BooleanAlgebra.Simplify
import BooleanAlgebra.Aggregate
import BooleanAlgebra.CNF

{-----------------------------------------------------------------------------}
-- Example boolean expressions in varying forms

-- | A simple boolean expression in variables @x@, @y@ and @z@.
exampleExpr01 :: BooleanExpr
exampleExpr01 = iBNot (iBVar "x" `iBAnd` iBVar "y") `iBAnd` iBVar "z"

-- | A constant boolean expression.
--
-- >>> printBool $ simplify $ exampleExpr02
exampleExpr02 :: BooleanExpr
exampleExpr02 = iBNot (iBNot iBTrue `iBAnd` iBNot iBFalse)
 
{-| Boolean expression in conjunctive normal form.

aggregateConjDisj' doesn't have a lot of work here:
>>> printBool $ aggregateConjDisj' $ simplify $ exampleExpr03
-}
exampleExpr03 :: BooleanExpr
exampleExpr03 = (iBVar "a" `iBOr` iBVar "b") `iBAnd` (iBVar "c" `iBOr` iBVar "d")

{-| Boolean expression in disjunctive normal form.

aggregateConjDisj' will result in nested CDs:
>>> printBool $ aggregateConjDisj' $ simplify $ exampleExpr04
-}
exampleExpr04 :: BooleanExpr
exampleExpr04 = (iBVar "a" `iBAnd` iBVar "b") `iBOr` (iBVar "c" `iBAnd` iBVar "d")

-- exampleExprCD :: BooleanExprCDLit
-- exampleExprCD = iBooleanCD [ [ iPos "a", iNeg "b" ], [ iNeg "c", iPos "d" ] ]

{- | This is a good example for toCNF

Try it yourself:
>>> printBool $ toCNF $ exampleExpr05
-}
exampleExpr05 :: BooleanExpr
exampleExpr05 = iBNot $ 
    (iBNot (iBVar "a") `iBOr` iBVar "b")
    `iBAnd` iBNot (iBVar "c" `iBAnd` iBVar "d")

-- | An example of the 'CNF' type
exampleCNF :: CNF
exampleCNF = BooleanCD [ [ lPos "a", lNeg "b" ], [ lNeg "c", lPos "d" ] ]

{-----------------------------------------------------------------------------}
-- Example substitutions on variables

-- | Renames variables (by prepending 'z')
exampleSubst01 :: BooleanExpr -> BooleanExpr
exampleSubst01 = substVar hom where
    hom :: BooleanVariable a -> Context BooleanBaseF a
    hom (BVariable s) = iBVar $ 'z' : s

-- | Base functor without variables
type BooleanBaseNoVarsF
    = BooleanValue
    :+: BooleanNot
    :+: BooleanAnd
    :+: BooleanOr

-- | Terms without variables
type BooleanExprNoVars = Term BooleanBaseNoVarsF

{- | Eliminate variables by substituting "true" for all of them

>>> printBool $ exampleSubst02 $ exampleExpr05
>>> printBool $ simplify $ exampleSubst02 $ exampleExpr05
-}
exampleSubst02 :: BooleanExpr -> BooleanExprNoVars
exampleSubst02 = substVar hom where
    hom :: BooleanVariable a -> Context BooleanBaseNoVarsF a
    hom (BVariable s) = iBTrue

{- | Replace variables with another term from an environment (map)

>>> printBool $ exampleSubst03 $ exampleExpr05
>>> printBool $ simplify $ exampleSubst03 $ exampleExpr05
-}
exampleSubst03 :: BooleanExpr -> BooleanExpr
exampleSubst03 = substitute' env where
    env :: HashMap String BooleanExpr
    env = fromList
        [   ("a",   iBNot (iBVar "a")   )
        ,   ("c",   iBFalse             )
        ]

{- | Eliminate variables by substituting values

>>> printBool $ exampleSubst04 $ exampleExpr05
>>> printBool $ simplify $ exampleSubst04 $ exampleExpr05
-}
exampleSubst04 :: BooleanExpr -> BooleanExprNoVars
exampleSubst04 term = let
    env :: HashMap String BooleanExprNoVars
    env = fromList
        [   ("a",   iBTrue              )
        ,   ("b",   iBFalse             )
        ,   ("c",   iBFalse             )
        ,   ("d",   iBFalse             )
        ]
    err :: String -> Except String (Context g a)
    err s = throwError $ "Unmapped variable " ++ s
    in case runExcept $ substituteM env err term of
        Left message -> error message       -- Well, this just an example
        Right term -> term

{-----------------------------------------------------------------------------}
