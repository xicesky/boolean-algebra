
{- |
Description     : Various examples
Stability       : experimental

Example expressions and demonstations.
-}
module BooleanAlgebra.Examples where

import Prelude hiding (and, or, not, (&&), (||))

import Control.Monad.Except

import Container

import BooleanAlgebra.Base.Class
import BooleanAlgebra.Base.Expression
import BooleanAlgebra.Base.Pretty
import BooleanAlgebra.Transform.Variable
import BooleanAlgebra.Transform.Simplify
import BooleanAlgebra.Transform.CNF

{-----------------------------------------------------------------------------}
-- Example boolean expressions in varying forms

-- | A simple boolean expression in variables @x@, @y@ and @z@.
exampleExpr01 :: BooleanExpr String
exampleExpr01 = not (var "x" `and` var "y") `and` var "z"

-- | A constant boolean expression.
--
-- >>> printBool $ simplify $ exampleExpr02
exampleExpr02 :: BooleanExpr String
exampleExpr02 = not (not true `and` not false)

{-| Boolean expression in conjunctive normal form.

aggregateConjDisj' doesn't have a lot of work here:
>>> printBool $ aggregateConjDisj' $ simplify $ exampleExpr03
-}
exampleExpr03 :: BooleanExpr String
exampleExpr03 = (var "a" `or` var "b") `and` (var "c" `or` var "d")

{-| Boolean expression in disjunctive normal form.

aggregateConjDisj' will result in nested CDs:
>>> printBool $ aggregateConjDisj' $ simplify $ exampleExpr04
-}
exampleExpr04 :: BooleanExpr String
exampleExpr04 = (var "a" `and` var "b") `or` (var "c" `and` var "d")

-- exampleExprCD :: BooleanExprCDLit
-- exampleExprCD = iBooleanCD [ [ iPos "a", iNeg "b" ], [ iNeg "c", iPos "d" ] ]

{- | This is a good example for toCNF

Try it yourself:
>>> printBool $ toCNF $ exampleExpr05
-}
exampleExpr05 :: BooleanExpr String
exampleExpr05 = not $
    (not (var "a") `or` var "b")
    `and` not (var "c" `and` var "d")

{- | This is an example for the pretty-printer

It should require no parentheses (it's in DNF).

Try it yourself:
>>> pretty exampleExpr06
-}
exampleExpr06 :: BooleanExpr String
exampleExpr06 = var "a" && not (var "b")
    || var "b" && var "c"
    || not (var "a") && var "c"
    || not (var "a") && not (var "b") && not (var "c")

{-----------------------------------------------------------------------------}
