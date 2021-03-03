
module BooleanAlgebra.Examples where

import Prelude hiding (and, or, not, (&&), (||))

import Control.Monad.Except

import Data.Comp.Term
import Data.Comp.Ops

import Container

import BooleanAlgebra.Base.Class
import BooleanAlgebra.Base.Expression
import BooleanAlgebra.Base.Pretty
import BooleanAlgebra.Transform.Variable
import BooleanAlgebra.Transform.Simplify
import BooleanAlgebra.Transform.Aggregate
import BooleanAlgebra.Transform.CNF

{-----------------------------------------------------------------------------}
-- Example boolean expressions in varying forms

-- | A simple boolean expression in variables @x@, @y@ and @z@.
exampleExpr01 :: BooleanExpr
exampleExpr01 = not (var "x" `and` var "y") `and` var "z"

-- | A constant boolean expression.
--
-- >>> printBool $ simplify $ exampleExpr02
exampleExpr02 :: BooleanExpr
exampleExpr02 = not (not true `and` not false)
 
{-| Boolean expression in conjunctive normal form.

aggregateConjDisj' doesn't have a lot of work here:
>>> printBool $ aggregateConjDisj' $ simplify $ exampleExpr03
-}
exampleExpr03 :: BooleanExpr
exampleExpr03 = (var "a" `or` var "b") `and` (var "c" `or` var "d")

{-| Boolean expression in disjunctive normal form.

aggregateConjDisj' will result in nested CDs:
>>> printBool $ aggregateConjDisj' $ simplify $ exampleExpr04
-}
exampleExpr04 :: BooleanExpr
exampleExpr04 = (var "a" `and` var "b") `or` (var "c" `and` var "d")

-- exampleExprCD :: BooleanExprCDLit
-- exampleExprCD = iBooleanCD [ [ iPos "a", iNeg "b" ], [ iNeg "c", iPos "d" ] ]

{- | This is a good example for toCNF

Try it yourself:
>>> printBool $ toCNF $ exampleExpr05
-}
exampleExpr05 :: BooleanExpr
exampleExpr05 = not $ 
    (not (var "a") `or` var "b")
    `and` not (var "c" `and` var "d")

{-  FIXME: use naming monad to create?

-- | An example of the 'CNF' type
exampleCNF :: CNF
exampleCNF = cnfFromList [ [ lPos "a", lNeg "b" ], [ lNeg "c", lPos "d" ] ]

-}

{-----------------------------------------------------------------------------}
-- Example substitutions on variables

-- | Renames variables (by prepending 'z')
exampleSubst01 :: BooleanExpr -> BooleanExpr
exampleSubst01 = substVar hom where
    hom :: BooleanVariable a -> Context BooleanBaseF a
    hom (BVariable s) = iBVariable $ 'z' : s

-- | Base functor without variables
type BooleanBaseNoVarsF
    = BooleanValue
    :+: BooleanNot
    :+: BooleanOp

-- | Terms without variables
type BooleanExprNoVars = Term BooleanBaseNoVarsF

{- | Eliminate variables by substituting "true" for all of them

>>> printBool $ exampleSubst02 $ exampleExpr05
>>> printBool $ simplify $ exampleSubst02 $ exampleExpr05
-}
exampleSubst02 :: BooleanExpr -> BooleanExprNoVars
exampleSubst02 = substVar hom where
    hom :: BooleanVariable a -> Context BooleanBaseNoVarsF a
    hom (BVariable s) = iBooleanValue True

{- | Replace variables with another term from an environment (map)

>>> printBool $ exampleSubst03 $ exampleExpr05
>>> printBool $ simplify $ exampleSubst03 $ exampleExpr05
-}
exampleSubst03 :: BooleanExpr -> BooleanExpr
exampleSubst03 = substitute' env where
    env :: HashMap String BooleanExpr
    env = fromList
        [   ("a",   not (var "a")       )
        ,   ("c",   false               )
        ]

{- | Eliminate variables by substituting values

>>> printBool $ exampleSubst04 $ exampleExpr05
>>> printBool $ simplify $ exampleSubst04 $ exampleExpr05
-}
exampleSubst04 :: BooleanExpr -> BooleanExprNoVars
exampleSubst04 term = let
    env :: HashMap String BooleanExprNoVars
    env = fromList
        [   ("a",   iBooleanValue True  )
        ,   ("b",   iBooleanValue False )
        ,   ("c",   iBooleanValue False )
        ,   ("d",   iBooleanValue False )
        ]
    err :: String -> Except String (Context g a)
    err s = throwError $ "Unmapped variable " ++ s
    in case runExcept $ substituteM env err term of
        Left message -> error message       -- Well, this just an example
        Right term -> term

{-----------------------------------------------------------------------------}
