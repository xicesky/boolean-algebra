
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-} 

-- | Let's try to generate some formulas for Sudoku :)
module Gen where

import Prelude hiding (and, or, not, (&&), (||))

import BooleanAlgebra.Class
import BooleanAlgebra.Base

-- FIXME ghci imports
import BooleanAlgebra.Pretty
import BooleanAlgebra.Simplify
import BooleanAlgebra.Aggregate
import BooleanAlgebra.CNF
import BooleanAlgebra.Examples

-- Predicate logic!?!?
instance Boolean b => Boolean (a -> b) where
    and a b i = and (a i) (b i)
    or  a b i = or  (a i) (b i)
    not a i   = not (a i)

forAll :: Boolean b => [a] -> (a -> b) -> b
forAll is p = foldr1 and $ fmap p is

exists :: Boolean b => [a] -> (a -> b) -> b
exists is p = foldr1 or $ fmap p is

{- Note: there is a (co?) monad here, i know it, just have to find it!
... and probably a nice QualifiedDo for stating rules!
-}


-- FIXME dumb encoding requires a variable
is :: BooleanAlgebra b => a -> (a -> Bool) -> b
is a f = if f a then truth (var "y") else falsity (var "y")

{-----------------------------------------------------------------------------}
-- Easy examples

neatExample01 :: forall b. BooleanAlgebra b => b
neatExample01 = forAll [1..9] $ (`is` odd) || (`is` even)

neatExample02 :: forall b. BooleanAlgebra b => b
neatExample02 = exists [1..9] $ not (`is` odd)

{-----------------------------------------------------------------------------}
-- Sudoku WIP

-- | Field x, y has number z
p :: BooleanAlgebra b => Int -> Int -> Int -> b
p x y z = var $ "P" ++ show x ++ show y ++ show z

sudoku :: BooleanAlgebra b => b
sudoku = let
    ns = [1..2]
    in foldr1 and
        [   exists ns $ forAll [3..4] $ p 0 -- forAll ns $ exists ns $ p 0
        ]

{- Idea: Encode the pidgeonhole problem!
-}

{- Idea: Encode "Who owns the zebra":
https://drive.google.com/file/d/1WRUQSKIHKLpEv3_OlcB6P4Z8G2-sismi/view
-}

{- Idea: Build isomorphisms between problem domain and SAT, so
we can easily encode the problem and decode the result.

Idea: Use isos to illustrate the solving process.
We could inspect certain variable assignments during solving,
or even interpret clauses.
interesting:
    - Facts found (e.g. assigned number)
    - Learned clauses
    - Implication graph...
    - Implications that lead to each fact
-}
