
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

{- Note: there is a (co?) monad here, i can smell it, just have to find it!
... and probably a nice QualifiedDo for stating rules!
-}

{-----------------------------------------------------------------------------}
-- Pre-defined rules and quantifiers

-- FIXME: Use non-empty lists or throw a dedicated error

-- ∀(x ∈ s). p(x)
forAll :: Boolean b => [a] -> (a -> b) -> b
forAll s p = foldr1 and $ fmap p s

-- ∃(x ∈ s). p(x)
exists :: Boolean b => [a] -> (a -> b) -> b
exists s p = foldr1 or $ fmap p s

{- | Mutually exclusive
>>> a `excludes` b := (!a || !b)

Obviously:
>>> (a => !b) == (!a || !b) = (!a <= b)
-}
excludes :: Boolean b => b -> b -> b
excludes a b = (not a) || (not b)

-- This variant actually generates CNF
-- TODO: Eliminate duplicate clauses (symmetry of `excludes`)
existsUnique :: (Eq a, BooleanAlgebra b) => [a] -> (a -> b) -> b
existsUnique s p = foldr1 and
    [   exists s p              -- ∃(x ∈ s). p(x)
    ,   forAll s $ \x1 ->       -- ∀(x1 ∈ s).
        forAll s $ \x2 ->       -- ∀(x2 ∈ s).
        given (x1 /= x2) $      -- (x1 ≠ x2) =>
        p x1 `excludes` p x2    -- ¬p(x1) ∨ ¬p(x2)
    ]

-- This variant is nice and short, but generates irregular terms
-- (which make a nice test)
--existsUnique' :: Boolean b => 

{-----------------------------------------------------------------------------}
-- Pre-defined rules and quantifiers: Hacks

-- FIXME dumb encoding requires a variable
uniqueVar :: BooleanAlgebra b => b
uniqueVar = var "?"

-- FIXME dumb encoding requires a variable
given :: BooleanAlgebra b => Bool -> b -> b
given True = id
given False = const $ truth uniqueVar

-- FIXME dumb encoding requires a variable
is :: BooleanAlgebra b => a -> (a -> Bool) -> b
is a f = if f a then truth uniqueVar else falsity uniqueVar

{-----------------------------------------------------------------------------}
-- Easy examples

neatExample01 :: forall b. BooleanAlgebra b => b
neatExample01 = forAll [1..9] $ (`is` odd) || (`is` even)

neatExample02 :: forall b. BooleanAlgebra b => b
neatExample02 = exists [1..9] $ not (`is` odd)

sortList :: forall b. BooleanAlgebra b => b
sortList = let
    -- | The n-th number is at the position i in the sorted list
    isAt :: BooleanAlgebra b => Int -> Int -> b
    isAt n i = var $ "N" ++ show n ++ "P" ++ show i

    -- | Three numbers, three indices
    ns :: [Int]
    ns = [1..3]

    in foldr1 and
        [   -- Each number has a unique position
            forAll ns $ \number ->
            existsUnique ns $ \position ->
            number `isAt` position
        ]


{-----------------------------------------------------------------------------}
-- Sudoku WIP

sudoku :: BooleanAlgebra b => b
sudoku = let
    ns = [1..2]

    -- | Field x, y has number z
    p :: BooleanAlgebra b => Int -> Int -> Int -> b
    p x y z = var $ "P" ++ show x ++ "_" ++ show y ++ "_" ++ show z

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
