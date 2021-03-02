
module BooleanAlgebra.Logic where

import Prelude hiding (and, or, not, (&&), (||))

import BooleanAlgebra.Class

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
excludes a b = not a || not b

-- | Implication (Material conditional)
implies :: Boolean b => b -> b -> b
implies a b = not a || b

-- | If and only if (Material biconditional)
iff :: Boolean b => b -> b -> b
iff a b = (a `implies` b) && (b `implies` a)

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
existsUnique' :: (Eq a, BooleanAlgebra b) => Boolean b => [a] -> (a -> b) -> b
existsUnique' s p = exists s $ \x ->
    forAll s $ \y ->
    p y `iff` (x `is` (== y))

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
