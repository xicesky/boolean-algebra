
{- |
Description     : Logic on boolean predicates
Stability       : experimental

Tools for encoding predicates as simple boolean logic.
-}
module BooleanAlgebra.Base.Logic
    (   -- * Quantifiers
        forAll
    ,   exists
    ,   existsUnique
    ,   existsUnique'

    ,   -- * Misc operations
        excludes, implies, iff
    ,   given, is, define

    ) where

import Prelude hiding (and, or, not, (&&), (||))

import BooleanAlgebra.Base.Class

-- Predicate logic!?!?
instance PreBoolean b => PreBoolean (a -> b) where
    not a i   = not (a i)

instance Boolean b => Boolean (a -> b) where
    and a b i = and (a i) (b i)
    or  a b i = or  (a i) (b i)

{- Note: there is a (co?) monad here, i can smell it, just have to find it!
... and probably a nice QualifiedDo for stating rules!
-}

{-----------------------------------------------------------------------------}
-- Pre-defined rules and quantifiers

-- FIXME: Use non-empty lists or throw a dedicated error

{- | For all values then given predicate is true.

∀(x ∈ s). p(x)

>>> forAll s $ \x -> p x
-}
forAll :: Boolean b => [a] -> (a -> b) -> b
forAll s p = foldr1 and $ fmap p s

{- | There exists a value for which the given predicate is true.

∃(x ∈ s). p(x)

>>> exists s $ \x -> p x
-}
exists :: Boolean b => [a] -> (a -> b) -> b
exists s p = foldr1 or $ fmap p s

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
{- | There exists a value for which the given predicate is true.

_DO NOT USE_: This variant generates irregular terms (which can get
very large when transforming to CNF). This is useful for some tests,
but 'existsUnique' does the same thing.

∃(x ∈ s). p(x)

>>> exists s $ \x -> p x
-}

existsUnique' :: (Eq a, BooleanAlgebra b) => Boolean b => [a] -> (a -> b) -> b
existsUnique' s p = exists s $ \x ->
    forAll s $ \y ->
    p y `iff` (x `is` (== y))

{-----------------------------------------------------------------------------}

{- | Mutually exclusive

>>> a `excludes` b := (!a || !b)

Obviously:

>>> (a => !b) == (!a || !b) == (!a <= b)
-}
excludes :: Boolean b => b -> b -> b
excludes a b = not a || not b

-- | Implication (Material conditional)
implies :: Boolean b => b -> b -> b
implies a b = not a || b

-- | If and only if (Material biconditional)
iff :: Boolean b => b -> b -> b
iff a b = (a `implies` b) && (b `implies` a)
-- = a `xnor` b -- but this is DNF

{- | Test a value using a Haskell predicate.

>>> 3 `is` odd
-}
is :: BooleanArithmetic b => a -> (a -> Bool) -> b
is a f = if f a then true else false

{- | Fixed condition for encoding a rule.

>>> given (i < j) $ myPred i j

This is equivalent to (but shorter than):

>>> fromBool (i < j) `implies` myPred i j

(and also encodes as a simpler formula).
-}
given :: BooleanArithmetic b => Bool -> b -> b
given True  = id
given False = const true

{- | "Define" something to be true or false.

Usually used to give preset meanings to predicates,
e.g. @myPred i@ holds exactly if @i@ is odd:

>>> define (myPred i) (odd i)

This is equivalent to (but shorter than):

>>> fromBool (odd i) `iff` myPred i
-}
define :: Boolean b => b -> Bool -> b
define pred True    = pred
define pred False   = not pred

{-----------------------------------------------------------------------------}
