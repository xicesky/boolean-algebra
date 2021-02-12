
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-} 

{-# LANGUAGE RankNTypes             #-}

module Thoughts where

import Data.Bifunctor

{-----------------------------------------------------------------------------}
-- From my experiment with compdata (extending it to bifunctors)

{- Idea:
What property of (:+:) makes this work?
Can we use PolyKinds to extend this to any Kind of f/v?
    Functor f, Functor g => Functor (f :+: g)
    Functor (f v), Functor (g v) => Functor (f :+: g) v
    Foldable f, Foldable g => Foldable (f :+: g)
    (Foldable (f v), Foldable (g v)) => Foldable ((f :+: g) v)

Is (:+:) really distributive over application?
    (f :+: g) x ~ (f x :+: g x)

Idea: If f and g were functions, polymorphism wouldn't help either:
-}

-- One example would be "summing" functions via pairing
mkSum1 :: a -> b -> (,) a b
mkSum1 = (,)

-- Or construction of an Either
mkSum2 :: a -> b -> Either a b
mkSum2 a b = Left a

-- Now examples for f and g:
f :: Int -> Int
f = (+1)

g :: Int -> String
g = show

-- Sums look like this
sum1 :: (,) (Int -> Int) (Int -> String)    -- ≠ Int -> (,) Int String
sum1 = mkSum1 f g

sum2 :: Either (Int -> Int) (Int -> String) -- ≠ Int -> Either Int String
sum2 = mkSum2 f g

-- But these can be lifted!
-- This one looks a bit like Applicative <*> but heterogenous
liftFS2 :: Either (v -> a) (v -> b) -> (v -> Either a b)
liftFS2 (Left fa) v = Left (fa v)
liftFS2 (Right fb) v = Right (fb v)

-- BTW: It's not an isomorphism, but i'm not sure how to prove it rn.
sinkFS :: (v -> Either a b) -> Either (v -> a) (v -> b)
sinkFS f = undefined -- ?? can't work because we don't know what f will do

sum3 :: Int -> Either Int String
sum3 = liftFS2 sum2

-- The relevant property is that "(,)" and "Either" are bifunctors!
--  bimap :: (a0 -> a1) -> (b0 -> b1) -> Either a0 b0 -> Either a1 b1
-- can be used to supply arguments into the functions inside
liftFS2' :: Either (v -> a) (v -> b) -> (v -> Either a b)
liftFS2' either v = bimap ($ v) ($ v) either

-- More general
liftFS :: Bifunctor p => p (a -> b) (a -> d) -> a -> p b d
liftFS f v = bimap ($ v) ($ v) f
-- try: (liftFS sum1) 100

{-----------------------------------------------------------------------------}
-- So, back to the type level

{-  We want a thing like
        (:+:) :: a -> b -> Sum a b                          -- Kind! signature
    but with PolyKinds, where "Sum a b" might not even be a type.
    (This means it can take two "Type functions", e.g. haskell functors)

    We then need to prove that "Sum" is a bifunctor on TYPES, so we can implement
        tyfun BiLift :: Sum f g -> v -> Sum (f v) (g v)     -- Kind! signature
            BiLift = ???
    
    And finally let it add two algebraic data types:
        data T1 a = ...
        data T2 a = ...
        type TSum a = BiLift (T1 :+: T2) a  -- This must be a type
    
    Reading: https://strathprints.strath.ac.uk/33726/1/ghani_popl08.pdf
        for the higher-order functors
-}

-- Natural transformations:
type Nat g h = forall a. g a -> h a

-- Higher-order functors
class HFunctor f where
    ffmap :: Functor g => (a -> b) -> (f g) a -> (f g) b
    hfmap :: Nat g h -> Nat (f g) (f h)

newtype Meh f a = Meh [f a]

instance HFunctor Meh where
    ffmap :: Functor g => (a -> b) -> Meh g a -> Meh g b
    ffmap f (Meh x) = Meh $ fmap (fmap f) x
    hfmap :: forall g h. (forall a. g a -> h a) -> (forall b. Meh g b -> Meh h b)
    hfmap f (Meh x) = Meh $ fmap f x
