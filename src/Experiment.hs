
{-# LANGUAGE DeriveFunctor, DeriveTraversable #-}

module Experiment where

import Debug.Trace

class ShowF f where
    showF :: f String -> String

data BasicF a = BasicF a a a
    deriving (Show, Eq, Functor, Foldable, Traversable)

data RecursiveF a = RNil | RRec (BasicF a) (RecursiveF a)
    deriving (Show, Eq, Functor, Foldable, Traversable)

-- newtype Fix f = Fix { unFix :: f (Fix f) }

-- instance (Functor f, ShowF  => Show (Fix f) where
--     showsPrec d (Fix v) = showsPrec d v

-- type RCon a = Fix (RecursiveF a)

con1 :: RecursiveF Int
con1 = RRec (BasicF 1 2 3) $ RRec (BasicF 5 7 9) $ RNil
con2 :: [Int]
con2 = [1, 2, 3, 5, 7, 9]

deleteIfEven :: Integral a => a -> Maybe a
deleteIfEven x = if even x then Nothing else Just x

f1 :: Functor f => f Int -> f (Maybe Int)
f1 = fmap deleteIfEven

f2 :: Traversable f => f Int -> Maybe (f Int)
f2 = sequenceA . f1

f3 :: Traversable f => f Int -> Maybe (f Int)
f3 = traverse deleteIfEven
