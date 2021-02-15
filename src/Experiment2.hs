
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-}

-- Type magic
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE LiberalTypeSynonyms    #-}

module Experiment2 where

{-  Interesting construction by Bartosz Milewski in
    https://stackoverflow.com/questions/48488021/once-i-have-an-f-algebra-can-i-define-foldable-and-traversable-in-terms-of-it
-}

newtype Fix f = Fix { unFix :: f (Fix f) }
-- newtype FixBi f a = FixBi { unFixBi :: f a (FixBi f a) }
--newtype Fix1 f a = Fix1 (Fix (f a))
--newtype Fix2 f a b = Fix2 (Fix (f a b))
type Fix1 f a = Fix (f a)
type Fix2 f a b = Fix (f a b)

data ListBiF a r
    = Nil
    | Cons a r
    deriving (Show, Eq, Functor)

nil = Fix Nil
cons a b = Fix $ Cons a b

type List a = Fix1 ListBiF a

type Algebra f a = f a -> a
type Coalgebra f a = a -> f a

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . unFix

cata1 :: Functor (f v) => (f v a -> a) -> Fix1 f v -> a
cata1 f = f . fmap (cata f) . unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana f = Fix . fmap (ana f) . f 

ana1 :: Functor (f v) => (a -> f v a) -> a -> Fix1 f v
ana1 f = Fix . fmap (ana1 f) . f

tAlg :: Applicative f => (a -> f b) -> Algebra (ListBiF a) (f (List b))
tAlg g (Nil     ) = pure nil
tAlg g (Cons v r) = cons <$> g v <*> r
    -- v : a, r : f (List b), g : a -> f b
    -- Cons <$> g v <*> sequenceA r

traverse' :: Applicative f => (a -> f b) -> Fix (ListBiF a) -> f (List b)
traverse' = cata . tAlg
