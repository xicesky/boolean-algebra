
-- | Basic functions & definitions that have no home yet
module Missing.Misc where

{-----------------------------------------------------------------------------}
-- Missing bits

-- Why doesn't recusion-schemes define an algebra type?
-- (probably because there are so many different ones)
-- We are going to mostly use cata, so here we go

type Alg f a = f a -> a

{- | Constant functor

A constant functor maps every type to a constant type
-}
class Functor f => ConstFunctor f where
    -- | A constant functor maps every type to a constant type
    constmap :: forall a b. f a -> f b
