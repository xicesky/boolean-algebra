
{- |
Description     : Misc tools
Stability       : experimental

Basic functions & definitions that have no home yet
-}
module Missing.Misc
    (   Alg
    ,   ConstFunctor(..)
    ) where

-- recursion-schemes
import Data.Functor.Foldable (cata)

{-----------------------------------------------------------------------------}
-- Missing bits

-- Why doesn't recusion-schemes define an algebra type?
-- (probably because there are so many different ones)
-- We are going to mostly use cata, so here we go

-- | Algebras for use with recursion-schemes 'cata'
type Alg f a = f a -> a

{- | Constant functor

A constant functor maps every type to a constant type.
-}
class Functor f => ConstFunctor f where
    -- | A constant functor maps every type to a constant type
    constmap :: forall a b. f a -> f b
