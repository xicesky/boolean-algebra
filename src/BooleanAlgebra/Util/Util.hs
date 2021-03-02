
-- | Various utilities that still have to be sorted out
module BooleanAlgebra.Util.Util where

-- | Constant functor
class Functor f => ConstFunctor f where
    -- | A constant functor maps every type to a constant type
    constmap :: forall a b. f a -> f b

