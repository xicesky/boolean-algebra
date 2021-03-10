

{- |
Description     : Void handling
Stability       : experimental

Utilities to work with the 'Void' type and other - isomorphic - types.
-}
module Missing.Void
    (   -- * IsoVoid
        IsoVoid(..)
    ,   -- * Void functor
        VoidF
    ) where

import Data.Void
import Data.Functor.Const

-- The ever looming void functor

-- | Void functor, @VoidF a ≅ Void@
type VoidF = Const Void

-- | Types that are actually the empty set, i.e. isomorphic to Void
class IsoVoid a where
    -- | Polymorphic version of 'absurd'
    absurd' :: a -> b

instance IsoVoid Void where
    absurd' = absurd

instance IsoVoid (VoidF a) where
    absurd' = absurd . getConst
