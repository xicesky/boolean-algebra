
-- | Utilities for handling voids
module Missing.Void where

import Data.Void
import Data.Functor.Const

-- The ever looming void functor
type VoidF = Const Void

-- Types that are actually the empty set, i.e. isomorphic to Void
class IsoVoid a where
    absurd' :: a -> b

instance IsoVoid Void where
    absurd' = absurd

instance IsoVoid (VoidF a) where
    absurd' = absurd . getConst
