
{-# LANGUAGE UndecidableInstances   #-}

{- |
Description     : Safe injection
Stability       : experimental

This module leverages some /type family magic/ to safely
inject from terms to other terms.
-}
module Term.Inject
    (   (:<:)
    ,   inject
    ,   GInject(..)
    ) where

import GHC.TypeLits (TypeError(..), ErrorMessage(..))
import Data.Kind (Type)
import Data.Void
import Data.Proxy

import Term.Term

data Injection
    = IHere
    | IVoid     -- annoying special case for Void
    | IBoth Injection Injection
    | IDeconstruct [Injection]
    | ILift1 Injection

{- | Find an injection s -> t

This type family has to be closed to not cause conflicts (because of Fix4).
-}
type family FindInjection s t :: Injection where
    FindInjection a a       = 'IHere
    FindInjection Void a    = 'IVoid
    FindInjection (Fix4 f a b c) (Fix4 f a' b' c')
                            = 'ILift1 (FindInjection1 (f a b c) (f a' b' c'))
    FindInjection Bool (Term op Bool var)       -- special rule #1: Bool to Term
                            = 'IHere
    FindInjection (Either a b) c                -- special rule #2: Either
                            = 'IBoth (FindInjection a c) (FindInjection b c)
    FindInjection x y       = TypeError
        (       'Text "Could not find an injection from "
        ':$$:   'Text "\t" ':<>: 'ShowType x
        ':$$:   'Text "to"
        ':$$:   'Text "\t" ':<>: 'ShowType y
        )

{- | Find an injection s a -> t a

This type family could be open to allow different implementations of Op.
-}
type family FindInjection1 (s :: Type -> Type) (t :: Type -> Type) :: Injection where
    FindInjection1 (Op uop bop flop) (Op uop' bop' flop')
        = 'IDeconstruct
        [   FindInjection uop uop'
        ,   FindInjection bop bop'
        ,   FindInjection flop flop'
        ]
    FindInjection1 (TermF op val var) (TermF op' val' var')
        = 'IDeconstruct
        [   FindInjection1 op op'
        ,   FindInjection val val'
        ,   FindInjection var var'
        ]

-- | Type-family guided injection.
class GInject s t (i :: Injection) where
    gInject :: Proxy i -> s -> t

instance GInject a a 'IHere where
    gInject _ = id

instance GInject Void a 'IVoid where
    gInject _ = absurd

class GInject1 s t (i :: Injection) where
    gInject1 :: Proxy i -> s a -> t a

instance (GInject uop uop' ia, GInject bop bop' ib, GInject flop flop' ic
    , ProperOpTag uop', ProperOpTag bop', ProperOpTag flop'
    )
    => GInject1 (Op uop bop flop) (Op uop' bop' flop') ('IDeconstruct [ia, ib, ic]) where
    gInject1 _ (UnaryOp o t) = UnaryOp (gInject @uop @uop' @ia Proxy o) t
    gInject1 _ (BinaryOp o t1 t2) = BinaryOp (gInject @bop @bop' @ib Proxy o) t1 t2
    gInject1 _ (FlatOp o t) = FlatOp (gInject @flop @flop' @ic Proxy o) t

instance (GInject1 op op' ia, GInject val val' ib, GInject var var' ic
    , ProperRecT op'
    )
    => GInject1 (TermF op val var) (TermF op' val' var') ('IDeconstruct [ia, ib, ic]) where
    gInject1 _ (RecT t) = RecT (gInject1 @op @op' @ia Proxy t)
    gInject1 _ (ConstT v) = ConstT (gInject @val @val' @ib Proxy v)
    gInject1 _ (VariableT v) = VariableT (gInject @var @var' @ic Proxy v)

instance (Functor (f a b c), GInject1 (f a b c) (f a' b' c') i)
    => GInject (Fix4 f a b c) (Fix4 f a' b' c') ('ILift1 i) where
    gInject _ = go where
        go (Fix4 f) = (Fix4
            . gInject1 @(f a b c) @(f a' b' c') @i Proxy
            . fmap go
            ) f

-- FindInjection Bool (Term op Bool var)       -- special rule #1: Bool to Term
--                         = 'IHere
-- FindInjection (Either a b) c                -- special rule #2: Either
--                         = 'IBoth (FindInjection a c) (FindInjection b c)

-- special rule #1: Bool to Term
instance GInject Bool (Term op Bool var) 'IHere where
    gInject _ = Val

-- special rule #2: Either
instance (GInject a c l, GInject b c r)
    => GInject (Either a b) c ('IBoth l r) where
    gInject _ (Left v)  = gInject @a @c @l Proxy v
    gInject _ (Right v) = gInject @b @c @r Proxy v

-- | @s :<: t@ signifies that s is a "subtype" of t and can be
-- safely injected
type (s :<: t) = (GInject s t (FindInjection s t))

-- | Safely inject type @s -> t@
inject :: forall s t. (s :<: t) => s -> t
inject = gInject @s @t @(FindInjection s t) Proxy
