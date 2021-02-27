
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-} 

-- Extensions for compdata usage
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE GADTs                  #-}

module BooleanAlgebra.Util.CompdataUtils where

import Data.Comp.Term
import Data.Comp.Ops
import Data.Comp.Sum (inject, deepInject)
import Data.Comp.Algebra
    (   Alg, Coalg, RAlg, RCoalg
    ,   cata, ana, para, apo
    ,   Hom, CxtFun
    ,   appCxt, appHom
    )

idHom :: forall g b. (Functor g) => g b -> Context g b
idHom = Term . fmap Hole

{- | Given a algebra that substitutes @f@ by some term in @g@,
    substitute all occurances of f in a term.
    @f@ may not occur in @g@.
-}
substAll :: forall f g a'. (Functor f, Functor g) =>
    (forall a. f (Context g a) -> Context g a)
    -> Context (f :+: g) a' -> Context g a'
substAll onF = let
    onFHom :: forall b. f b -> Context g b
    onFHom = onF . fmap Hole
    in appHom $ caseF onFHom idHom

{- | Given a algebra that substitutes @f@ by some term in @f :+: g@,
    substitute all occurances of f in a term.
    In contrast to substAll, f occurs in the output and can thus just
    be preserved if needed.
-}
-- substSome :: forall f g a'. (Functor f, Functor g) =>
--     (forall a. f (Context (f :+: g) a) -> Context (f :+: g) a)
--     -> Context (f :+: g) a' -> Context (f :+: g) a'
-- substSome onF = let
--     onFHom :: forall b. f b -> Context (f :+: g) b
--     onFHom = onF . fmap Hole

--     -- Error: Could not deduce Subsume (...) g1 (f :+: g1)
--     meh :: forall g b. (Functor g) => g b -> Context (f :+: g) b
--     meh = let
--         idHom0 :: g b -> Context g b
--         idHom0 = idHom
--         deepInject0 :: (g :<: (f :+: g)) => Context g b -> Context (f :+: g) b
--         deepInject0 = deepInject

--         in deepInject0 . idHom0     -- Not possible, Elem f (f :+: g) is undetermined!

--     almost :: forall b. (f :+: g) b -> Context (f :+: g) b
--     almost = caseF onFHom meh

--     in appHom $ almost

substSome :: forall f g a'. (Functor f, Functor g, g :<: (f :+: g)) -- Weird constraint necessary
    => (forall a. f (Context (f :+: g) a) -> Context (f :+: g) a)   -- Replacement function
    -> CxtFun (f :+: g) (f :+: g)                                   -- Transformation
substSome onF = let
    onFHom :: forall b. f b -> Context (f :+: g) b
    onFHom = onF . fmap Hole
    pass :: forall b. (Functor g) => g b -> Context (f :+: g) b
    pass = deepInject . idHom
    in appHom $ caseF onFHom pass

{- Another thing that can't work with compdata:
    here the variable g should be inferred, but there are ambiguous
    variables all inbetween.
-}
-- substVars :: forall f g a. (Functor f, Functor g, f :=: BooleanVariable :+: g)
--     => (BooleanVariable a -> Context f a) -> CxtFun f f
-- substVars fVar = let
--     nothing :: g a -> Context f a
--     nothing = _
--     both :: f a -> Context f a
--     both = spl fVar nothing
--     in appHom both
