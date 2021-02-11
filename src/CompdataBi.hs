
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-}

-- Extensions for compdata usage
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module CompdataBi where

import CustomCompData
-- import Data.Comp.Term
-- import Data.Comp.Ops
-- import Data.Comp.Sum (inject, split)
-- import Data.Comp.Algebra
--     (   Alg, Coalg, RAlg, RCoalg
--     ,   cata, ana, para, apo
--     )

-- import Data.Comp.Derive
-- --import Data.Comp.Derive.Show
-- import Data.Comp.Show ()            -- for the Show instance
-- import Data.Comp.Equality ()        -- for the Eq instance

import Data.Proxy

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

{- We need proxys to make sure Haskell knows the type
    of (BooleanValue True) when using functions like
    "inject"
-}

data BooleanValue v e = BooleanValue Bool
    deriving (Show, Functor)

data BooleanVariable v e = BVariable v
    deriving (Show, Functor)

data BooleanAnd v e = BAnd e e
    deriving (Show, Functor)

{-
    • Could not deduce (Subsume
                          (compdata-0.12.1:Data.Comp.SubsumeCommon.ComprEmb
                             (Elem (BooleanAnd v0) f))
                          (BooleanAnd v0)
                          f)
      from the context: Subsume
                          (compdata-0.12.1:Data.Comp.SubsumeCommon.ComprEmb
                             (Elem (BooleanAnd v) f))
                          (BooleanAnd v)
                          f
        bound by the inferred type for ‘iBAnd’:
                   forall v (f :: * -> *) h a.
                   Subsume
                     (compdata-0.12.1:Data.Comp.SubsumeCommon.ComprEmb
                        (Elem (BooleanAnd v) f))
                     (BooleanAnd v)
                     f =>
                   Cxt h f a -> Cxt h f a -> Cxt h f a
        at src/CompdataBi.hs:(37,3)-(49,5)
      The type variable ‘v0’ is ambiguous
    • In the ambiguity check for the inferred type for ‘iBAnd’
      To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
      When checking the inferred type
        iBAnd :: forall v (f :: * -> *) h a.
                 Subsume
                   (compdata-0.12.1:Data.Comp.SubsumeCommon.ComprEmb
                      (Elem (BooleanAnd v) f))
                   (BooleanAnd v)
                   f =>
                 Cxt h f a -> Cxt h f a -> Cxt h f a
-}

-- inject :: (g :<: f) => g (Cxt h f a) -> Cxt h f a            -- normal compdata
-- inject :: (g :<: f) => g v (Cxt h f v a) -> Cxt h f v a      -- custom compdata

iBooleanValue :: (BooleanValue :<: f) => Bool -> Term f v
iBooleanValue b = inject (BooleanValue b)

iBVariable :: (BooleanVariable :<: f) => v -> Term f v
iBVariable v = inject (BVariable v)

iBAnd :: (BooleanAnd :<: f) => Term f v -> Term f v -> Term f v
iBAnd a b = inject (BAnd a b)

iBVar :: (BooleanVariable :<: f) => v -> Term f v
iBVar = iBVariable

-- iBooleanValue x_a5va x_a5vb = inject ((BooleanValue x_a5va) x_a5vb)
-- iBVariable x_a5ve = inject (BVariable x_a5ve)
-- iBAnd x_a5vh x_a5vi x_a5vj = inject (((BAnd x_a5vh) x_a5vi) x_a5vj)

--FIXME: https://stackoverflow.com/questions/30058292/handling-ambiguous-types-in-composable-free-dsls

-- $(derive
--     [ makeTraversable
--     , makeFoldable
--     , makeEqF
--     , makeShowF
--     , smartConstructors
--     ]
--     [''BooleanValue
--     ,''BooleanVariable
--     -- ,''BooleanNot
--     ,''BooleanAnd
--     -- ,''BooleanOr
--     ])

-- "derived" instances
instance ShowF (BooleanValue v) where
    showF :: BooleanValue v String -> String
    showF = show

instance Show v => ShowF (BooleanVariable v) where
    showF :: BooleanVariable v String -> String
    showF = show

instance ShowF (BooleanAnd v) where
    showF :: BooleanAnd v String -> String
    showF (BAnd a b) = "(Band " ++ a ++ " " ++ b ++ ")"

-- Base functor (called "Signature" in compdata)
type BooleanBaseF
    = BooleanValue
    :+: BooleanVariable
    -- :+: BooleanNot
    :+: BooleanAnd
    -- :+: BooleanOr

-- "Standard" boolean expressions
-- use ⊤, ⊥, ¬, ∧, ∨ and variables
type BooleanExpr v = Term BooleanBaseF v

exampleExpr01 :: BooleanExpr String
exampleExpr01 = (iBVar "x" `iBAnd` iBVar "y") `iBAnd` iBVar "z"

{- Idea:
What property of (:+:) makes this work?
Can we use PolyKinds to extend this to any Kind of f/v?
    Functor f, Functor g => Functor (f :+: g)
    Functor (f v), Functor (g v) => Functor (f :+: g) v
    Foldable f, Foldable g => Foldable (f :+: g)
    (Foldable (f v), Foldable (g v)) => Foldable ((f :+: g) v)

Is (:+:) really distributive over application?
    (f :+: g) x ~ (f x :+: g x)
    
-}
