
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-} 

-- Extensions for compdata usage
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module BooleanAlgebra.Base where

{- Inspiration:
    https://tuprints.ulb.tu-darmstadt.de/2759/1/rkibria-dissertation-final-korrigiert1.pdf
    https://bartoszmilewski.com/2017/02/28/f-algebras/
    https://www-ps.informatik.uni-kiel.de/~sebf/projects/sat-solver/Control/Monad/Constraint/Boolean.lhs.html
    Recursion schemes: https://blog.sumtypeofway.com/archive.html
    Or as video: https://www.youtube.com/watch?v=Zw9KeP3OzpU
    Datatypes a la carte and followups:
        http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf
        http://www.cs.nott.ac.uk/~pszgmh/alacarte.pdf
-}

--import Data.Comp
import Data.Comp.Term
import Data.Comp.Ops
import Data.Comp.Sum (inject, split)
import Data.Comp.Algebra
    (   Alg, Coalg, RAlg, RCoalg
    ,   cata, ana, para, apo
    )

import Data.Comp.Derive
--import Data.Comp.Derive.Show
import Data.Comp.Show ()            -- for the Show instance
import Data.Comp.Equality ()        -- for the Eq instance

import BooleanAlgebra.THUtil

{-----------------------------------------------------------------------------}
-- Thinking

{-----------------------------------------------------------------------------}
-- Basic boolean expressions

-- Boolean Values
data BooleanValue e = BTrue | BFalse
    deriving (Show, Functor)

-- Variables
data BooleanVariable e = BVariable String   -- BIG YIKES! FIXME: Need to rewrite compdata
    deriving Functor

-- Boolean negation
data BooleanNot e = BNot e
    deriving Functor

-- Boolean conjunction
data BooleanAnd e = BAnd e e
    deriving Functor

-- Boolean disjunction
data BooleanOr e = BOr e e
    deriving Functor

{- Automatically generate everything we need. To see what's being
    generated by template haskell, run:
    stack ghci --ghci-options="-ddump-splices"

    We get smart constructors like:
        iBNot :: (BooleanNot :<: f) => Ctx h f a -> Ctx h f a
-}
$(deriveDefault
    [''BooleanValue
    ,''BooleanVariable
    ,''BooleanNot
    ,''BooleanAnd
    ,''BooleanOr
    ])

-- Shorthand names for some constructors
iBVar :: (BooleanVariable :<: f) => String -> Cxt h f a
iBVar = iBVariable

-- Base functor (called "Signature" in compdata)
type BooleanBaseF
    = BooleanValue
    :+: BooleanVariable
    :+: BooleanNot
    :+: BooleanAnd
    :+: BooleanOr

-- "Standard" boolean expressions
-- use ⊤, ⊥, ¬, ∧, ∨ and variables
type BooleanExpr = Term BooleanBaseF

-- Show instance for BooleanExpr already exists in Data.Comp.Show!

exampleExpr01 :: BooleanExpr
exampleExpr01 = (iBNot (iBVar "x" `iBAnd` iBVar "y")) `iBAnd` iBVar "z"

{-----------------------------------------------------------------------------}
-- Simplified form
-- without literal values (True, False)

-- BooleanExprF without BooleanValue
type BooleanExprSimpF
    =   BooleanVariable
    :+: BooleanNot
    :+: BooleanAnd
    :+: BooleanOr

-- Simplified boolean expressions are either just "true" or "false"
-- or terms without any boolean values
type BooleanExprSimp = Either (BooleanValue ()) (Term BooleanExprSimpF)

{-----------------------------------------------------------------------------}
-- Boolean "literal" form
-- Literal = Variable + optional Negation

data BooleanLit e = BooleanLit Bool String
    deriving (Show, Eq, Functor)
    {- NOTE: For this construction deriveDefault apparently
    can't work out Show and Eq -}

$(deriveDefault [''BooleanLit] )

-- Shorthands
iPos :: (BooleanLit :<: f) => String -> Cxt h f a
iPos = iBooleanLit True

iNeg :: (BooleanLit :<: f) => String -> Cxt h f a
iNeg = iBooleanLit False

-- BooleanExpr without BooleanValue, BooleanVariable, BooleanNot
--  but using BooleanLit
type BooleanExprLitF
    =   BooleanLit
    :+: BooleanAnd
    :+: BooleanOr

type BooleanExprLit = Term BooleanExprLitF

{-----------------------------------------------------------------------------}
-- Aggregate form

{- BooleanCD: Conjunction over Disjunctions
    BooleanCD [[a,b],[c,d]] ≅ (a ∨ b) ∧ (c ∨ d)
-}
data BooleanCD e = BooleanCD [[e]]
    --deriving Functor
    deriving (Show, Eq, Functor)

-- Don't derive ShowF for BooleanCD, see bug description in Aggregate.hs
$(deriveNoShow [''BooleanCD])

-- BooleanExprLit where BooleanCD replaces (BooleanAnd, BooleanOr)
type BooleanExprCDLitF
    =   BooleanLit
    :+: BooleanCD

type BooleanExprCDLit = Term BooleanExprCDLitF

{-----------------------------------------------------------------------------}
-- Conjunctive normal form (CNF)

type CNF = BooleanCD (BooleanLit ())

-- Shorthands
lPos :: String -> BooleanLit a
lPos = BooleanLit True

lNeg :: String -> BooleanLit a
lNeg = BooleanLit False
