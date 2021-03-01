
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

import Data.Kind (Type)
import Data.Void
import Control.Applicative (Alternative(..))

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
import BooleanAlgebra.Class
import qualified BooleanAlgebra.Class as B

{-----------------------------------------------------------------------------}
-- Annotations for HLint

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

{-----------------------------------------------------------------------------}
-- General utilities

class Functor f => ConstFunctor f where
    constmap :: forall a b. f a -> f b

{-----------------------------------------------------------------------------}
-- Basic boolean expressions

-- Boolean Values

type BooleanValue :: Type -> Type
data BooleanValue e = BTrue | BFalse
    deriving (Show, Eq, Functor)

-- Variables
type BooleanVariable :: Type -> Type
data BooleanVariable e = BVariable String   -- BIG YIKES! FIXME: Need to rewrite compdata
    deriving (Show, Eq, Functor)

-- Boolean negation
data BooleanNot e = BNot e
    deriving (Show, Eq, Functor)

-- Boolean conjunction
data BooleanAnd e = BAnd e e
    deriving (Show, Eq, Functor)

-- Boolean disjunction
data BooleanOr e = BOr e e
    deriving (Show, Eq, Functor)

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

instance ConstFunctor BooleanValue where
    constmap BTrue = BTrue
    constmap BFalse = BFalse

instance ConstFunctor BooleanVariable where
    constmap (BVariable s) = BVariable s

-- Shorthand names for some constructors
iBVar :: (BooleanVariable :<: f) => String -> Cxt h f a
iBVar = iBVariable

-- Useful inverses
varName :: BooleanVariable a -> String
varName (BVariable n) = n

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

instance Boolean BooleanExpr where
    and = iBAnd
    or = iBOr
    not = iBNot

instance BooleanAlgebra BooleanExpr where
    var = iBVar

instance Boolean (BooleanValue a) where
    and BTrue  x = x
    and BFalse _ = BFalse
    or  BTrue  _ = BTrue
    or  BFalse x = x
    not BTrue    = BTrue
    not BFalse   = BFalse

{-----------------------------------------------------------------------------}
-- Simplified form
-- without literal values (True, False)

-- BooleanExprF without BooleanValue
type BooleanExprSimpF
    =   BooleanVariable
    :+: BooleanNot
    :+: BooleanAnd
    :+: BooleanOr

-- The type of "just boolean values"
type JustBooleanValue = BooleanValue Void

-- Modifier for terms so they can hold trivial values (True, False)
type MaybeTrivial f = Either JustBooleanValue f

-- Simplified boolean expressions are either just "true" or "false"
-- or terms without any boolean values
type BooleanExprSimp = MaybeTrivial (Term BooleanExprSimpF)

instance Boolean (Term BooleanExprSimpF) where
    and = iBAnd
    or = iBOr
    not = iBNot

instance BooleanAlgebra (Term BooleanExprSimpF) where
    var = iBVar

instance Boolean BooleanExprSimp where
    and (Left x) (Left y)       = Left $ x `B.and` y
    and (Right x) (Right y)     = Right $ x `B.and` y
    and (Left BTrue) (Right y)  = Right y
    and (Left BFalse) (Right _) = Left BFalse
    and (Right x) left          = B.and left (Right x)

    or  (Left x) (Left y)       = Left $ x `B.or` y
    or  (Right x) (Right y)     = Right $ x `B.or` y
    or  (Left BTrue) (Right _)  = Left BTrue
    or  (Left BFalse) (Right y) = Right y
    or  (Right x) left          = B.or left (Right x)
    
    not (Left x)                = Left $ B.not x
    not (Right x)               = Right $ B.not x

{-----------------------------------------------------------------------------}
-- Boolean "literal" form
-- Literal = Variable + optional Negation

type BooleanLit :: Type -> Type
data BooleanLit e = BooleanLit Bool String
    deriving (Show, Eq, Functor)
    {- NOTE: For this construction deriveDefault apparently
    can't work out Show and Eq -}

$(deriveDefault [''BooleanLit])

instance ConstFunctor BooleanLit where
    constmap (BooleanLit b s) = BooleanLit b s

-- Shorthands
lPos :: String -> BooleanLit a
lPos = BooleanLit True

lNeg :: String -> BooleanLit a
lNeg = BooleanLit False

iPos :: (BooleanLit :<: f) => String -> Cxt h f a
iPos = iBooleanLit True

iNeg :: (BooleanLit :<: f) => String -> Cxt h f a
iNeg = iBooleanLit False

litName :: BooleanLit e -> String
litName (BooleanLit _ n) = n

-- BooleanExpr without BooleanValue, BooleanVariable, BooleanNot
--  but using BooleanLit
type BooleanExprLitF
    =   BooleanLit
    :+: BooleanAnd
    :+: BooleanOr

type BooleanExprLit = Term BooleanExprLitF

{-----------------------------------------------------------------------------}
-- Aggregate form

-- | Boolean conjunctions of arbitrary length
-- a.k.a. "flattened" and expressions
data Conjunction e = Conjunction [e]
    deriving (Show, Eq, Functor)

-- | Boolean disjunctions of arbitrary length
-- a.k.a. "flattened" or expressions
data Disjunction e = Disjunction [e]
    deriving (Show, Eq, Functor)

-- Don't derive ShowF for aggregates, see bug description in Pretty.hs
$(deriveNoShow [''Conjunction])
$(deriveNoShow [''Disjunction])

instance Applicative Conjunction where
    pure = Conjunction . pure
    (<*>) (Conjunction a) (Conjunction b)
        = Conjunction (a <*> b)

instance Alternative Conjunction where
    empty = Conjunction empty
    (<|>) (Conjunction a) (Conjunction b)
        = Conjunction (a <|> b)

instance Applicative Disjunction where
    pure = Disjunction . pure
    (<*>) (Disjunction a) (Disjunction b)
        = Disjunction (a <*> b)

instance Alternative Disjunction where
    empty = Disjunction empty
    (<|>) (Disjunction a) (Disjunction b)
        = Disjunction (a <|> b)

-- | Flattened BooleanExprLit
type BooleanExprFlatLitF
    =   BooleanLit
    :+: Conjunction
    :+: Disjunction

type BooleanExprFlatLit = Term BooleanExprFlatLitF

-- TODO: This could be done but needs a bigger impl which doesn't belong here!
-- instance Boolean (BooleanExprFlatLit) where
--     and = iBAnd
--     or = iBOr
--     not = iBNot

{-----------------------------------------------------------------------------}
-- Conjunctive normal form (CNF)

type CNF = Conjunction (Disjunction (BooleanLit Void))

-- And another little shortcut
cnfFromList :: [[BooleanLit a]] -> CNF
cnfFromList = Conjunction . fmap (Disjunction . fmap constmap)

{-----------------------------------------------------------------------------}
-- Numbered variables

{- This is a hack more than anything else - i would rather abstract the type
    of variables and then just "fmap" over the whole formula. But alas,
    compdata can't handle that (yet).
-}

data BooleanVarI e = BVariableI Int
    deriving (Show, Eq, Functor)

$(deriveDefault [''BooleanVarI])

-- Shorthand names for some constructors
iBVarI :: (BooleanVarI :<: f) => Int -> Cxt h f a
iBVarI = iBVariableI
