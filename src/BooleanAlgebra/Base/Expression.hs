
{-# LANGUAGE PatternSynonyms        #-}

module BooleanAlgebra.Base.Expression where

import Data.Kind (Type)
import Data.Void
import Control.Applicative (Alternative(..))

import Container
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import Data.Comp
import Data.Comp.Derive
import Data.Comp.Show ()            -- for the Show instance
import Data.Comp.Equality ()        -- for the Eq instance

import BooleanAlgebra.Util.THUtil
import BooleanAlgebra.Util.Util
import BooleanAlgebra.Base.Class
import qualified BooleanAlgebra.Base.Class as B

{-----------------------------------------------------------------------------}
-- Annotations for HLint

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

{-----------------------------------------------------------------------------}
-- Components of boolean terms (expressions)

-- | Boolean Values (as part of terms)
type BooleanValue :: Type -> Type
data BooleanValue e = BooleanValue Bool
    deriving (Show, Eq, Functor)

pattern BTrue = BooleanValue True
pattern BFalse = BooleanValue False

-- | Variables
type BooleanVariable :: Type -> Type
data BooleanVariable e = BVariable String
    deriving (Show, Eq, Functor)

-- | Literals: A variable with a sign
type BooleanLit :: Type -> Type
data BooleanLit e = BooleanLit Int
    deriving (Show, Eq, Functor)

-- | Boolean negation
data BooleanNot e = BNot e
    deriving (Show, Eq, Functor)

-- | Boolean binary operators
data BooleanOp e
    = BAnd e e                              -- ^ Conjunction
    | BOr e e                               -- ^ Disjunction
    deriving (Show, Eq, Functor)

-- | Boolean conjunctions of arbitrary length
-- a.k.a. "flattened" and expressions
data Conjunction e = Conjunction [e]
    deriving (Show, Eq, Functor)

-- | Boolean disjunctions of arbitrary length
-- a.k.a. "flattened" or expressions
data Disjunction e = Disjunction [e]
    deriving (Show, Eq, Functor)

{-----------------------------------------------------------------------------}

-- | Modifier for terms that can not otherwise hold trivial values (True, False)
type MaybeTrivial f = Either (BooleanValue Void) f

{-----------------------------------------------------------------------------}
-- Putting it together using compdata

$(deriveDefault
    [''BooleanValue
    ,''BooleanVariable
    ,''BooleanNot
    ,''BooleanOp
    ,''BooleanLit
    ])

-- Don't derive ShowF for aggregates, see bug description in Pretty.hs
$(deriveNoShow [''Conjunction])
$(deriveNoShow [''Disjunction])

-- Base functor (called "Signature" in compdata)
type BooleanBaseF
    = BooleanValue
    :+: BooleanVariable
    :+: BooleanNot
    :+: BooleanOp

-- "Standard" algebraic boolean expressions
-- use ⊤, ⊥, ¬, ∧, ∨ and variables
type BooleanExpr = Term BooleanBaseF

{-----------------------------------------------------------------------------}
-- Conjunctive normal form

-- | Conjunctive normal form (CNF)
type CNF = Conjunction (Disjunction (BooleanLit Void))

-- | Build CNF from a nested list of literals
cnfFromList :: [[BooleanLit a]] -> CNF
cnfFromList = Conjunction . fmap (Disjunction . fmap constmap)

-- TODO: inverse
-- cnfToList :: CNF -> [[BooleanLit a]]

{-----------------------------------------------------------------------------}
-- Utilities

-- | Inverse of 'BVariable'
unVar :: BooleanVariable a -> String
unVar (BVariable n) = n

-- | Inverse of 'BooleanValue'
unVal :: BooleanValue a -> Bool
unVal (BooleanValue b) = b

-- | Inverse of 'BooleanLit'
unLit :: BooleanLit e -> Int
unLit (BooleanLit n) = n

-- Shorthands

iVal :: (BooleanValue :<: f) => Bool -> Cxt h f a
iVal = iBooleanValue

iTrue :: (BooleanValue :<: f) => Cxt h f a
iTrue = iVal True

iFalse :: (BooleanValue :<: f) => Cxt h f a
iFalse = iVal False

iLit ::  (BooleanLit :<: f) => Int -> Cxt h f a
iLit = iBooleanLit

-- lPos :: String -> BooleanLit a
-- lPos = BooleanLit True

-- lNeg :: String -> BooleanLit a
-- lNeg = BooleanLit False

-- iPos :: (BooleanLit :<: f) => String -> Cxt h f a
-- iPos = iBooleanLit True

-- iNeg :: (BooleanLit :<: f) => String -> Cxt h f a
-- iNeg = iBooleanLit False

{-----------------------------------------------------------------------------}
-- ConstFunctor

instance ConstFunctor BooleanValue where
    constmap (BooleanValue b) = BooleanValue b

instance ConstFunctor BooleanVariable where
    constmap (BVariable i) = BVariable i

instance ConstFunctor BooleanLit where
    constmap (BooleanLit i) = BooleanLit i

{-----------------------------------------------------------------------------}
-- BooleanAlgebra class instances

instance Boolean (BooleanValue a) where
    and BTrue  x = x
    and BFalse _ = BFalse
    or  BTrue  _ = BTrue
    or  BFalse x = x
    not BTrue    = BTrue
    not BFalse   = BFalse

instance Boolean BooleanExpr where
    and = iBAnd
    or = iBOr
    not = iBNot

instance BooleanPreAlgebra BooleanExpr where
    var = iBVariable

instance BooleanArithmetic BooleanExpr where
    fromBool b = iBooleanValue b

instance BooleanAlgebra BooleanExpr

{-----------------------------------------------------------------------------}
-- Applicative / Alternative for Conjunction / Disjunction

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
