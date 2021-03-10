
{-# LANGUAGE PatternSynonyms        #-}

module BooleanAlgebra.Base.Expression where

import Prelude hiding (and, or, not, (&&), (||))
import qualified Prelude as P

import Data.Kind (Type)
import Data.Void
import Control.Applicative (Alternative(..))

import Missing.Void
import Term.Term
import BooleanAlgebra.Base.Class
import qualified BooleanAlgebra.Base.Class as B

{-----------------------------------------------------------------------------}
-- Annotations for HLint

{-# ANN module "HLint: ignore Use newtype instead of data" #-}

{-----------------------------------------------------------------------------}
-- Components of boolean terms (expressions)

data BooleanUOp = BooleanNot
    deriving (Show, Eq, Ord)
data BooleanBOp = BooleanAnd | BooleanOr
    deriving (Show, Eq, Ord)
data BooleanFlatOp = BConjunction | BDisjunction
    deriving (Show, Eq, Ord)

instance ProperOpTag BooleanBOp where
    opPrec BooleanAnd = 6
    opPrec BooleanOr = 3
    opName BooleanAnd = "BAnd"
    opName BooleanOr = "BOr"

instance ProperOpTag BooleanUOp where
    opPrec BooleanNot = 10
    opName BooleanNot = "BNot"

instance ProperOpTag BooleanFlatOp where
    opPrec BConjunction = 6
    opPrec BDisjunction = 3
    opName BConjunction = "BConj"
    opName BDisjunction = "BDisj"

-- Show1 instance for Op is below

type BOps = Op BooleanUOp BooleanBOp Void
type BFlOps = Op BooleanUOp Void BooleanFlatOp

-- | "Standard" boolean expressions
-- use ⊤, ⊥, ¬, ∧, ∨ and variables
type BooleanExpr            = Term BOps Bool

-- | Boolean expressions with flattened operators
type BooleanExprFlat        = Term BFlOps Bool

-- Dumb idea? This looks very nice because the Bool gets /factored out/ in the type!
--type BooleanValue = Term VoidF Bool Void

{-----------------------------------------------------------------------------}

instance PreBoolean (BooleanExpr a) where
    not a   = BNot a

instance Boolean (BooleanExpr a) where
    and a b = BAnd a b
    or a b  = BOr a b

instance BooleanArithmetic (BooleanExpr a) where
    fromBool = Val

instance BooleanPreAlgebra (BooleanExpr String) where
    var = Var

instance BooleanAlgebra (BooleanExpr String)

{-----------------------------------------------------------------------------}
-- Various little helpers

invertOp :: BooleanBOp -> BooleanBOp
invertOp BooleanAnd = BooleanOr
invertOp BooleanOr = BooleanAnd

{-----------------------------------------------------------------------------}
-- Boolean literals

-- | A /literal/ is a variable with a sign
type Literal a = (Bool, a)

instance PreBoolean (Literal a) where
    -- litNeg (b, v) = (not b, v)
    not (b, n)   = (not b, n)

-- | Terms over literals (usually eschewing negation)
newtype TermLit op val name  = TermLit { unTermLit :: Term op val (Literal name) }

deriving instance (Show val, Show name) => Show (TermLit op val name)
deriving instance (Eq val, Eq name) => Eq (TermLit op val name)
deriving instance Functor (TermLit op val)
deriving instance Foldable (TermLit op val)
deriving instance Traversable (TermLit op val)

-- Operations without negation (for use with TermLit)
type BNOps = Op Void BooleanBOp Void

-- | Substitute terms for literals
substVarsL :: forall var var' bop bflop val.
    (ProperOpTag bop, ProperOpTag bflop)
    => (var -> TermLit (Op BooleanUOp bop bflop) val var')
    -> TermLit (Op BooleanUOp bop bflop) val var
    -> TermLit (Op BooleanUOp bop bflop) val var'
substVarsL f = TermLit . substVars f' . unTermLit where
    f' :: Literal var -> Term (Op BooleanUOp bop bflop) val (Literal var')
    f' (True, name)  = unTermLit $ f name
    f' (False, name) = BNot $ unTermLit $ f name

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
