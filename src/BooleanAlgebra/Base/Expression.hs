
{-# LANGUAGE PatternSynonyms        #-}

{- |
Description     : Boolean algebra datatype
Stability       : experimental

-}
module BooleanAlgebra.Base.Expression
    (   -- * Terms and operations
        BOps, BFlOps, BNOps
    ,   BooleanExpr
    ,   invertOp

    ,   -- * Literals
        Literal
    ,   TermLit(..)
    ,   substVarsL

    ,   -- * Clauses and CNF
        Conjunction(..), Disjunction(..)
    ,   CNF(..)
    ,   distributeDisjunction
    ,   joinConjunction
    ,   joinDisjunction

    ,   -- * Operation tags
        BooleanUOp(..), BooleanBOp(..), BooleanFlatOp(..)

    ,   -- * Pattern synonyms
        pattern BNot, pattern BAnd, pattern BOr
    ,   pattern BConj, pattern BDisj
    ,   pattern Lit

    ) where

import Prelude hiding (and, or, not, (&&), (||))
import qualified Prelude as P

import Data.Kind (Type)
import Data.Void
import Control.Applicative (Alternative(..))

import Missing.Void
import Term.Term
import Term.Substitution
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
type BFlOps = Op Void Void BooleanFlatOp

-- | "Standard" boolean expressions
-- use ⊤, ⊥, ¬, ∧, ∨ and variables
type BooleanExpr            = Term BOps Bool

-- | Boolean expressions with flattened operators
--type BooleanExprFlat        = Term BFlOps Bool

-- Dumb idea? This looks very nice because the Bool gets /factored out/ in the type!
--type BooleanValue = Term VoidF Bool Void

{-----------------------------------------------------------------------------}
-- Pattern synonyms

pattern BNot
    :: () => (ProperRecT (Op BooleanUOp b c), ProperOpTag BooleanUOp)
    => Term (Op BooleanUOp b c) val var
    -> Term (Op BooleanUOp b c) val var
pattern BAnd
    :: () => (ProperRecT (Op a BooleanBOp c), ProperOpTag BooleanBOp)
    => Term (Op a BooleanBOp c) val var
    -> Term (Op a BooleanBOp c) val var
    -> Term (Op a BooleanBOp c) val var
pattern BOr
    :: () => (ProperRecT (Op a BooleanBOp c), ProperOpTag BooleanBOp)
    => Term (Op a BooleanBOp c) val var
    -> Term (Op a BooleanBOp c) val var
    -> Term (Op a BooleanBOp c) val var

pattern BNot a      = Fix4 (RecT (UnaryOp BooleanNot a))
pattern BAnd a b    = Fix4 (RecT (BinaryOp BooleanAnd a b))
pattern BOr  a b    = Fix4 (RecT (BinaryOp BooleanOr a b))

pattern BConj :: () => (ProperRecT (Op a b1 BooleanFlatOp), ProperOpTag BooleanFlatOp)
    => [Fix4 TermF (Op a b1 BooleanFlatOp) b2 c] -> Fix4 TermF (Op a b1 BooleanFlatOp) b2 c
pattern BConj xs    = Fix4 (RecT (FlatOp BConjunction xs))

pattern BDisj :: () => (ProperRecT (Op a b1 BooleanFlatOp), ProperOpTag BooleanFlatOp)
    => [Fix4 TermF (Op a b1 BooleanFlatOp) b2 c] -> Fix4 TermF (Op a b1 BooleanFlatOp) b2 c
pattern BDisj xs    = Fix4 (RecT (FlatOp BDisjunction xs))


{-# COMPLETE Var, Val, BNot, BAnd, BOr, BFlOp #-}
{-# COMPLETE Var, Val, BNot, BAnd, BOr, BConj, BDisj #-}

{-----------------------------------------------------------------------------}

instance PreBoolean (Term BOps val var) where
    not a   = BNot a

instance Boolean (Term BOps val var) where
    and a b = BAnd a b
    or a b  = BOr a b

instance BooleanArithmetic (Term BOps Bool var) where
    fromBool = Val

instance BooleanPreAlgebra (Term BOps val String) where
    var = Var

instance BooleanAlgebra (Term BOps Bool String)

instance InterpretBooleanArithmetic (Term BOps Bool Void) where
    interpretArith (Val v)      = fromBool v
    interpretArith (Var v)      = absurd v
    interpretArith (BNot x)     = not (interpretArith x)
    interpretArith (BAnd a b)   = and (interpretArith a) (interpretArith b)
    interpretArith (BOr a b)    = or (interpretArith a) (interpretArith b)

instance InterpretBooleanAlgebra (Term BOps Bool String) where
    interpretAlg (Val v)    = fromBool v
    interpretAlg (Var v)    = var v
    interpretAlg (BNot x)   = not (interpretAlg x)
    interpretAlg (BAnd a b) = and (interpretAlg a) (interpretAlg b)
    interpretAlg (BOr a b)  = or (interpretAlg a) (interpretAlg b)

{-----------------------------------------------------------------------------}
-- Various little helpers

invertOp :: BooleanBOp -> BooleanBOp
invertOp BooleanAnd = BooleanOr
invertOp BooleanOr = BooleanAnd

{-----------------------------------------------------------------------------}
-- Boolean literals

-- | A /literal/ is a variable with a sign
type Literal a = (Bool, a)

-- Pattern, for example @Lit "x"@ or @not (Lit "x")@
pattern Lit :: a -> Literal a
pattern Lit a   <- (_, a) where
    Lit a = (True, a)

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
-- Conjunction / Disjunction clauses and CNF
-- TODO: These probably deserve their own module and maybe "Monad" instances?

-- | Boolean conjunctions of arbitrary length
-- a.k.a. "flattened" and expressions
newtype Conjunction e = Conjunction { unConjunction :: [e] }
    deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Boolean disjunctions of arbitrary length
-- a.k.a. "flattened" or expressions
newtype Disjunction e = Disjunction { unDisjunction :: [e] }
    deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Conjunctive normal form
newtype CNF name = CNF { unCNF :: Conjunction (Disjunction (Literal name)) }

deriving instance Show name => Show (CNF name)
deriving instance Eq name => Eq (CNF name)
deriving instance Functor CNF
deriving instance Foldable CNF
deriving instance Traversable CNF

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

distributeDisjunction :: Disjunction (Conjunction e) -> Conjunction (Disjunction e)
distributeDisjunction = sequenceA       -- Well, isn't this easy.

joinConjunction :: Conjunction (Conjunction e) -> Conjunction e
joinConjunction (Conjunction xs) = -- Monad.join
    Conjunction [y | Conjunction x <- xs, y <- x]

joinDisjunction :: Disjunction (Disjunction e) -> Disjunction e
joinDisjunction (Disjunction xs) = -- Monad.join
    Disjunction [y | Disjunction x <- xs, y <- x]
