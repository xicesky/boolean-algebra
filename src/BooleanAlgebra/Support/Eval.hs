
{- |
Description     : Evaluation tools
Stability       : experimental

This module provides instances an evaluation function 'eval' and
tools to compare terms under evaluation (for use with QuickCheck).
-}
module BooleanAlgebra.Support.Eval
    (   -- * Evaluating terms
        Eval(..)

    ,   -- * Testing term equality
        equalEval
    ,   propEqual
    ,   propImplies

    ) where

import Prelude hiding (and, or, not, (&&), (||))

import Data.Kind (Type)
import Data.Void
import Data.Monoid (All(..), Any(..))

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- quickcheck
import Test.QuickCheck (Property, forAll, (===), (==>))

import Missing.Void
import Term.Term
import Term.Substitution
import Term.Inject

import BooleanAlgebra.Base.Class
import BooleanAlgebra.Base.Expression
import BooleanAlgebra.Transform.Simplify
import BooleanAlgebra.Transform.Variable
import BooleanAlgebra.Support.Arbitrary

import GHC.Stack (HasCallStack)
import Debug.Trace

{-----------------------------------------------------------------------------}
-- Evaluation

class Eval f name where
    eval :: HasCallStack => (name -> Bool) -> f name -> Bool

instance Ord name => Eval (Term BOps Bool) name where
    eval lookup term = let     -- constantFold already does it, and we can prove it :)
        saturated :: Term BOps Bool Void
        saturated = substVars (Val . lookup) term
        in case constantFold' saturated of
            Left b  -> b
            Right x -> absurd' x

instance Ord name => Eval (TermLit BNOps Void) name where
    eval :: (name -> Bool) -> TermLit BNOps Void name -> Bool
    eval lookup term = let
        lookupLit :: Literal name -> Term BNOps Bool Void
        lookupLit (b, name) = Val $ lookup name `xor` not b
        saturated :: Term BNOps Bool Void
        saturated = substVars lookupLit $ inject $ unTermLit term
        term' :: Term BOps Bool Void
        term' = inject saturated
        in case constantFold' term' of
            Left b  -> b
            Right x -> absurd' x

instance Ord name => Eval CNF name where
    eval :: (name -> Bool) -> CNF name -> Bool
    eval lookup (CNF conj) = let
        lookupLit :: Literal name -> Bool
        lookupLit (b, name) = lookup name `xor` not b
        saturated :: Conjunction (Disjunction Bool)
        saturated = fmap (fmap lookupLit) conj
        in getAll $ foldMap (All . getAny . foldMap Any) saturated

{-----------------------------------------------------------------------------}
-- Useful testing utilities

-- | True if @t1@ and @t2@ evaluate to the same value
-- for a given assignment function @lookup@.
equalEval :: (HasCallStack, Show (f name), Show (g name)
    , Eval f name, Eval g name)
    => (name -> Bool) -> f name -> g name -> Bool
equalEval lookup t1 t2 = let
    lhs = eval lookup t1
    rhs = eval lookup t2
    in
    -- trace ("equalEval " ++ show t1 ++ " " ++ show t2 ++ ":\n"
    -- ++ "    " ++ show lhs ++ " == " ++ show rhs) $
    lhs == rhs

-- | Equality of two terms of possibly different types
-- under arbitrary variable mappings
propEqual :: forall name f g. (HasCallStack
    , Show name, Ord name
    , Show (f name), Show (g name)
    , HasNames (f name), HasNames (g name)
    , NameT (f name) ~ name, NameT (g name) ~ name
    , Eval f name, Eval g name
    )
    => f name -> g name -> Property
propEqual t1 t2 = let
    vars :: Set name
    vars = Set.union (variableNames t1) (variableNames t2)
    in forAll (generateMapping (Set.toList vars)) $ \env ->
        -- trace ("XXX " ++ show env) $
        let
        lhs = eval (env Map.!) t1
        rhs = eval (env Map.!) t2
        in
        -- trace ("propEqual " ++ show t1 ++ " " ++ show t2 ++ ":\n"
        -- ++ "    " ++ show lhs ++ " == " ++ show rhs) $
        lhs === rhs

{- | Comparing of two terms of possibly different types where
finding a solution for @t1@ /implies/ a solution for @t2@
but not the other way around. Useful for testing the 'tseitinTransform'',
because it adds additional variables.

_Warning_: This property tries to generate mappings that satisfy @t1@,
but it cannot quickly find them. So this will be slow for hardly satisfiable
terms @t1@.
-}
propImplies :: forall name f g. (HasCallStack
    , Show name, Ord name
    , Show (f name), Show (g name)
    , HasNames (f name), HasNames (g name)
    , NameT (f name) ~ name, NameT (g name) ~ name
    , Eval f name, Eval g name
    )
    => f name -> g name -> Property
propImplies t1 t2 = let
    vars :: Set name
    vars = Set.union (variableNames t1) (variableNames t2)
    test :: Map name Bool -> Bool
    test env = eval (env Map.!) t1
    in forAll (tryGenerateMapping 5 test (Set.toList vars)) $ \env ->
        -- trace ("XXX " ++ show env) $
        let
        lhs = eval (env Map.!) t1
        rhs = eval (env Map.!) t2
        in
        -- trace ("propImplies " ++ show t1 ++ " " ++ show t2 ++ ":\n"
        -- ++ "    " ++ show lhs ++ " == " ++ show rhs) $
        -- lhs ==> rhs      -- fails if lhs is UNSAT!
        -- trace ("propImplies " ++ show lhs ++ " " ++ show rhs) $
        not lhs || rhs
