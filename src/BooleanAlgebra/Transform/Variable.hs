
{- |
Description     : Variable name handling
Stability       : experimental

_WIP_: This module is work in progress.
-}
module BooleanAlgebra.Transform.Variable
    (   -- * Collecting variable names
        HasNames(..)

    ,   -- * Name maps
        MappedNames(..)
    ,   Context(..)
    ,   getNameMap
    ,   getIndexMap
    ,   buildContext
    ,   destroyContext

    ,   -- * Creating fresh names
        findFreshName
    ,   FreshState
    ,   MonadFresh(..)
    ,   FreshT
    ,   runFreshT
    ) where

import Data.Kind (Type)

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Term.Variable.Names
import Term.Variable.Context
import Term.Variable.FreshT
import BooleanAlgebra.Base.Expression

{-----------------------------------------------------------------------------}
-- Simple tools

instance Ord name => HasNames (TermLit op val name) where
    type NameT (TermLit op val name) = name
    variableNames :: TermLit op val name -> Set name
    variableNames = foldMap Set.singleton

instance Ord name => HasNames (CNF name) where
    type NameT (CNF name) = name
    variableNames :: CNF name -> Set name
    variableNames = foldMap Set.singleton

{-----------------------------------------------------------------------------}
-- Idea: compare terms for α-Equivalence
