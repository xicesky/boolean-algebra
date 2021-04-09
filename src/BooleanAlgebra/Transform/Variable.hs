
{- |
Description     : Variable name handling
Stability       : experimental

_WIP_: This module is work in progress.
-}
module BooleanAlgebra.Transform.Variable
    (   -- * Collecting variable names
        HasNames(..)
    ,   maximumVarNum

    ,   -- * Abstracting variable names
        MonadName
    ,   NamingT
    ,   runNamingT
    ,   slurpNames

    ,   -- * Creating fresh names
        MonadGenName
    ,   GenNameT
    ,   runGenNameT
    ,   runGenNameTString
    ,   newNamedWithPrefix
    ,   liftNames
    ,   liftNamesM

    ) where

import Data.Kind (Type)
import Data.Traversable
import Data.Functor.Identity

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Missing.Bimap as Bimap
import Control.Monad.Naming.Class
import Control.Monad.Naming.NamingT
import Control.Monad.Naming.GenNameT
import Term.Variable.Names
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

{- | Get the maximum variable number, or @0@ .

Returns @0@ if the term has no variables. Useful for a @CNF Int@
representation, where the number of variables has to be specified.
-}
maximumVarNum :: (HasNames t, Num (NameT t), Ord (NameT t)) =>
    t -> NameT t
maximumVarNum term = let
    names = variableNames term
    in if null names then 0 else maximum names

{-----------------------------------------------------------------------------}
-- Idea: compare terms for α-Equivalence
