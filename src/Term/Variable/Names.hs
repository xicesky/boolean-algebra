
{- |
Description     : Handling (free) variables
Stability       : experimental

Provides mechanisms to handle free variables.
-}
module Term.Variable.Names
    (   -- * Open class for variable names
        HasNames(..)

    ,   -- * Bidirectional name map
        mappedNames
    ) where

import Data.Kind (Type)

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Missing.Bimap
import Term.Term

{-----------------------------------------------------------------------------}

-- | Types that contain variable names
class HasNames t where
    type NameT t :: Type
    variableNames :: t -> Set (NameT t)

instance Ord name => HasNames (Term op val name) where
    type NameT (Term op val name) = name
    variableNames :: Term op val name -> Set name
    variableNames = foldMap Set.singleton

{-----------------------------------------------------------------------------}

{- | Build a map of all free variables and assigned numbers.

Each variable is assigned a unique positive @Int@. The variables numbers
start at 1, so you can use the /sign/ for additional information.
-}
mappedNames :: (HasNames t, Ord (NameT t)) => t -> Bimap Int (NameT t)
mappedNames = setToIndexMap [1..] . variableNames
