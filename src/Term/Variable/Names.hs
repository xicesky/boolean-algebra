
{- |
Description     : Handling (free) variables
Stability       : experimental

Provides mechanisms to handle free variables.
-}
module Term.Variable.Names
    (   -- * Open class for variable names
        HasNames(..)

    ,   -- * Bidirectional name map
        MappedNames
    ,   mkMappedNames
    ,   mappedNames
    ,   mappedHasName
    ,   mappedInsert
    ) where

import Data.Kind (Type)

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

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

{- | Bidirectional map between variables names and
    variable numbers (@Int@).
-}
type MappedNames name = (Map Int name, Map name Int)

{- | Build a map of all free variables and assigned numbers.

Each variable is assigned a unique positive @Int@. The variables numbers
start at 1, so you can use the /sign/ for additional information.
-}
mkMappedNames :: Ord name => Set name -> MappedNames name
mkMappedNames nSet = let
    nList = Set.toList nSet
    in (Map.fromList $ zip [1..] nList, Map.fromList $ zip nList [1..])

-- | Utility functions that works on types implementing 'HasNames'
mappedNames :: (HasNames t, Ord (NameT t)) => t -> MappedNames (NameT t)
mappedNames = mkMappedNames . variableNames

-- | Check if @name@ is mapped.
mappedHasName :: Ord name => MappedNames name -> name -> Bool
mappedHasName (_, m) name = Map.member name m

{- | Insert a new mapping @Int \<-\> name@ into the map.

Since mappings are unique, this function will fail with @error@, if
you try to (re-)map an existing name or existing variable.
-}
mappedInsert :: (Ord name, Show name) => Int -> name -> MappedNames name -> MappedNames name
mappedInsert i name (iton, ntoi)
    | Map.member i iton     = error $ "Index " ++ show i ++ " already exists"
    | Map.member name ntoi  = error $ "Name " ++ show name ++ " already exists"
    | otherwise             = (Map.insert i name iton, Map.insert name i ntoi)
