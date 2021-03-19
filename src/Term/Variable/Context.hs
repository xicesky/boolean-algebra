
{- |
Description     : Terms with names in a context
Stability       : experimental

Provides /contexts/ over terms, that map variable
numbers to names.
-}
module Term.Variable.Context
    (   -- * Definition
        Context(..)

    ,   -- * Utilities
        getNameMap
    ,   getIndexMap
    ,   buildContext
    ,   destroyContext
    ,   ctxRenameVars
    ) where

import Data.Kind (Type)

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Term.Variable.Names

{-----------------------------------------------------------------------------}

{- | A context over a structure @f@ maps the variable numbers
in @f Int@ to names @name@.
-}
data Context f name = Context
    {   getMappedNames :: MappedNames name
    ,   getTerm :: f Int
    }

-- | Retrieve the mapping from @Int@ to @name@
getNameMap :: Context f name -> Map Int name
getNameMap = fst . getMappedNames

-- | Retrieve the mapping from @name@ to @Int@
getIndexMap :: Context f name ->  Map name Int
getIndexMap = snd . getMappedNames

deriving instance (Show (f Int), Show name) => Show (Context f name)
deriving instance (Eq (f Int), Eq name) => Eq (Context f name)

-- FIXME: Wrong for: Term op val (Bool, String)
-- It would build a name map from LITERALS to Ints
-- Demo:
-- >>> buildContext $ simplify demo1b
buildContext :: forall f name. (Traversable f, Ord name)
    => f name -> Context f name
buildContext t = let
    mappedNames :: MappedNames name
    mappedNames = mkMappedNames (foldMap Set.singleton t)
    indexNames :: name -> Int
    indexNames = (Map.!) (snd mappedNames)
    in Context mappedNames (fmap indexNames t)

destroyContext :: forall f name. (Traversable f, Ord name)
    => Context f name -> f name
destroyContext (Context mappedNames t) = let
    remapNames :: Int -> name
    remapNames = (Map.!) (fst mappedNames)
    in fmap remapNames t

{- | Map variable names in a context

Uses a mapping function to change variable names in a context. This function
even works for non-injective mappings.
If the mapping function is injective, this is the same as /renaming/ variables.
If, on the other hand, the function maps two names to the same new name,
the variables will become indistinguishable.
-}
-- TODO: Could be much more efficient than rebuilding the whole context
ctxRenameVars :: (Traversable f, Ord a, Ord b) => (a -> b) -> Context f a -> Context f b
ctxRenameVars rename = buildContext . fmap rename . destroyContext

