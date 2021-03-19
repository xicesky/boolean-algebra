
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

#warning#
/Warning/: This assumes that @f@ is a 'Functor' over variable names.
If your varaibles contain additional information (for example a /sign/
or source location annotations), then these would be considered
/part of the name/!

For example: Consider a term over variables with an additional
Boolean:

>>> term :: Term op val (Bool, String)

Creating a 'Context' (using 'buildContext') directly over this term
would build a mapping @(Bool, String) \<-\> Int@, which means the variables
(True, "a") and (False, "a") are considered to be different names.

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

{- | Build a context over any 'Traversable' functor containing
variable names.

Please see the [warning about names](#warning).
-}
buildContext :: forall f name. (Traversable f, Ord name)
    => f name -> Context f name
buildContext t = let
    mappedNames :: MappedNames name
    mappedNames = mkMappedNames (foldMap Set.singleton t)
    indexNames :: name -> Int
    indexNames = (Map.!) (snd mappedNames)
    in Context mappedNames (fmap indexNames t)

{- | Destroy a @Context f name@ to yield the original
'Traversable' @f name@.

This is the inverse of 'buildContext'.
-}
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
