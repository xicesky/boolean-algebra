
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Description     : Name-handling monads
Stability       : experimental

Provides monad transformer interfaces and utilities:

'MonadName' for bidirectional mapping between names and 'Int'. This is
implemented by @NamingT@ from this same package.

'MonadGenName' for additional automatic name generation. This is implemented by
@GenNameT@ from this same package.
-}
module Control.Monad.Naming.Class
    (   -- * Unique integers
        MonadUniqueInt(..)
    ,   newIndex
    ,   peekIndex

    ,   -- * Name mapping
        MonadName(..)
    ,   newExactName
    ,   unsafeNewExactName
    ,   getNameMap
    ,   nameOf
    ,   unsafeNameOf
    ,   indexOf
    ,   isNameTaken
    ,   autoMapName

    ,   -- * Name generating
        MonadGenName(..)
    ,   NamingFun
    ,   defaultNamingFun
    ,   generateName
    ,   newNamedWithPrefix
    ,   newNamed
    ,   setNamePrefix

    ) where

import Data.Maybe (fromMaybe)

-- mtl / transformers
import Control.Monad.State.Strict

import Missing.Bimap
import qualified Missing.Bimap as Bimap
import Missing.Optics

import GHC.Stack (HasCallStack)

{-
TODO / Ideas:
    - Need an Arbitrary instance for testing!

    - Create a module Control.Monad.Naming that hides internals

    - The state... functions don't give us any laws to hold on to.
        Find more specific types that can provide all the neccessary
        functionality and don't just expose plain states.
        For example: The index cannot be reset, names cannot be deleted,
        the scheme cannot be read.

    - A safe version of newExactName
    - Provide a way to run without "error" (e.g. in ExceptT)

-}

{-----------------------------------------------------------------------------}

{- | Type of a name generator function.

A 'NamingFun' produces names @n@ when given a predicate to check for existing
names @n -> Bool@ and a prefix.
-}
type NamingFun n = (n -> Bool) -> n -> n

{- | Interface for monads producing unique 'Int'.

Provides 'newIndex' and 'peekIndex' below.
-}
class Monad m => MonadUniqueInt m where

    {- | /Internal/ state accessor.
    -}
    stateIndex :: EmbedStateFun Int m

{- | Interface for monads that map names.

See e.g. 'newExactName', 'nameOf' and 'indexOf' below.
-}
class (MonadUniqueInt m, Ord n) => MonadName n m | m -> n where

    {- | /Internal/ state accessor.
    -}
    stateNames :: EmbedStateFun (Bimap Int n) m

{- | Interface for monads that generate names.

This should be implemented for all monads that provide fresh names,
most likely stacks of monad tranformers that use 'NamingT'.

See e.g. 'newNamed' and 'setNamePrefix' below.
-}
class (MonadName n m, Monoid n, Ord n) => MonadGenName n m | m -> n where

    {- | /Internal/ state accessor.
    -}
    statePrefix :: EmbedStateFun n m

    {- | /Internal/ state accessor.
    -}
    stateScheme :: EmbedStateFun (NamingFun n) m

{-----------------------------------------------------------------------------}

{- | Default name generator for 'String' names.

Tries suffixes of the letters @['a'..'z']@, increasing in length.
-}
defaultNamingFun :: NamingFun String
defaultNamingFun filter prefix = head $ dropWhile (not . filter) varnames where
    suffixes :: [String]
    suffixes = [replicate k ['a'..'z'] | k <- [1..]] >>= sequence
    --suffixes = "" :([replicate k ['a'..'z'] | k <- [1..]] >>= sequence)
    varnames :: [String]
    varnames = (prefix++) <$> suffixes

{-----------------------------------------------------------------------------}
-- Orphans for lifting through other monad transformers

instance MonadUniqueInt m => MonadUniqueInt (StateT s m) where
    stateIndex = lift . stateIndex

instance MonadName n m => MonadName n (StateT s m) where
    stateNames = lift . stateNames

instance MonadGenName n m => MonadGenName n (StateT s m) where
    statePrefix = lift . statePrefix
    stateScheme = lift . stateScheme

{-----------------------------------------------------------------------------}
-- MonadUniqueInt

-- | Generate a new unique integer
newIndex :: MonadUniqueInt m => m Int
newIndex = stateIndex $ \i -> (i, i+1)

-- | Peek at the next unique integer that would be generated
peekIndex :: MonadUniqueInt m => m Int
peekIndex = sGet stateIndex

{-----------------------------------------------------------------------------}
-- MonadName

{- | Create a new entry, given an exact name to use.

Returns 'Nothing' if the name already exists.
-}
newExactName :: forall n m. MonadName n m => n -> m (Maybe Int)
newExactName name = indexOf name >>= \case
    Just _  -> return Nothing
    Nothing -> do
        i <- newIndex
        ns <- sGet stateNames
        case Bimap.insert i name ns of
            Right m                 -> sSet stateNames m >> return (Just i)
            -- This should never happen, we just created a new index - TODO test
            Left KeyExistsError     -> error "Index already exists"
            -- This should never happen, we just checked indexOf - TODO test
            Left ValueExistsError   -> error "Name already exists"

{- | Create a new entry, given an exact name to use.

Throws an 'error' if the name already exists.
-}
unsafeNewExactName :: forall n m. (HasCallStack, MonadName n m) => n -> m Int
unsafeNewExactName name = fromMaybe (error "unsafeNewExactName") <$> newExactName name

{- | Get the 'Bimap' of registered names.
-}
getNameMap :: MonadName n m => m (Bimap Int n)
getNameMap = sGet stateNames

{- | Get the name associated with an entry.
-}
nameOf :: MonadName n m => Int -> m (Maybe n)
nameOf i = Bimap.lookup i <$> getNameMap

{- | /Unsafe/ version of 'nameOf'

Assumes that the name is defined and throws an 'error' if this is not the case.
-}
unsafeNameOf :: MonadName n m => Int -> m n
unsafeNameOf i = fromMaybe (error $ "unsafeNameOf " ++ show i) <$> nameOf i

{- | Get the entry associated with a name.
-}
indexOf :: MonadName n m => n -> m (Maybe Int)
indexOf n = Bimap.lookupR n <$> getNameMap

{- | Check if a name is already defined (taken).
-}
isNameTaken :: MonadName n m => n -> m Bool
isNameTaken n = indexOf n >>= \case
    Nothing -> return False
    Just _  -> return True

{- | Automatically map names

Maps names to indices by looking them up using 'indexOf' or
creates a new name using newExactName. Use with a traversal to get rid
of all names, for example:

>>> unsafeRunNamingT $ traverse autoMapName ["a", "b", "c", "b", "a", "c"]

[0,1,2,1,0,2]
-}
autoMapName :: MonadName n m => n -> m Int
autoMapName n = indexOf n >>= \case
    Just i      -> return i
    -- TODO test: should not fail here, we just checked indexOf
    Nothing     -> unsafeNewExactName n

{-----------------------------------------------------------------------------}
-- MonadGenName

{- | Generate a new name (that's not already taken).

This just /generates/ the name and does /not/ define it.
-}
generateName :: MonadGenName n m => n -> m n
generateName prefix = do
    names <- sGet stateNames
    prefix0 <- sGet statePrefix
    nFun <- sGet stateScheme
    let name = nFun (`Bimap.notMemberR` names) (prefix0 <> prefix)
    return name

{- | Define a new (autogenerated) name.
-}
newNamedWithPrefix :: MonadGenName n m => n -> m Int
newNamedWithPrefix prefix = do
    n <- generateName prefix
    -- Should not fail if the generator works correctly - TODO test
    unsafeNewExactName n

{- | Define a new (autogenerated) name.

Shortcut version of 'newNamedWithPrefix'.
-}
newNamed :: MonadGenName n m => m Int
newNamed = newNamedWithPrefix mempty

{- | Set the current name prefix.

This prefix will be used for all generated names (in addition to any prefix
given to 'generateName' / 'newNamedWithPrefix', of course).
-}
setNamePrefix :: MonadGenName n m => n -> m ()
setNamePrefix = sSet statePrefix
