
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Naming.Class where

import Data.Maybe (fromMaybe)

-- mtl / transformers
import Control.Monad.State.Strict

import Missing.Bimap
import qualified Missing.Bimap as Bimap
import Missing.Optics

import GHC.Stack (HasCallStack)

{-
FIXME:
    - Documentation

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

type NamingFun n = (n -> Bool) -> n -> n

class Monad m => MonadUniqueInt m where

    -- Internals
    stateIndex :: EmbedStateFun Int m

class (MonadUniqueInt m, Ord n) => MonadName n m | m -> n where

    -- Internals
    stateNames :: EmbedStateFun (Bimap Int n) m

{- | Monad tranformer interface

This should be implemented for all monads that provide fresh names,
most likely stacks of monad tranformers that use 'NamingT'.
-}
class (MonadName n m, Monoid n, Ord n) => MonadGenName n m | m -> n where

    -- Internals
    statePrefix :: EmbedStateFun n m
    stateScheme :: EmbedStateFun (NamingFun n) m

{-----------------------------------------------------------------------------}

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
-- Internal utilities

generateName :: MonadGenName n m => n -> m n
generateName prefix = do
    names <- sGet stateNames
    prefix0 <- sGet statePrefix
    nFun <- sGet stateScheme
    let name = nFun (`Bimap.notMemberR` names) (prefix0 <> prefix)
    return name

{-----------------------------------------------------------------------------}
-- Using MonadName

-- Visible externally

newIndex :: MonadUniqueInt m => m Int
newIndex = stateIndex $ \i -> (i, i+1)

peekIndex :: MonadUniqueInt m => m Int
peekIndex = sGet stateIndex

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

unsafeNewExactName :: forall n m. (HasCallStack, MonadName n m) => n -> m Int
unsafeNewExactName name = fromMaybe (error "unsafeNewExactName") <$> newExactName name

newNamedWithPrefix :: MonadGenName n m => n -> m Int
newNamedWithPrefix prefix = do
    n <- generateName prefix
    -- Should not fail if the generator works correctly - TODO test
    unsafeNewExactName n

newNamed :: MonadGenName n m => m Int
newNamed = newNamedWithPrefix mempty

setNamePrefix :: MonadGenName n m => n -> m ()
setNamePrefix = sSet statePrefix

getNameMap :: MonadName n m => m (Bimap Int n)
getNameMap = sGet stateNames

nameOf :: MonadName n m => Int -> m (Maybe n)
nameOf i = Bimap.lookup i <$> getNameMap

indexOf :: MonadName n m => n -> m (Maybe Int)
indexOf n = Bimap.lookupR n <$> getNameMap

isNameTaken :: MonadName n m => n -> m Bool
isNameTaken n = indexOf n >>= \case
    Nothing -> return False
    Just _  -> return True

-- | /Unsafe/ version of nameOf
unsafeNameOf :: MonadName n m => Int -> m n
unsafeNameOf i = fromMaybe (error $ "unsafeNameOf " ++ show i) <$> nameOf i

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
