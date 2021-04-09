
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Naming.Class where

import Data.Maybe (fromMaybe)

import Missing.Bimap
import qualified Missing.Bimap as Bimap
import Missing.Optics

{-
FIXME:
    - Documentation

TODO / Ideas:
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
-- Internal utilities

internalGenerateName :: MonadGenName n m => n -> m n
internalGenerateName prefix = do
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

-- | FIXME This can /fail/
newExactName :: forall n m. MonadName n m => n -> m Int
newExactName name = do
    i <- newIndex
    stateNames $ \names -> (i, ins i name names)
    where
    ins :: Int -> n -> Bimap Int n -> Bimap Int n
    ins va vb map = case Bimap.insert va vb map of
        Left KeyExistsError     -> error $ "Index " ++ show va ++ " already exists"
        Left ValueExistsError   -> error "Name already exists"
        Right m                 -> m

newNamedWithPrefix :: MonadGenName n m => n -> m Int
newNamedWithPrefix prefix = do
    n <- internalGenerateName prefix
    newExactName n

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
    Nothing     -> newExactName n   -- TODO test: should not fail here
