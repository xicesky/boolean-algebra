
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Missing.Monad.NamingT where

import Data.Maybe (fromMaybe)

-- containers
import Data.Map.Strict (Map)

-- mtl / transformers
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

-- optics
import Optics
import Optics.TH (makeFieldLabelsWith, noPrefixFieldLabels)
--import Optics.TH (makeFieldLabelsNoPrefix)  -- req optics-0.4

import Missing.Bimap
import qualified Missing.Bimap as Bimap
import Missing.Optics

{-
FIXME:
    - Documentation
    - Shouldn't be in "Missing", rather in "Utils"
    - Provide a way to run without "error" (e.g. in ExceptT)
    - Use a phantom tag to make unsafeRunNamingT safe.
    - Provide a way to run without the Monoid constraint!
        - Requires a phantom tag, same as the unsafeRunNamingT

TODO / Ideas:
    - add a randomization source

    - The state... functions don't give us any laws to hold on to.
        Find more specific types that can provide all the neccessary
        functionality and don't just expose plain states.
        For example: The index cannot be reset, names cannot be deleted,
        the scheme cannot be read.
-}

{-----------------------------------------------------------------------------}

class Monad m => MonadUniqueInt m where

    -- Internals
    stateIndex :: EmbedStateFun Int m

class (MonadUniqueInt m, Monoid n, Ord n) => MonadName n m | m -> n where

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

{- | Internal state of 'NamingT'.
-}
data NamingTState n = NamingTState
    {   nsNextIndex :: Int              -- ^ the next free integer index
    ,   nsPrefix :: n                   -- ^ the default prefix for fresh names
    ,   nsNames :: Bimap Int n          -- ^ mapping @name \<-\> Int@
    ,   nsScheme :: NamingFun n
    }

type NamingFun n = (n -> Bool) -> n -> n

-- | Monad transformer with the ability to create fresh names.
newtype NamingT n m a = NamingT { toStateT :: StateT (NamingTState n) m a }

deriving instance Functor m => Functor (NamingT n m)
deriving instance Monad m => Applicative (NamingT n m)
deriving instance Monad m => Monad (NamingT n m)
deriving instance MonadTrans (NamingT n)

deriving instance MonadIO m => MonadIO (NamingT n m)
-- ... and so on... implement as we need it

instance Show n => Show (NamingTState n) where
    showsPrec d (NamingTState nextIndex prefix names scheme)
        = showParen (d >= 11) $ showString "NamingTState {"
        . showString "nsNextIndex = "
        . shows nextIndex
        . showString ", nsPrefix = "
        . shows prefix
        . showString ", nsNames = "
        . shows names
        . showString ", nsScheme = <?> }"

-- makeFieldLabelsWith noPrefixFieldLabels ''NamingTState

{-----------------------------------------------------------------------------}
-- Manual splices

-- TODO Remove this section and use makeFieldLabelsWith (above) when
-- haskell-language-server can handle template haskell without crashing.
-- https://github.com/haskell/haskell-language-server/issues/1297
-- https://github.com/haskell/haskell-language-server/issues/1342

{-# ANN module "HLint: ignore Redundant bracket" #-}
{-# ANN module "HLint: ignore Avoid lambda" #-}

instance (k_a86Y ~ A_Lens,
          a_a86Z ~ Bimap Int n_a7SZ,
          b_a870 ~ Bimap Int n_a7SZ) =>
         LabelOptic "nsNames" k_a86Y (NamingTState n_a7SZ) (NamingTState n_a7SZ) a_a86Z b_a870 where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_a871 s_a872
           -> case s_a872 of {
                NamingTState x1_a873 x2_a874 x3_a875 x4_a876
                  -> (fmap
                        (\ y_a877 -> (((NamingTState x1_a873) x2_a874) y_a877) x4_a876))
                       (f_a871 x3_a875) })
instance (k_a878 ~ A_Lens, a_a879 ~ Int, b_a87a ~ Int) =>
         LabelOptic "nsNextIndex" k_a878 (NamingTState n_a7SZ) (NamingTState n_a7SZ) a_a879 b_a87a where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_a87b s_a87c
           -> case s_a87c of {
                NamingTState x1_a87d x2_a87e x3_a87f x4_a87g
                  -> (fmap
                        (\ y_a87h -> (((NamingTState y_a87h) x2_a87e) x3_a87f) x4_a87g))
                       (f_a87b x1_a87d) })
instance (k_a87i ~ A_Lens, a_a87j ~ n_a7SZ, b_a87k ~ n_a7SZ) =>
         LabelOptic "nsPrefix" k_a87i (NamingTState n_a7SZ) (NamingTState n_a7SZ) a_a87j b_a87k where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_a87l s_a87m
           -> case s_a87m of {
                NamingTState x1_a87n x2_a87o x3_a87p x4_a87q
                  -> (fmap
                        (\ y_a87r -> (((NamingTState x1_a87n) y_a87r) x3_a87p) x4_a87q))
                       (f_a87l x2_a87o) })
instance (k_a87s ~ A_Lens,
          a_a87t ~ NamingFun n_a7SZ,
          b_a87u ~ NamingFun n_a7SZ) =>
         LabelOptic "nsScheme" k_a87s (NamingTState n_a7SZ) (NamingTState n_a7SZ) a_a87t b_a87u where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_a87v s_a87w
           -> case s_a87w of {
                NamingTState x1_a87x x2_a87y x3_a87z x4_a87A
                  -> (fmap
                        (\ y_a87B -> (((NamingTState x1_a87x) x2_a87y) x3_a87z) y_a87B))
                       (f_a87v x4_a87A) })

{-----------------------------------------------------------------------------}

defaultNamingFun :: NamingFun String
defaultNamingFun filter prefix = head $ dropWhile (not . filter) varnames where
    suffixes :: [String]
    suffixes = [replicate k ['a'..'z'] | k <- [1..]] >>= sequence
    --suffixes = "" :([replicate k ['a'..'z'] | k <- [1..]] >>= sequence)
    varnames :: [String]
    varnames = (prefix++) <$> suffixes

{-----------------------------------------------------------------------------}

instance Monad m => MonadUniqueInt (NamingT n m) where
    stateIndex = NamingT . stateFun #nsNextIndex

instance (Monad m, Monoid n, Ord n) => MonadName n (NamingT n m) where
    stateNames = NamingT . stateFun #nsNames

instance (Monad m, Monoid n, Ord n) => MonadGenName n (NamingT n m) where
    statePrefix = NamingT . stateFun #nsPrefix
    stateScheme = NamingT . stateFun #nsScheme

-- | Run the 'NamingT' monad transformer.
runNamingT :: forall m n a. (Monad m, Monoid n, Ord n) =>
    Int -> NamingFun n -> NamingT n m a -> m a
runNamingT initialIndex nfun namet = do
    (a, _) <- runStateT (toStateT namet) initNamingTState
    return a
    where
        initNamingTState :: NamingTState n
        initNamingTState = NamingTState
            {   nsNextIndex = initialIndex
            ,   nsPrefix = mempty
            ,   nsNames = Bimap.empty
            ,   nsScheme = nfun
            }

{- | Run the 'NamingT' monad transformer without the ability
to create new names.

Trying to create a name will result in an /'error'/.
-}
unsafeRunNamingT :: (Monad m, Monoid n, Ord n) =>
    Int -> NamingT n m a -> m a
unsafeRunNamingT initialIndex = runNamingT initialIndex
    (\_ _ -> error "unsafeRunNamingT: can't create new names.")

{- | Run the 'NamingT' monad transformer using defaults.
-}
runNamingTString :: Monad m => Int -> NamingT String m a -> m a
runNamingTString initialIndex = runNamingT initialIndex defaultNamingFun

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

{-----------------------------------------------------------------------------}

{- | Lift a function operating on integers to one operating on names.
-}
liftNamesM :: forall m f g. (Monad m, Traversable f, Traversable g) =>
    Int -> (f Int -> NamingT String m (g Int)) -> f String -> m (g String)
liftNamesM initialIndex f fs = runNamingTString initialIndex wrap where
    wrap :: NamingT String m (g String)
    wrap = do
        fi <- traverse autoMapName fs
        gi <- f fi
        traverse unsafeNameOf gi

{- | Lift a function operating on integers to one operating on names.

Non-monadic variant of 'liftNamesM'.
-}
liftNames :: forall f g. (Traversable f, Traversable g) =>
    Int -> (f Int -> NamingT String Identity (g Int)) -> f String -> g String
liftNames initialIndex f fs = runIdentity $ liftNamesM initialIndex f fs

{- | Separate names from a term into a map
-}
slurpNames :: (Monoid n, Ord n, Traversable f) =>
    Int -> f n -> (Map Int n, f Int)
slurpNames initialIndex fn = runIdentity $ unsafeRunNamingT initialIndex $ do
    fi <- traverse autoMapName fn
    nm <- Bimap.toMap <$> getNameMap
    return (nm, fi)
