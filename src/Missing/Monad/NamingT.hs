
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Missing.Monad.NamingT where

import Data.Maybe (fromMaybe)
import Data.Functor.Const

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
    - Provide a way to run without the Monoid constraint!

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

-- | Name generator function
newtype NamingFun n = NamingFun { _namingFun :: (n -> Bool) -> n -> n }

-- | _Tag_ type for NamingT without a name generator
type NoNameGen = Const ()

-- | _Tag_ type (alias) for NamingT with a name generator
type WithNameGen = NamingFun

{- | Internal state of 'NamingT'.
-}
data NamingTState gen n = NamingTState
    {   nsNextIndex :: Int              -- ^ the next free integer index
    ,   nsNames :: Bimap Int n          -- ^ mapping @name \<-\> Int@
    ,   nsPrefix :: n                   -- ^ the default prefix for fresh names
    ,   nsScheme :: gen n               -- ^ optional name generator
    }

-- | Monad transformer with the ability to create fresh names.
newtype NamingT gen n m a = NamingT { toStateT :: StateT (NamingTState gen n) m a }

deriving instance Functor m => Functor (NamingT gen n m)
deriving instance Monad m => Applicative (NamingT gen n m)
deriving instance Monad m => Monad (NamingT gen n m)
deriving instance MonadTrans (NamingT gen n)

deriving instance MonadIO m => MonadIO (NamingT gen n m)
-- ... and so on... implement as we need it

internalShow :: Show n => (gen n -> ShowS) -> NamingTState gen n -> ShowS
internalShow showGen (NamingTState nextIndex prefix names scheme)
    = showString "NamingTState {"
    . showString "nsNextIndex = "
    . shows nextIndex
    . showString ", nsPrefix = "
    . shows prefix
    . showString ", nsNames = "
    . shows names
    . showGen scheme
    . showString "}"

instance Show n => Show (NamingTState NamingFun n) where
    showsPrec d ns
        = showParen (d >= 11) $ internalShow (const $ showString ", nsScheme = <?> ") ns

instance Show n => Show (NamingTState NoNameGen n) where
    showsPrec d ns
        = showParen (d >= 11) $ internalShow (const id) ns

-- makeFieldLabelsWith noPrefixFieldLabels ''NamingTState

{-----------------------------------------------------------------------------}
-- Manual splices

-- TODO Remove this section and use makeFieldLabelsWith (above) when
-- haskell-language-server can handle template haskell without crashing.
-- https://github.com/haskell/haskell-language-server/issues/1297
-- https://github.com/haskell/haskell-language-server/issues/1342

{-# ANN module "HLint: ignore Redundant bracket" #-}
{-# ANN module "HLint: ignore Avoid lambda" #-}

-- makeFieldLabelsWith noPrefixFieldLabels ''NamingTState
-- ======>
instance (k_a71x ~ A_Lens,
          a_a71y ~ Bimap Int n_a5cv,
          b_a71z ~ Bimap Int n_a5cv) =>
         LabelOptic "nsNames" k_a71x (NamingTState gen_a5cu n_a5cv) (NamingTState gen_a5cu n_a5cv) a_a71y b_a71z where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_a71A s_a71B
           -> case s_a71B of {
                NamingTState x1_a71C x2_a71D x3_a71E x4_a71F
                  -> (fmap
                        (\ y_a71G -> (((NamingTState x1_a71C) y_a71G) x3_a71E) x4_a71F))
                       (f_a71A x2_a71D) })
instance (k_a71H ~ A_Lens, a_a71I ~ Int, b_a71J ~ Int) =>
         LabelOptic "nsNextIndex" k_a71H (NamingTState gen_a5cu n_a5cv) (NamingTState gen_a5cu n_a5cv) a_a71I b_a71J where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_a71K s_a71L
           -> case s_a71L of {
                NamingTState x1_a71M x2_a71N x3_a71O x4_a71P
                  -> (fmap
                        (\ y_a71Q -> (((NamingTState y_a71Q) x2_a71N) x3_a71O) x4_a71P))
                       (f_a71K x1_a71M) })
instance (k_a71R ~ A_Lens, a_a71S ~ n_a5cv, b_a71T ~ n_a5cv) =>
         LabelOptic "nsPrefix" k_a71R (NamingTState gen_a5cu n_a5cv) (NamingTState gen_a5cu n_a5cv) a_a71S b_a71T where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_a71U s_a71V
           -> case s_a71V of {
                NamingTState x1_a71W x2_a71X x3_a71Y x4_a71Z
                  -> (fmap
                        (\ y_a720 -> (((NamingTState x1_a71W) x2_a71X) y_a720) x4_a71Z))
                       (f_a71U x3_a71Y) })
instance (k_a721 ~ A_Lens,
          a_a722 ~ gen_a5cu n_a5cv,
          b_a723 ~ gen_a71w n_a5cv) =>
         LabelOptic "nsScheme" k_a721 (NamingTState gen_a5cu n_a5cv) (NamingTState gen_a71w n_a5cv) a_a722 b_a723 where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_a724 s_a725
           -> case s_a725 of {
                NamingTState x1_a726 x2_a727 x3_a728 x4_a729
                  -> (fmap
                        (\ y_a72a -> (((NamingTState x1_a726) x2_a727) x3_a728) y_a72a))
                       (f_a724 x4_a729) })

{-----------------------------------------------------------------------------}

defaultNamingFun :: NamingFun String
defaultNamingFun = NamingFun f where
    f :: (String -> Bool) -> String -> String
    f filter prefix = head $ dropWhile (not . filter) (varnames prefix)
    suffixes :: [String]
    suffixes = [replicate k ['a'..'z'] | k <- [1..]] >>= sequence
    --suffixes = "" :([replicate k ['a'..'z'] | k <- [1..]] >>= sequence)
    varnames :: String -> [String]
    varnames prefix = (prefix++) <$> suffixes

{-----------------------------------------------------------------------------}

instance Monad m => MonadUniqueInt (NamingT gen n m) where
    stateIndex = NamingT . stateFun #nsNextIndex

instance (Monad m, Monoid n, Ord n) => MonadName n (NamingT gen n m) where
    stateNames = NamingT . stateFun #nsNames

instance (Monad m, Monoid n, Ord n) => MonadGenName n (NamingT NamingFun n m) where
    statePrefix = NamingT . stateFun #nsPrefix
    stateScheme = NamingT . stateFun #nsScheme

-- | Run the 'NamingT' monad transformer.
runGenNamingT :: forall gen m n a. (Monad m, Monoid n, Ord n) =>
    Int -> gen n -> NamingT gen n m a -> m a
runGenNamingT initialIndex nfun namet = do
    (a, _) <- runStateT (toStateT namet) initNamingTState
    return a
    where
        initNamingTState :: NamingTState gen n
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
runNamingT :: (Monad m, Monoid n, Ord n) =>
    Int -> NamingT NoNameGen n m a -> m a
runNamingT initialIndex = runGenNamingT initialIndex (Const ())

{- | Run the 'NamingT' monad transformer using defaults.
-}
runNamingTString :: Monad m => Int -> NamingT NamingFun String m a -> m a
runNamingTString initialIndex = runGenNamingT initialIndex defaultNamingFun

{-----------------------------------------------------------------------------}
-- Internal utilities

internalGenerateName :: MonadGenName n m => n -> m n
internalGenerateName prefix = do
    names <- sGet stateNames
    prefix0 <- sGet statePrefix
    (NamingFun nFun) <- sGet stateScheme
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
    Int -> (f Int -> NamingT NamingFun String m (g Int)) -> f String -> m (g String)
liftNamesM initialIndex f fs = runNamingTString initialIndex wrap where
    wrap :: NamingT NamingFun String m (g String)
    wrap = do
        fi <- traverse autoMapName fs
        gi <- f fi
        traverse unsafeNameOf gi

{- | Lift a function operating on integers to one operating on names.

Non-monadic variant of 'liftNamesM'.
-}
liftNames :: forall f g. (Traversable f, Traversable g) =>
    Int -> (f Int -> NamingT NamingFun String Identity (g Int)) -> f String -> g String
liftNames initialIndex f fs = runIdentity $ liftNamesM initialIndex f fs

{- | Separate names from a term into a map
-}
slurpNames :: (Monoid n, Ord n, Traversable f) =>
    Int -> f n -> (Map Int n, f Int)
slurpNames initialIndex fn = runIdentity $ runNamingT initialIndex $ do
    fi <- traverse autoMapName fn
    nm <- Bimap.toMap <$> getNameMap
    return (nm, fi)
