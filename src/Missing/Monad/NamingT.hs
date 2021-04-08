
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
    - Use a phantom tag to make unsafeRunNamingT safe.
    - Provide a way to run without "error" (e.g. in ExceptT)
        - Then get rid of the "Show n" constraint!
    - Provide a way to run without the Monoid constraint!
        - Requires a phantom tag, same as the unsafeRunNamingT

TODO:
    - add a randomization source
-}

{-----------------------------------------------------------------------------}

{- | Monad tranformer interface

This should be implemented for all monads that provide fresh names,
most likely stacks of monad tranformers that use 'NamingT'.
-}
class (Monad m, Monoid n, Ord n) => MonadName n m | m -> n where

    newIndex :: m Int
    peekIndex :: m Int
    newNamedWithPrefix :: n -> m Int

    -- | FIXME This can /fail/
    newExactName :: n -> m Int

    newNamed :: m Int
    newNamed = newNamedWithPrefix mempty

    setNamePrefix :: n -> m ()
    getNameMap :: m (Bimap Int n)

    nameOf :: Int -> m (Maybe n)
    nameOf i = Bimap.lookup i <$> getNameMap

    indexOf :: n -> m (Maybe Int)
    indexOf n = Bimap.lookupR n <$> getNameMap

    -- | /Unsafe/ version of nameOf
    unsafeNameOf :: Int -> m n
    unsafeNameOf i = fromMaybe (error $ "unsafeNameOf " ++ show i) <$> nameOf i

    {- | Automatically map names

    Maps names to indices by looking them up using 'indexOf' or
    creates a new name using newExactName. Use with a traversal to get rid
    of all names, for example:

    >>> unsafeRunNamingT $ traverse autoMapName ["a", "b", "c", "b", "a", "c"]

    [0,1,2,1,0,2]
    -}
    autoMapName :: n -> m Int
    autoMapName n = indexOf n >>= \case
        Just i      -> return i
        Nothing     -> newExactName n   -- TODO test: should not fail here

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

instance (Monad m, Show n, Monoid n, Ord n) => MonadName n (NamingT n m) where
    newIndex :: NamingT n m Int
    newIndex = NamingT internalNewIndex

    peekIndex :: NamingT n m Int
    peekIndex = NamingT $ use #nsNextIndex

    newNamedWithPrefix :: n -> NamingT n m Int
    newNamedWithPrefix prefix = NamingT $ internalNewName prefix

    newExactName :: n -> NamingT n m Int
    newExactName name = NamingT $ internalNewExactName name

    setNamePrefix = NamingT . assign #nsPrefix
    getNameMap = NamingT $ use #nsNames
    nameOf i = NamingT $ Bimap.lookup i <$> use #nsNames
    indexOf n = NamingT $ Bimap.lookupR n <$> use #nsNames

-- | Run the 'NamingT' monad transformer.
runNamingT :: forall m n a. (Monad m, Show n, Monoid n, Ord n) =>
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
unsafeRunNamingT :: (Monad m, Show n, Monoid n, Ord n) =>
    Int -> NamingT n m a -> m a
unsafeRunNamingT initialIndex = runNamingT initialIndex
    (\_ _ -> error "unsafeRunNamingT: can't create new names.")

{- | Run the 'NamingT' monad transformer using defaults.
-}
runNamingTString :: Monad m => Int -> NamingT String m a -> m a
runNamingTString initialIndex = runNamingT initialIndex defaultNamingFun

{-----------------------------------------------------------------------------}
-- Internals

internalNewIndex :: Monad m => StateT (NamingTState n) m Int
internalNewIndex = updating #nsNextIndex (+1)

internalGenerateName :: (Ord n, Semigroup n, Monad m) => n -> StateT (NamingTState n) m n
internalGenerateName prefix = do
    names <- use #nsNames
    prefix0 <- use #nsPrefix
    nFun <- use #nsScheme
    let name = nFun (`Bimap.notMemberR` names) (prefix0 <> prefix)
    return name

internalNewExactName :: (Show n, Ord n, Semigroup n, Monad m) => n -> StateT (NamingTState n) m Int
internalNewExactName name = do
    i <- internalNewIndex
    modifying' #nsNames $ Bimap.insertUnsafe i name
    return i

internalNewName :: (Show n, Ord n, Semigroup n, Monad m) => n -> StateT (NamingTState n) m Int
internalNewName prefix = do
    n <- internalGenerateName prefix
    internalNewExactName n

-- | Create a fresh name (internal)
-- fsMkFreshName :: String -> FreshState -> (Int, FreshState)
-- fsMkFreshName prefix (FreshState index p names) = let
--     name :: String
--     name = findFreshName names prefix
--     in (index, FreshState (index+1) p (mappedInsert index name names))


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
slurpNames :: (Show n, Monoid n, Ord n, Traversable f) =>
    Int -> f n -> (Map Int n, f Int)
slurpNames initialIndex fn = runIdentity $ unsafeRunNamingT initialIndex $ do
    fi <- traverse autoMapName fn
    nm <- Bimap.toMap <$> getNameMap
    return (nm, fi)
