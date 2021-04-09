
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Control.Monad.Naming.GenNameT where

-- mtl / transformers
import Control.Monad.Identity
import Control.Monad.State

-- optics
import Optics
import Optics.TH (makeFieldLabelsWith, noPrefixFieldLabels)
--import Optics.TH (makeFieldLabelsNoPrefix)  -- req optics-0.4

import Missing.Bimap
import qualified Missing.Bimap as Bimap
import Missing.Optics

import Control.Monad.Naming.Class

{-
TODO:
    - add a randomization source
-}

{-----------------------------------------------------------------------------}

{- | Internal state of 'GenNameT'.
-}
data GenNameTState n = GenNameTState
    {   nsNextIndex :: Int              -- ^ the next free integer index
    ,   nsPrefix :: n                   -- ^ the default prefix for fresh names
    ,   nsNames :: Bimap Int n          -- ^ mapping @name \<-\> Int@
    ,   nsScheme :: NamingFun n         -- ^ a name generator
    }

-- makeFieldLabelsWith noPrefixFieldLabels ''GenNameTState

{-----------------------------------------------------------------------------}
-- Manual splices

-- TODO Remove this section and use makeFieldLabelsWith (above) when
-- haskell-language-server can handle template haskell without crashing.
-- https://github.com/haskell/haskell-language-server/issues/1297
-- https://github.com/haskell/haskell-language-server/issues/1342

{-# ANN module "HLint: ignore Redundant bracket" #-}
{-# ANN module "HLint: ignore Avoid lambda" #-}

-- makeFieldLabelsWith noPrefixFieldLabels ''GenNameTState
-- ======>
instance (k_a74E ~ A_Lens,
          a_a74F ~ Bimap Int n_a718,
          b_a74G ~ Bimap Int n_a718) =>
         LabelOptic "nsNames" k_a74E (GenNameTState n_a718) (GenNameTState n_a718) a_a74F b_a74G where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_a74H s_a74I
           -> case s_a74I of {
                GenNameTState x1_a74J x2_a74K x3_a74L x4_a74M
                  -> (fmap
                        (\ y_a74N -> (((GenNameTState x1_a74J) x2_a74K) y_a74N) x4_a74M))
                       (f_a74H x3_a74L) })
instance (k_a74O ~ A_Lens, a_a74P ~ Int, b_a74Q ~ Int) =>
         LabelOptic "nsNextIndex" k_a74O (GenNameTState n_a718) (GenNameTState n_a718) a_a74P b_a74Q where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_a74R s_a74S
           -> case s_a74S of {
                GenNameTState x1_a74T x2_a74U x3_a74V x4_a74W
                  -> (fmap
                        (\ y_a74X -> (((GenNameTState y_a74X) x2_a74U) x3_a74V) x4_a74W))
                       (f_a74R x1_a74T) })
instance (k_a74Y ~ A_Lens, a_a74Z ~ n_a718, b_a750 ~ n_a718) =>
         LabelOptic "nsPrefix" k_a74Y (GenNameTState n_a718) (GenNameTState n_a718) a_a74Z b_a750 where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_a751 s_a752
           -> case s_a752 of {
                GenNameTState x1_a753 x2_a754 x3_a755 x4_a756
                  -> (fmap
                        (\ y_a757 -> (((GenNameTState x1_a753) y_a757) x3_a755) x4_a756))
                       (f_a751 x2_a754) })
instance (k_a758 ~ A_Lens,
          a_a759 ~ NamingFun n_a718,
          b_a75a ~ NamingFun n_a718) =>
         LabelOptic "nsScheme" k_a758 (GenNameTState n_a718) (GenNameTState n_a718) a_a759 b_a75a where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_a75b s_a75c
           -> case s_a75c of {
                GenNameTState x1_a75d x2_a75e x3_a75f x4_a75g
                  -> (fmap
                        (\ y_a75h -> (((GenNameTState x1_a75d) x2_a75e) x3_a75f) y_a75h))
                       (f_a75b x4_a75g) })


{-----------------------------------------------------------------------------}

-- | Monad transformer with the ability to create fresh names.
newtype GenNameT n m a = GenNameT { toStateT :: StateT (GenNameTState n) m a }

deriving instance Functor m => Functor (GenNameT n m)
deriving instance Monad m => Applicative (GenNameT n m)
deriving instance Monad m => Monad (GenNameT n m)
deriving instance MonadTrans (GenNameT n)

deriving instance MonadIO m => MonadIO (GenNameT n m)
-- ... and so on... implement as we need it

instance Show n => Show (GenNameTState n) where
    showsPrec d (GenNameTState nextIndex prefix names scheme)
        = showParen (d >= 11) $ showString "GenNameTState {"
        . showString "nsNextIndex = "
        . shows nextIndex
        . showString ", nsPrefix = "
        . shows prefix
        . showString ", nsNames = "
        . shows names
        . showString ", nsScheme = <?> }"

instance Monad m => MonadUniqueInt (GenNameT n m) where
    stateIndex = GenNameT . stateFun #nsNextIndex

instance (Monad m, Monoid n, Ord n) => MonadName n (GenNameT n m) where
    stateNames = GenNameT . stateFun #nsNames

instance (Monad m, Monoid n, Ord n) => MonadGenName n (GenNameT n m) where
    statePrefix = GenNameT . stateFun #nsPrefix
    stateScheme = GenNameT . stateFun #nsScheme

{-----------------------------------------------------------------------------}

-- | Run the 'GenNameT' monad transformer.
runGenNameT :: forall m n a. (Monad m, Monoid n, Ord n) =>
    Int -> NamingFun n -> GenNameT n m a -> m a
runGenNameT initialIndex nfun namet = do
    (a, _) <- runStateT (toStateT namet) initGenNameTState
    return a
    where
        initGenNameTState :: GenNameTState n
        initGenNameTState = GenNameTState
            {   nsNextIndex = initialIndex
            ,   nsPrefix = mempty
            ,   nsNames = Bimap.empty
            ,   nsScheme = nfun
            }

-- | Run the 'GenNameT' monad transformer using defaults.
runGenNameTString :: Monad m => Int -> GenNameT String m a -> m a
runGenNameTString initialIndex = runGenNameT initialIndex defaultNamingFun

{- | Lift a function operating on integers to one operating on names.
-}
liftNamesM :: forall m f g. (Monad m, Traversable f, Traversable g) =>
    Int -> (f Int -> GenNameT String m (g Int)) -> f String -> m (g String)
liftNamesM initialIndex f fs = runGenNameTString initialIndex wrap where
    wrap :: GenNameT String m (g String)
    wrap = do
        fi <- traverse autoMapName fs
        gi <- f fi
        traverse unsafeNameOf gi

{- | Lift a function operating on integers to one operating on names.

Non-monadic variant of 'liftNamesM'.
-}
liftNames :: forall f g. (Traversable f, Traversable g) =>
    Int -> (f Int -> GenNameT String Identity (g Int)) -> f String -> g String
liftNames initialIndex f fs = runIdentity $ liftNamesM initialIndex f fs

{-----------------------------------------------------------------------------}
