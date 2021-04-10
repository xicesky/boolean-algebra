
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Control.Monad.Naming.GenNameT where

-- mtl / transformers
import Control.Monad.Identity
import Control.Monad.State.Strict

-- optics
import Optics
import Optics.TH (makeFieldLabelsWith, noPrefixFieldLabels)
--import Optics.TH (makeFieldLabelsNoPrefix)  -- req optics-0.4

import Missing.Bimap
import qualified Missing.Bimap as Bimap
import Missing.Optics

import Control.Monad.Naming.Class

{-
TODO / Ideas:
    - add a randomization source
-}

{-----------------------------------------------------------------------------}

{- | Internal state of 'GenNameT'.
-}
data GenNameTState n = GenNameTState
    {   nsNextIndex :: Int              -- ^ the next free integer index
    ,   nsNames :: Bimap Int n          -- ^ mapping @name \<-\> Int@
    ,   nsPrefix :: n                   -- ^ the default prefix for fresh names
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
instance (k_a78N ~ A_Lens,
          a_a78O ~ Bimap Int n_a5BX,
          b_a78P ~ Bimap Int n_a5BX) =>
         LabelOptic "nsNames" k_a78N (GenNameTState n_a5BX) (GenNameTState n_a5BX) a_a78O b_a78P where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_a78Q s_a78R
           -> case s_a78R of {
                GenNameTState x1_a78S x2_a78T x3_a78U x4_a78V
                  -> (fmap
                        (\ y_a78W -> (((GenNameTState x1_a78S) y_a78W) x3_a78U) x4_a78V))
                       (f_a78Q x2_a78T) })
instance (k_a78X ~ A_Lens, a_a78Y ~ Int, b_a78Z ~ Int) =>
         LabelOptic "nsNextIndex" k_a78X (GenNameTState n_a5BX) (GenNameTState n_a5BX) a_a78Y b_a78Z where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_a790 s_a791
           -> case s_a791 of {
                GenNameTState x1_a792 x2_a793 x3_a794 x4_a795
                  -> (fmap
                        (\ y_a796 -> (((GenNameTState y_a796) x2_a793) x3_a794) x4_a795))
                       (f_a790 x1_a792) })
instance (k_a797 ~ A_Lens, a_a798 ~ n_a5BX, b_a799 ~ n_a5BX) =>
         LabelOptic "nsPrefix" k_a797 (GenNameTState n_a5BX) (GenNameTState n_a5BX) a_a798 b_a799 where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_a79a s_a79b
           -> case s_a79b of {
                GenNameTState x1_a79c x2_a79d x3_a79e x4_a79f
                  -> (fmap
                        (\ y_a79g -> (((GenNameTState x1_a79c) x2_a79d) y_a79g) x4_a79f))
                       (f_a79a x3_a79e) })
instance (k_a79h ~ A_Lens,
          a_a79i ~ NamingFun n_a5BX,
          b_a79j ~ NamingFun n_a5BX) =>
         LabelOptic "nsScheme" k_a79h (GenNameTState n_a5BX) (GenNameTState n_a5BX) a_a79i b_a79j where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_a79k s_a79l
           -> case s_a79l of {
                GenNameTState x1_a79m x2_a79n x3_a79o x4_a79p
                  -> (fmap
                        (\ y_a79q -> (((GenNameTState x1_a79m) x2_a79n) x3_a79o) y_a79q))
                       (f_a79k x4_a79p) })

{-----------------------------------------------------------------------------}

-- | Monad transformer with the ability to create fresh names.
newtype GenNameT n m a = GenNameT { unGenNameT :: StateT (GenNameTState n) m a }

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

instance (Monad m, Ord n) => MonadName n (GenNameT n m) where
    stateNames = GenNameT . stateFun #nsNames

instance (Monad m, Monoid n, Ord n) => MonadGenName n (GenNameT n m) where
    statePrefix = GenNameT . stateFun #nsPrefix
    stateScheme = GenNameT . stateFun #nsScheme

{-----------------------------------------------------------------------------}

-- | Run the 'GenNameT' monad transformer.
runGenNameT :: forall m n a. (Monad m, Monoid n, Ord n) =>
    Int -> NamingFun n -> GenNameT n m a -> m a
runGenNameT initialIndex nfun namet = do
    (a, _) <- runStateT (unGenNameT namet) initGenNameTState
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
