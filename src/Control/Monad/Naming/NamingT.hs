
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Control.Monad.Naming.NamingT where

-- containers
import Data.Map.Strict (Map)

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
TODO / Ideas:
    - Ability to remap names
-}

{-----------------------------------------------------------------------------}

{- | Internal state of 'NamingT'.
-}
data NamingTState n = NamingTState
    {   nsNextIndex :: Int              -- ^ the next free integer index
    ,   nsNames :: Bimap Int n          -- ^ mapping @name \<-\> Int@
    }
    deriving (Show, Eq, Ord)

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
instance (k_a73w ~ A_Lens,
          a_a73x ~ Bimap Int n_a5wN,
          b_a73y ~ Bimap Int n_a73v) =>
         LabelOptic "nsNames" k_a73w (NamingTState n_a5wN) (NamingTState n_a73v) a_a73x b_a73y where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_a73z s_a73A
           -> case s_a73A of {
                NamingTState x1_a73B x2_a73C
                  -> (fmap (\ y_a73D -> (NamingTState x1_a73B) y_a73D))
                       (f_a73z x2_a73C) })
instance (k_a73E ~ A_Lens, a_a73F ~ Int, b_a73G ~ Int) =>
         LabelOptic "nsNextIndex" k_a73E (NamingTState n_a5wN) (NamingTState n_a5wN) a_a73F b_a73G where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_a73H s_a73I
           -> case s_a73I of {
                NamingTState x1_a73J x2_a73K
                  -> (fmap (\ y_a73L -> (NamingTState y_a73L) x2_a73K))
                       (f_a73H x1_a73J) })

{-----------------------------------------------------------------------------}

-- | Monad transformer with the ability to map names.
newtype NamingT n m a = NamingT { toStateT :: StateT (NamingTState n) m a }

deriving instance Functor m => Functor (NamingT n m)
deriving instance Monad m => Applicative (NamingT n m)
deriving instance Monad m => Monad (NamingT n m)
deriving instance MonadTrans (NamingT n)

deriving instance MonadIO m => MonadIO (NamingT n m)
-- ... and so on... implement as we need it

instance Monad m => MonadUniqueInt (NamingT n m) where
    stateIndex = NamingT . stateFun #nsNextIndex

instance (Monad m, Ord n) => MonadName n (NamingT n m) where
    stateNames = NamingT . stateFun #nsNames

{-----------------------------------------------------------------------------}

-- | Run the 'NamingT' monad transformer.
runNamingT :: forall m n a. (Monad m, Ord n) =>
    Int -> NamingT n m a -> m a
runNamingT initialIndex namet = do
    (a, _) <- runStateT (toStateT namet) initNamingTState
    return a
    where
        initNamingTState :: NamingTState n
        initNamingTState = NamingTState
            {   nsNextIndex = initialIndex
            ,   nsNames = Bimap.empty
            }

{- | Separate names from a term into a map
-}
slurpNames :: (Ord n, Traversable f) =>
    Int -> f n -> (Map Int n, f Int)
slurpNames initialIndex fn = runIdentity $ runNamingT initialIndex $ do
    fi <- traverse autoMapName fn
    nm <- Bimap.toMap <$> getNameMap
    return (nm, fi)
