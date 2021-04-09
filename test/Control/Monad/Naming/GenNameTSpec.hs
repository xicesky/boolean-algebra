
module Control.Monad.Naming.GenNameTSpec where

import Data.Foldable (length)
import Data.Functor.Identity

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- hspec & quickcheck
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic

import Control.Monad.Naming.Class
import Control.Monad.Naming.GenNameT

{-# ANN module "HLint: ignore Redundant $" #-}

{-----------------------------------------------------------------------------}
-- Utilities

type Naming = GenNameT String Identity

-- | Test that each entry in a list is unique
allDifferent :: [String] -> Bool
allDifferent xs = length (Set.fromList xs) == length xs

-- | Run GenNameT for property testing
monadicGenNameT :: Testable a => PropertyM Naming a -> Property
monadicGenNameT = monadic run where
    run :: Naming Property -> Property
    run = runIdentity . runGenNameTString 1

{-----------------------------------------------------------------------------}

prop_newNamed_generates_uniques :: Int -> Property
prop_newNamed_generates_uniques count = monadicGenNameT $ do
    names <- genNames count
    return $ allDifferent names
    where
    genNames :: Int -> PropertyM Naming [String]
    genNames count
        | count <= 0    = return []
        | otherwise     = do
        -- Generate a name
        i0 <- run peekIndex
        v  <- run newNamed
        assert $ i0 == v

        -- Index has increased
        i1 <- run peekIndex
        assert $ i0 < i1

        -- Name should be available
        (Just vn) <- run $ nameOf v

        -- Continue with more variables
        rest <- genNames (count - 1)

        -- Map back to strings
        return $ vn : rest


spec_newNamed :: Spec
spec_newNamed = describe "newNamed" $ do
    prop "generates unique names" $ prop_newNamed_generates_uniques

{-----------------------------------------------------------------------------}

{- | The following law holds:

prop> liftNames n return == id
-}
prop_liftNames_return_id :: Int -> [String] -> Property
prop_liftNames_return_id n strs =
    liftNames n return strs === strs

spec_liftNames :: Spec
spec_liftNames = describe "liftNames" $ do
    prop "law: liftNames return == id" $ prop_liftNames_return_id

{-----------------------------------------------------------------------------}

spec :: Spec
spec = do
    spec_newNamed
    spec_liftNames
