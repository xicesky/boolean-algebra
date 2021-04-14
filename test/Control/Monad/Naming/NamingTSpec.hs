
module Control.Monad.Naming.NamingTSpec where

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- hspec & quickcheck
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic

import Control.Monad.Naming.Class
import Control.Monad.Naming.NamingT

{-# ANN module "HLint: ignore Redundant $" #-}

{-----------------------------------------------------------------------------}

prop_reattachNames_inverts_slurpNames :: [String] -> Property
prop_reattachNames_inverts_slurpNames term = let
    nameMap :: Map Int String
    numberedTerm :: [Int]
    (nameMap, numberedTerm) = slurpNames 1 term
    in reattachNames nameMap numberedTerm === term

spec_slurpNames :: Spec
spec_slurpNames = describe "slurpNames" $ do
    prop "is reverted by reattachNames" $ prop_reattachNames_inverts_slurpNames

{-----------------------------------------------------------------------------}

spec :: Spec
spec = do
    spec_slurpNames
