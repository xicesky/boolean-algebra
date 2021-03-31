
module BooleanAlgebra.Transform.VariableSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import BooleanAlgebra
import BooleanAlgebra.Support.Arbitrary

import Debug.Trace

{-# ANN module "HLint: ignore Redundant $" #-}

{-----------------------------------------------------------------------------}

-- prop_nameLiterals_inverts :: BooleanExprLit -> Bool
-- prop_nameLiterals_inverts term = let
--     nameMap :: NameMap
--     numberedTerm :: BooleanExprLitI
--     (nameMap, numberedTerm) = numberLiterals term
--     in nameLiterals nameMap numberedTerm == term

-- prop_nameLiterals_inverts_CNF :: CNF -> Bool
-- prop_nameLiterals_inverts_CNF term = let
--     nameMap :: NameMap
--     numberedTerm :: BooleanExprLitI
--     (nameMap, numberedTerm) = numberLiterals term
--     in nameLiterals nameMap numberedTerm == term

-- spec_nameLiterals = describe "nameLiterals" $ do
--     prop "is the inverse of  'numberLiterals'" prop_nameLiterals_inverts

{-----------------------------------------------------------------------------}
-- HSpec

spec :: Spec
spec = do
    --spec_nameLiterals
    it "doesn't exist anymore" $ pending
