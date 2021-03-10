
{-# LANGUAGE AllowAmbiguousTypes #-}

-- Temporary module for playing around in ghci
module Interactive where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import BooleanAlgebra
import BooleanAlgebra.Examples

import Debug.Trace

-- prop_nameLiterals_inverts :: BooleanExprLit -> Bool
-- prop_nameLiterals_inverts term = let
--     nameMap :: NameMap
--     numberedTerm :: BooleanExprLitI
--     (nameMap, numberedTerm) = numberLiterals term
--     in trace ("prop_nameLiterals_inverts " ++ prettyBool term) $
--     nameLiterals nameMap numberedTerm == term

-- checkArbitrary @CNF
checkArbitrary :: forall a. (Arbitrary a, Show a, Eq a) => IO ()
checkArbitrary = quickCheck (\(x::a) -> x == x)
