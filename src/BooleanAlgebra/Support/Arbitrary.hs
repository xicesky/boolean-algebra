
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | 'Arbitrary' instances for QuickCheck usage

module BooleanAlgebra.Support.Arbitrary where

import Prelude hiding (and, or, not, (&&), (||))

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Modifiers

import BooleanAlgebra.Base.Class
import BooleanAlgebra.Base.Expression
import BooleanAlgebra.Base.Pretty
import BooleanAlgebra.Transform.Simplify
import BooleanAlgebra.Transform.CNF

import Debug.Trace

chooseB :: Gen Bool
chooseB = chooseEnum (minBound, maxBound)

generateBoolean :: forall b. Boolean b => [b] -> Gen b
generateBoolean atoms = let
    literal :: Gen b
    literal = do
        sign <- chooseB
        ifthenelse sign id not <$> elements atoms
    term :: Int -> Gen b
    term x | x <= 0   = literal
    term size  = -- trace ("term " ++ show size) $
        chooseInt (0,9) >>= \case
        0            -> literal
        1            -> not <$> term (size - 1)
        n   | even n -> and <$> term (size `div` 2) <*> term (size `div` 2)
        n   | odd n  -> or  <$> term (size `div` 2) <*> term (size `div` 2)
    in sized term

generateVarName :: Gen String
generateVarName = do
    l <- (`div` 10) <$> getSize
    vectorOf l (elements ['a'..'z'])
-- generateVarName = getPrintableString <$> arbitrary
-- generateVarName = getUnicodeString <$> arbitrary


generateBA :: forall b. BooleanAlgebra b => Gen b
generateBA = do
    n <- getSize
    -- overlapping variables aren't a problem here
    vars <- vectorOf n (var <$> generateVarName)
    generateBoolean (true : false : vars)

instance Arbitrary BooleanExpr where
    arbitrary = generateBA

-- TODO: Generate BooleanExprLit directly

instance Arbitrary BooleanExprLit where
    arbitrary = sized $ \case
        0 -> return $ iPos "a"
        _ -> let
            t0 :: Gen (MaybeTrivial BooleanExprLit)
            t0 = simplify <$> (arbitrary :: Gen BooleanExpr)
            -- Try until Right
            t1 :: Gen BooleanExprLit
            t1 = t0 >>= \case
                Left _ -> arbitrary
                Right (t :: BooleanExprLit) -> return t
            in -- trace ("arbitrary (BooleanExprLit) ") $
                t1

instance Arbitrary CNF where
    arbitrary = scale (`div` 2) $ toCNF <$> (arbitrary :: Gen BooleanExpr)
