
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Description     : Generators for QuickCheck
Stability       : experimental

This module provides instances of 'Arbitrary' for terms and
various generates for use with QuickCheck.
-}
module BooleanAlgebra.Support.Arbitrary
    (   -- * Terms
        generateBA
    ,   generateTerm

    ,   -- * Variable assignments
        generateMapping
    ,   generateMapping'
    ,   tryGenerateMapping

        -- * Other utilities
    ,   generateVarName
    ,   chooseBs

    ) where

import Prelude hiding (and, or, not, (&&), (||))

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Modifiers

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Term.Term

import BooleanAlgebra.Base.Class
import BooleanAlgebra.Base.Expression
-- import BooleanAlgebra.Base.Pretty
import BooleanAlgebra.Transform.Variable

-- import BooleanAlgebra.Transform.Simplify
-- import BooleanAlgebra.Transform.CNF

import GHC.Stack (HasCallStack)
-- import Debug.Trace

{- | Choose between two alternatives, with
    @x@ in @n@ chances to take the first
-}
choiceOf :: Int -> Int -> Gen a -> Gen a -> Gen a
choiceOf x n a b = chooseInt (1, n) >>= \case
    i   | i <= x    -> a
    i               -> b

-- | Generate a single boolean value
chooseB :: Gen Bool
chooseB = chooseEnum (minBound, maxBound)

-- | Generate a list of @n@ booleans
chooseBs :: Int -> Gen [Bool]
chooseBs n = vectorOf n chooseB

-- | Generate a random assignment for variables @vars@
generateMapping :: forall name. Ord name =>
    [name] -> Gen (Map name Bool)
generateMapping vars = do
        vals <- chooseBs (length vars)
        return $ Map.fromList $ zip vars vals

-- | Generate a random assignment for variables in term @t@
generateMapping' :: forall t. (HasNames t, Ord (NameT t)) =>
    t -> Gen (Map (NameT t) Bool)
generateMapping' term = let
    vars :: [NameT t]
    vars = Set.toList $ variableNames term
    in generateMapping vars

-- | Generate a random assignment for variables @vars@
-- Try a few times to satisfy a property (such as satisfying the term).
tryGenerateMapping :: forall name. Ord name =>
    Int -> (Map name Bool -> Bool) -> [name] -> Gen (Map name Bool)
tryGenerateMapping tries test vars = resize tries $
    generateMapping vars `suchThatMaybe` test >>= \case
        Just mapping    -> return mapping
        Nothing         -> generateMapping vars     -- give an arbitrary mapping

-- TODO generalize and put into our Term library

-- | Generate a boolean term of a maximum @size@ from atoms @vasr@ and @vals@
generateTerm :: forall var val. HasCallStack
    => Int -> [var] -> [val] -> Gen (Term BOps val var)
generateTerm size vars vals = term size where
    atom :: Gen (Term BOps val var)
    atom    | null vals = Var <$> elements vars
            | otherwise = choiceOf 1 6 (Val <$> elements vals) (Var <$> elements vars)
    term :: Int -> Gen (Term BOps val var)
    term x | x <= 0     = atom
    term size           = -- trace ("term " ++ show size) $
        chooseInt (0,9) >>= \case
        0            -> atom
        1            -> not <$> term (size - 1)
        n   | even n -> and <$> term (size `div` 2) <*> term (size `div` 2)
        n   | odd n  -> or  <$> term (size `div` 2) <*> term (size `div` 2)

-- | Generate a random variable name
generateVarName :: Gen String
generateVarName = do
    l <- (`div` 10) <$> getSize
    vectorOf l (elements ['a'..'z'])
-- generateVarName = getPrintableString <$> arbitrary
-- generateVarName = getUnicodeString <$> arbitrary

-- | Generate any instance of BooleanAlgebra
generateBA :: forall b. BooleanAlgebra b => Gen b
generateBA = do
    n <- getSize
    -- overlapping variables aren't a problem here
    vars <- vectorOf (n+1) generateVarName
    (term :: Term BOps Bool String) <- generateTerm n vars []
    return $ interpretAlg term

instance Arbitrary (BooleanExpr String) where
    arbitrary = generateBA

    shrink (BUOp op t) = t : [BUOp op t' | t' <- shrink t]
    shrink (BBOp op a b) = a : b : [BBOp op a' b' | (a', b') <- shrink (a, b)]
    shrink (BFlOp op xs) = xs ++ [BFlOp op xs' | xs' <- shrink xs]
    shrink _ = []

-- -- FIXME: Weird slow and ineffective
-- -- Generate BooleanExprLit directly instead

-- data Named t = Named [String] t
--     deriving (Show, Eq, Ord, Functor)
-- toNamed (ns, t) = Named ns t

-- instance Arbitrary (Named BooleanExprLit) where
--     arbitrary = sized $ \case
--         0 -> return $ Named ["?"] (iBooleanLit 0)
--         _ -> let
--             t0 :: Gen ([String], MaybeTrivial BooleanExprLit)
--             t0 = simplify <$> (arbitrary :: Gen BooleanExpr)
--             -- Try until you get it right :)
--             t1 :: Gen (Named BooleanExprLit)
--             t1 = t0 >>= \case
--                 (_, Left _) -> arbitrary
--                 (names, Right (t :: BooleanExprLit))
--                     -> return $ Named names t
--             in -- trace ("arbitrary (BooleanExprLit) ") $
--                 t1

-- instance Arbitrary (Named CNF) where
--     arbitrary = scale (`div` 2) $
--         toNamed . toCNF <$> (arbitrary :: Gen BooleanExpr)
