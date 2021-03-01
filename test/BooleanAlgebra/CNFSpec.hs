
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-} 

module BooleanAlgebra.CNFSpec where

import Control.Monad (join)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import BooleanAlgebra.Class
import BooleanAlgebra.Base
import BooleanAlgebra.Pretty
import BooleanAlgebra.Simplify
import BooleanAlgebra.Aggregate
import BooleanAlgebra.CNF
import BooleanAlgebra.Examples
import qualified Gen as G

-- distributeToCNF :: DistributeDoC f => Term f -> CNF
regression01In :: BooleanExprCDLit
regression01In = iBooleanCD [
    [   iBooleanCD [[ iPos [c] | c <- "abc" ]]
    ,   iBooleanCD [[ iPos [c] | c <- "def" ]]
    ]]

regression01Ex :: CNF
regression01Ex = BooleanCD [[ lPos [c1], lPos [c2] ] | c1 <- "abc", c2 <- "def" ]

regression02In :: BooleanExpr
regression02In = let
    isAt :: BooleanAlgebra b => Int -> Int -> b
    isAt n i = var $ "N" ++ show n ++ "P" ++ show i
    in  
    G.forAll [1..3] $ \number ->
    G.existsUnique [1..3] $ \position ->
    number `isAt` position

count x = length . filter (==x)

regression02Check :: CNF -> Bool
regression02Check (BooleanCD xs) =
    -- N1P1 occurs at most once in each clause
    (maximum . fmap (count $ BooleanLit True "vN1P1")) xs == 1

spec_distributeToCNF = describe "distributeToCNF" $ do
    it "distributes correctly" $ distributeToCNF regression01In `shouldBe` regression01Ex

spec_toCNF = describe "toCNF" $ do
    it "works on regression 02" $ toCNF regression02In `shouldSatisfy` regression02Check

spec :: Spec
spec = do
    spec_distributeToCNF
    spec_toCNF

