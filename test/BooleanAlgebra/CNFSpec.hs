
module BooleanAlgebra.CNFSpec where

import Control.Monad (join)
import Data.Foldable (toList)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import BooleanAlgebra
import BooleanAlgebra.Examples
import qualified Gen as G

regression01In :: BooleanExprFlatLit
regression01In = iConjunction [ iDisjunction
    [   iConjunction [ iPos [c] | c <- "abc" ]
    ,   iConjunction [ iPos [c] | c <- "def" ]
    ]]

regression01Ex :: CNF
regression01Ex = Conjunction [ Disjunction [ lPos [c1], lPos [c2] ] | c1 <- "abc", c2 <- "def" ]

regression02In :: BooleanExpr
regression02In = let
    isAt :: BooleanAlgebra b => Int -> Int -> b
    isAt n i = var $ "N" ++ show n ++ "P" ++ show i
    in  
    G.forAll [1..3] $ \number ->
    G.existsUnique [1..3] $ \position ->
    number `isAt` position

count :: Eq a => (a -> Bool) -> [a] -> Int
count x = length . filter x

-- Count appeareances of "N1P1" per clause
regression02Info :: CNF -> [Int]
regression02Info (Conjunction xs) = fmap (count ((=="N1P1") . litName) . toList) xs

regression02Check :: CNF -> Bool
regression02Check cnf =
    -- N1P1 occurs at most once in each clause
    maximum (regression02Info cnf) == 1

spec_distributeToCNF = describe "distributeToCNF" $ do
    it "distributes correctly" $ distributeToCNF regression01In `shouldBe` regression01Ex

spec_toCNF = describe "toCNF" $ do
    it "works on regression 02" $ toCNF regression02In `shouldSatisfy` regression02Check

spec :: Spec
spec = do
    spec_distributeToCNF
    spec_toCNF
