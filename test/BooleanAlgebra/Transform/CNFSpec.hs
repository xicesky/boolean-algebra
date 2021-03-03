
module BooleanAlgebra.Transform.CNFSpec where

import Control.Monad (join)
import Data.Foldable (toList)
import Data.List (elemIndex)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import BooleanAlgebra
import BooleanAlgebra.Transform.IntermediateForms
import BooleanAlgebra.Examples
import qualified BooleanAlgebra.Base.Logic as L

regression01In :: BooleanExprFlatLit
regression01In = iConjunction [ iDisjunction
    [   iConjunction [ iLit c | c <- [1..3] ]
    ,   iConjunction [ iLit c | c <- [4..6] ]
    ]]

regression01Ex :: CNF
regression01Ex = Conjunction [ Disjunction [ BooleanLit c1, BooleanLit c2 ] | c1 <- [1..3], c2 <- [4..6] ]

regression02In :: BooleanExpr
regression02In = let
    isAt :: BooleanAlgebra b => Int -> Int -> b
    isAt n i = var $ "N" ++ show n ++ "P" ++ show i
    in  
    L.forAll [1..3] $ \number ->
    L.existsUnique [1..3] $ \position ->
    number `isAt` position

count :: Eq a => (a -> Bool) -> [a] -> Int
count x = length . filter x

-- Count appeareances of "N1P1" per clause
regression02Info :: [String] -> CNF -> [Int]
regression02Info names (Conjunction xs) = let
    n1p1Index :: Int
    Just n1p1Index = elemIndex "N1P1" names
    in fmap (count ((== n1p1Index) . unLit) . toList) xs

regression02Check :: Named CNF -> Bool
regression02Check (Named names cnf) =
    -- N1P1 occurs at most once in each clause
    maximum (regression02Info names cnf) == 1

toCNF' = toNamed . toCNF

spec_distributeToCNF = describe "distributeToCNF" $ do
    it "distributes correctly" $ distributeToCNF regression01In `shouldBe` regression01Ex

spec_toCNF = describe "toCNF" $ do
    it "works on regression 02" $ toCNF' regression02In `shouldSatisfy` regression02Check

spec :: Spec
spec = do
    spec_distributeToCNF
    spec_toCNF
