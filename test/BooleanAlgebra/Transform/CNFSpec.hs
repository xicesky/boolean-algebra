
module BooleanAlgebra.Transform.CNFSpec where

import Data.Void
import Control.Monad (join)
import Data.Foldable (toList)
import Data.List (elemIndex)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Term.Term
import BooleanAlgebra
import BooleanAlgebra.Examples
import qualified BooleanAlgebra.Base.Logic as L
import BooleanAlgebra.Support.Eval

regression01In :: TermLit BFlOps Void Int
regression01In = TermLit $ BConj [ BDisj
    [   BConj [ Var (Lit c) | c <- [1..3] ]
    ,   BConj [ Var (Lit c) | c <- [4..6] ]
    ]]

regression01Ex :: CNF Int
regression01Ex = CNF $ Conjunction [
    Disjunction [ Lit c1, Lit c2 ] | c1 <- [1..3], c2 <- [4..6]
    ]

regression02In :: BooleanExpr String
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
regression02Info :: CNF String -> [Int]
regression02Info (CNF (Conjunction xs)) =
    fmap (count ((== "N1P1") . snd) . toList) xs

regression02Check :: CNF String -> Bool
regression02Check cnf =
    -- N1P1 occurs at most once in each clause
    maximum (regression02Info cnf) == 1

prop_CNF_preservesSolutions :: HasCallStack => BooleanExpr String -> Property
prop_CNF_preservesSolutions t = propEqual t (toCNF t)

-- TODO: This test really requires a SAT solver to be reliable
prop_CNF2_preservesSolutions :: HasCallStack => BooleanExpr String -> Property
prop_CNF2_preservesSolutions t = propImplies (toCNF2 t) t

spec_distributeToCNF = describe "distributeToCNF" $ do
    it "distributes correctly" $ distributeToCNF regression01In `shouldBe` regression01Ex

spec_toCNF = describe "toCNF" $ do
    it "works on regression 02" $ toCNF regression02In `shouldSatisfy` regression02Check
    prop "preserves solutions" $ mapSize (`div` 2) $ prop_CNF_preservesSolutions

spec_toCNF2 = describe "toCNF2" $ do
    prop "preserves solutions" $ mapSize (`div` 2) $ prop_CNF2_preservesSolutions

spec :: Spec
spec = do
    spec_distributeToCNF
    spec_toCNF
    spec_toCNF2
