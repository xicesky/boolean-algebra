
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

{-# ANN module "HLint: ignore Redundant $" #-}
{-# ANN module "HLint: ignore Use camelCase" #-}

{-----------------------------------------------------------------------------}
-- Specific regression tests

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
    isAt :: BooleanAlgebra b String => Int -> Int -> b String
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

{-----------------------------------------------------------------------------}
-- cnfStats

validCNFStats :: CNFStats -> Bool
validCNFStats stats = Prelude.and
    [   nClauses stats >= 0
    ,   minClauseLength stats >= 0
    ,   maxClauseLength stats >= 0
    ,   averageClauseLength stats >= 0
    ,   realToFrac (minClauseLength stats) <= averageClauseLength stats
    ,   averageClauseLength stats <= realToFrac (maxClauseLength stats)
    ]

prop_cnfStats_valid :: BooleanExpr String -> Bool
prop_cnfStats_valid expr = let
    cnf :: CNF String
    cnf = toCNF2 expr
    in validCNFStats (cnfStats cnf)

spec_cnfStats :: Spec
spec_cnfStats = describe "cnfStats" $ do
    prop "returns valid results" $ prop_cnfStats_valid
    it "works on empty CNF" $ validCNFStats (cnfStats (CNF (Conjunction [])))
    it "works on empty clauses" $ validCNFStats (cnfStats (CNF (Conjunction [Disjunction []])))

{-----------------------------------------------------------------------------}
-- distributeToCNF

spec_distributeToCNF :: Spec
spec_distributeToCNF = describe "distributeToCNF" $ do
    it "distributes correctly" $ distributeToCNF regression01In `shouldBe` regression01Ex

{-----------------------------------------------------------------------------}
-- toCNF

prop_CNF_preservesSolutions :: BooleanExpr String -> Property
prop_CNF_preservesSolutions t = propEqual t (toCNF t)

spec_toCNF :: Spec
spec_toCNF = describe "toCNF" $ do
    it "works on regression 02" $ toCNF regression02In `shouldSatisfy` regression02Check
    prop "preserves solutions" $ mapSize (`div` 2) $ prop_CNF_preservesSolutions

{-----------------------------------------------------------------------------}
-- toCNF2

-- TODO: This test really requires a SAT solver to be reliable
prop_CNF2_preservesSolutions :: BooleanExpr String -> Property
prop_CNF2_preservesSolutions t = propImplies (toCNF2 t) t

spec_toCNF2 = describe "toCNF2" $ do
    prop "preserves solutions" $ mapSize (`div` 2) $ prop_CNF2_preservesSolutions

{-----------------------------------------------------------------------------}

spec :: Spec
spec = do
    spec_cnfStats
    spec_distributeToCNF
    spec_toCNF
    spec_toCNF2
