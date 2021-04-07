
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BooleanAlgebra.Solver.ClassSpec where

import Prelude hiding (and, or, not, (&&), (||))
import Data.Typeable (Typeable)
import Control.Exception (throwIO)

-- hspec & quickcheck
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic

import BooleanAlgebra
import BooleanAlgebra.Format.Dimacs (toDimacs, toDimacsVars)
import BooleanAlgebra.Support.Minisat

-- TODO: useful examples should be part of the library
import Gen (pidgeonHole')

import Debug.Trace

{-# ANN module "HLint: ignore Redundant $" #-}

{-----------------------------------------------------------------------------}

-- | Helper for generating VERY small natural numbers: 1 <= n <= 5
newtype VerySmallNat = VerySmallNat Int
    deriving (Show, Eq, Ord, Num, Bounded, Enum)

instance Arbitrary VerySmallNat where
    arbitrary = chooseEnum (1, 5)

{-----------------------------------------------------------------------------}

-- TODO should probably be in Base.Expression
-- FIXME: That's not a "Name" because it must work for Int, too
-- | Proper variable names for use in sat solving
class (Show name, Ord name, Typeable name) => ProperName name where
    {-# MINIMAL #-}

instance ProperName String
instance ProperName Int

-- | Run the solver, throwing errors in IO
runSolver :: forall name s. (ProperName name, Monoid name, Solver s IO) =>
    s -> CNF name -> PropertyM IO (SatResult name)
runSolver s cnf = run $ runSatT handleError $ solve s cnf where
    handleError :: SatError name -> IO a
    handleError = throwIO

-- | For a given problem, toCNF and toCNF2 are equally solvable
prop_solves_CNF :: Solver s IO => s -> Int -> BooleanExpr String -> Property
prop_solves_CNF s timeLimit expr = within timeLimit $ monadicIO $ let
    cnf1 = toCNF expr
    cnf2 = toCNF2 expr
    in do
        r1 <- runSolver s cnf1
        r2 <- runSolver s cnf2
        assert (r1 `eqSat` r2)

prop_detects_unsat :: Solver s IO => s -> Int -> VerySmallNat -> Property
prop_detects_unsat s timeLimit (VerySmallNat probSize) = (probSize > 0) ==>
    withMaxSuccess 10 $ -- reduce amount of runs
    within timeLimit $ monadicIO $ do
        --traceM $ "prop_detects_unsat s " ++ show probSize
        -- We are using 'toCNF' here, because it's already in CNF
        result <- runSolver s $ toCNF $ (pidgeonHole' probSize :: BooleanExpr String)
        assert (result == Unsat)

{-----------------------------------------------------------------------------}
-- For interactive use only

--mapSize (`div` 2) $ within 100000 $ prop_solves_CNF BasicSolver

slowExample01 :: BooleanExpr String
slowExample01 = (BNot (Var "bz") `BAnd` Var "vw") `BAnd` ((Var "cq" `BAnd` (Var "sv"
    `BOr` Var "xb")) `BOr` (((Var "vc" `BOr` Var "sz") `BOr` (Var "yb"
    `BOr` Var "yg")) `BOr` ((Var "ga" `BOr` Var "pu") `BOr` (Var "ma" `BAnd` Var "ld"))))

slowExample02 :: BooleanExpr String
slowExample02 = BNot ((BNot (BNot (Var "rq" `BOr` (Var "ta" `BOr` Var "kk")))
    `BOr` ((BNot (Var "ta") `BOr` Var "rx") `BOr` (Var "tq" `BOr` (Var "qu"
    `BAnd` Var "tw")))) `BAnd` (Var "cx" `BOr` ((Var "wp" `BAnd` (Var "wp"
    `BOr` Var "me")) `BOr` ((Var "fw" `BOr` Var "jw") `BOr` Var "vf"))))

minisatFail :: BooleanExpr String
minisatFail = Var "" `BAnd` Var ""

cnfInt :: CNF String -> CNF Int
cnfInt cnf = let
    (_, cnfi) = slurpNames cnf
    in cnfi

dimacs :: CNF String -> IO ()
dimacs = putStrLn . toDimacsVars

prop_dummy :: BooleanExpr String -> Bool
prop_dummy expr = length (variableNames expr) < 5

qcEx0 :: IO ()
qcEx0 = quickCheck $ verboseShrinking $ prop_dummy

qcEx1 :: IO ()
qcEx1 = quickCheck $ -- quickCheckWith (stdArgs {maxShrinks = 3}) $ 
    mapSize (`div` 2) $ prop_solves_CNF BasicSolver 100000

quickSolve :: BooleanExpr String -> IO (Either (SatError String) (SatResult String))
quickSolve = solve' BasicSolver . toCNF

{-----------------------------------------------------------------------------}
-- HSpec

spec_BasicSolver :: Spec
spec_BasicSolver = describe "BasicSolver" $ do
    prop "solves CNF" $ mapSize (`div` 8) $ -- yes, div 8. This solver is slow.
        prop_solves_CNF BasicSolver 100000
    prop "detects UNSAT" $
        prop_detects_unsat BasicSolver 100000

spec_Minisat :: Spec
spec_Minisat = describe "Minisat" $ do
    prop "solves CNF" $ mapSize (`div` 4) $ -- div 4 because to toCNF
        prop_solves_CNF (Minisat "minisat") 500000
    prop "detects UNSAT" $
        prop_detects_unsat BasicSolver 500000

spec :: Spec
spec = do
    spec_BasicSolver
    spec_Minisat
