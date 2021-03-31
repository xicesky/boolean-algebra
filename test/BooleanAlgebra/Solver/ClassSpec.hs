
module BooleanAlgebra.Solver.ClassSpec where

import Control.Exception (throwIO)

import Prelude hiding (and, or, not, (&&), (||))

-- hspec & quickcheck
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic

import BooleanAlgebra
import BooleanAlgebra.Format.Dimacs (toDimacs, toDimacsVars)
import BooleanAlgebra.Support.Minisat

{-# ANN module "HLint: ignore Redundant $" #-}

{-----------------------------------------------------------------------------}

-- | Run the solver, throwing errors in IO
runSolver :: forall name s. (ProperName name, Solver s IO) =>
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
    Context _ cnfi = buildContext cnf
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

{-----------------------------------------------------------------------------}
-- HSpec

spec_BasicSolver :: Spec
spec_BasicSolver = describe "BasicSolver" $
    prop "solves CNF" $ mapSize (`div` 8) $ -- yes, div 8. This solver is slow.
        prop_solves_CNF BasicSolver 100000

spec_Minisat :: Spec
spec_Minisat = describe "Minisat" $
    prop "solves CNF" $ mapSize (`div` 4) $ -- div 4 because to toCNF
        prop_solves_CNF (Minisat "minisat") 500000

spec :: Spec
spec = do
    spec_BasicSolver
    spec_Minisat
    -- it "is WIP" $ pending
