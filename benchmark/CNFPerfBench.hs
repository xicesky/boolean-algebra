{-
Criterion-based benchmarks for cnf transformations.
http://www.serpentine.com/criterion/tutorial.html

-}

module CNFPerfBench where

import Data.Functor.Identity (Identity(..))

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Criterion.Main

import BooleanAlgebra
--import BooleanAlgebra.Problem.Encoding
import Gen

problem :: BooleanExpr String
problem = pidgeonHole' 3

-- | Run SAT or throw an error
solveProblem :: (BooleanExpr String -> CNF String) -> BooleanExpr String -> SatResult String
solveProblem toCNF problem = runIdentity $ runSatT (error . show) $
        solve BasicSolver (toCNF problem)

benchmarks :: [Benchmark]
benchmarks = 
    [ bench "toCNF" $ whnf (solveProblem toCNF) problem
    -- this runs for ages because of bad solver / bad transformation combo
    , bench "toCNF2" $ whnf (solveProblem toCNF2) problem
    ]

main :: IO ()
main = defaultMain benchmarks
