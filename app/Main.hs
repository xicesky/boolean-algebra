module Main where

import Criterion.Main

import BooleanAlgebra.Base
import BooleanAlgebra.Pretty
import BooleanAlgebra.Simplify
import BooleanAlgebra.Aggregate
import BooleanAlgebra.CNF
import BooleanAlgebra.Examples

{- Notes & interesting reads:
    https://en.wikipedia.org/wiki/Satisfiability_modulo_theories
    https://en.wikipedia.org/wiki/Constraint_satisfaction_problem
    https://curry.pages.ps.informatik.uni-kiel.de/curry-lang.org/
    http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.34.4164&rep=rep1&type=pdf

Inspiration for solver:
    https://www-ps.informatik.uni-kiel.de/~sebf/data/pub/atps09.pdf
    http://hackage.haskell.org/package/cflp
-}

demo :: BooleanExpr -> IO ()
demo ex = do
    putStr "Original    : "; printBool ex
    putStr "Simplified  : "; printBool $ simplifyPrimitive ex
    putStr "Intermediate: "; printBool $ aggregateConjDisj' $ pushNegations' $ simplifyPrimitive ex
    putStr "CNF         : "; printBool $ toCNF ex

standardDemo :: IO ()
standardDemo = do
    -- pslBE exampleExpr01
    -- pslBE exampleExpr02
    -- pslBE (simplifyPrimitive exampleExpr02)
    demo exampleExpr01
    putStrLn ""

    demo exampleExpr05
    putStrLn ""

    -- And just to show off a tree view
    putStrLn "Expression as tree:"
    drawBool exampleExpr05

main :: IO ()
main = standardDemo

-- TODO: Use criterion for benchmarks.

-- defaultMain [
--     bgroup "fib"
--         [ bench "1"  $ whnf fib 1
--         , bench "5"  $ whnf fib 5
--         , bench "9"  $ whnf fib 9
--         , bench "11" $ whnf fib 11
--         ]
--     ]
