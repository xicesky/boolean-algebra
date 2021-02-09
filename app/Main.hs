module Main where

import BooleanAlgebra

{- Notes & interesting reads:
    https://en.wikipedia.org/wiki/Satisfiability_modulo_theories
    https://en.wikipedia.org/wiki/Constraint_satisfaction_problem
    https://curry.pages.ps.informatik.uni-kiel.de/curry-lang.org/

Inspiration for solver:
    https://www-ps.informatik.uni-kiel.de/~sebf/data/pub/atps09.pdf
    http://hackage.haskell.org/package/cflp
-}

pslBE :: PrettyAlmostBool a => a -> IO ()
pslBE = putStrLn . prettyBool

demo :: BooleanExpr -> IO ()
demo ex = do
    putStr "Original    : "; pslBE ex
    putStr "Simplified  : "; pslBE $ simplifyPrimitive ex
    putStr "Intermediate: "; pslBE $ aggregateConjDisj' $ pushNegations' $ simplifyPrimitive ex
    --putStr "CNF         : "; pslBE $ toCNF ex
    --putStr "CNF-Data    : "; putStrLn $ show $ toCNFData ex

main :: IO ()
main = do
    -- pslBE exampleExpr01
    -- pslBE exampleExpr02
    -- pslBE (simplifyPrimitive exampleExpr02)
    demo exampleExpr01
    putStrLn ""

    demo exampleExpr05
    putStrLn ""
