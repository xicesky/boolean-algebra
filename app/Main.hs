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

pslBE :: Show vn => BooleanExpr vn -> IO ()
pslBE = putStrLn . simplePretty

main :: IO ()
main = do
    pslBE exampleExpr01
    pslBE exampleExpr02
    pslBE (simplifyPrimitive exampleExpr02)
