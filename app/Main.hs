module Main where

import BooleanAlgebra

{- Notes & interesting reads:
    https://en.wikipedia.org/wiki/Satisfiability_modulo_theories
    https://en.wikipedia.org/wiki/Constraint_satisfaction_problem
-}

pslBE :: Show vn => BooleanExpr vn -> IO ()
pslBE = putStrLn . simplePretty

main :: IO ()
main = do
    pslBE exampleExpr01
    pslBE exampleExpr02
    pslBE (simplifyPrimitive exampleExpr02)
