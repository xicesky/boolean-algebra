module Main where

import BooleanAlgebra

pslBE :: Show vn => BooleanExpr vn -> IO ()
pslBE = putStrLn . simplePretty

main :: IO ()
main = do
    -- putStrLn (simplePretty exampleExpr01)
    pslBE exampleExpr01
    pslBE exampleExpr02
    pslBE (simplifyPrimitive exampleExpr02)

