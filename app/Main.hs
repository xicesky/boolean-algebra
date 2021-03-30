
module Main where

import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class
import Criterion.Main

import BooleanAlgebra
import BooleanAlgebra.Examples
import BooleanAlgebra.Support.Minisat
import Gen

{- Notes & interesting reads:
    https://en.wikipedia.org/wiki/Satisfiability_modulo_theories
    https://en.wikipedia.org/wiki/Constraint_satisfaction_problem
    https://curry.pages.ps.informatik.uni-kiel.de/curry-lang.org/
    http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.34.4164&rep=rep1&type=pdf

Inspiration for solver:
    https://www-ps.informatik.uni-kiel.de/~sebf/data/pub/atps09.pdf
    http://hackage.haskell.org/package/cflp
-}

demo :: BooleanExpr String -> IO ()
demo ex = do
    putStr "Original    : "; printBool ex
    putStr "Simplified  : "; printBool $ simplify ex
    -- The intermediate form doesn't really show anything fancy
    --putStr "Intermediate: "; printBool $ aggregateConjDisj' $ simplify ex
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

formatMinisatResult :: forall a. (Show a, Ord a) => SatResult a -> String
formatMinisatResult = \case
    Sat map     -> Map.foldrWithKey showsEntry id map ""
    other       -> show other
    where
        showsEntry :: a -> Bool -> ShowS -> ShowS
        showsEntry name value rest =
            showString "    " . shows name
            . showString " = " . shows value
            . showString "\n" . rest

minisatDemo :: IO ()
minisatDemo = do
    let cnf = toCNF2 (sortList :: BooleanExpr String)
    print $ cnfStats cnf
    runSatT print $ do
        result <- runMinisat "minisat" cnf
    
        -- putStrLn $ show result
        liftIO $ putStrLn $ formatMinisatResult result

main :: IO ()
main =
    -- standardDemo
    minisatDemo

-- TODO: Use criterion for benchmarks.

-- defaultMain [
--     bgroup "fib"
--         [ bench "1"  $ whnf fib 1
--         , bench "5"  $ whnf fib 5
--         , bench "9"  $ whnf fib 9
--         , bench "11" $ whnf fib 11
--         ]
--     ]
