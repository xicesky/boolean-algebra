
module Main where

import Data.Void (Void)
import Control.Monad
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class
import Text.Pretty.Simple (pPrint)

import BooleanAlgebra
import BooleanAlgebra.Examples
import BooleanAlgebra.Support.Minisat
import Gen

-- For showing internal pretty-printer state
import Term.Prettyprinter

{- Notes & interesting reads:
    https://en.wikipedia.org/wiki/Satisfiability_modulo_theories
    https://en.wikipedia.org/wiki/Constraint_satisfaction_problem
    https://curry.pages.ps.informatik.uni-kiel.de/curry-lang.org/
    http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.34.4164&rep=rep1&type=pdf

Inspiration for solver:
    https://www-ps.informatik.uni-kiel.de/~sebf/data/pub/atps09.pdf
    http://hackage.haskell.org/package/cflp
-}

demo :: Bool -> BooleanExpr String -> IO ()
demo doCNF ex = do
    -- FIXME: Use pretty && fill instead of custom spaces
    print $ fromString "Original    :" <+> align (prettyBool ex)
    print $ fromString "Simplified  :" <+> align (prettyBool $ simplify ex)
    -- The intermediate form doesn't really show anything fancy
    --putStr "Intermediate: "; printBool $ aggregateConjDisj' $ simplify ex
    when doCNF $
        print $ fromString "CNF         :" <+> align (prettyBool $ toCNF ex)

standardDemo :: IO ()
standardDemo = do
    -- pslBE exampleExpr01
    -- pslBE exampleExpr02
    -- pslBE (simplifyPrimitive exampleExpr02)
    demo True exampleExpr01
    putStrLn ""

    demo True exampleExpr05
    putStrLn ""

    demo False exampleExpr06
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
    let (meh :: Minisat) = Minisat "minisat"
    runSatT print $ do
        result <- solve meh cnf
    
        -- putStrLn $ show result
        liftIO $ putStrLn $ formatMinisatResult result

bugDemo :: IO ()
bugDemo = do
    let problem :: BooleanExpr String = pidgeonHole' 1
    let cnf :: CNF String = toCNF problem
    result <- solve' (Minisat "minisat") cnf
    print result

prettyPrintDemo :: IO ()
prettyPrintDemo = do
    let problem :: BooleanExpr String = pidgeonHole' 3
    --let obj = problem
    --let obj = simplify problem
    let obj = toCNF problem
    putStr "pretty-s.   : "; pPrint obj
    print $ fromString "PP output   :" <+> pretty obj
    -- Demo of debugging the pretty-printer
    --putStr "PP repr     : "; pPrint (diag' $ pretty obj :: Diag ())

main :: IO ()
main =
    standardDemo
    -- minisatDemo
    -- bugDemo
    -- prettyPrintDemo

-- defaultMain [
--     bgroup "fib"
--         [ bench "1"  $ whnf fib 1
--         , bench "5"  $ whnf fib 5
--         , bench "9"  $ whnf fib 9
--         , bench "11" $ whnf fib 11
--         ]
--     ]
