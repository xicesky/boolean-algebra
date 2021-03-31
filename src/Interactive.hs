
{-# LANGUAGE AllowAmbiguousTypes #-}

-- Temporary module for playing around in ghci
module Interactive where

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Monad.IO.Class

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import BooleanAlgebra
import BooleanAlgebra.Examples
import BooleanAlgebra.Format.Dimacs (toDimacs, toDimacsVars, parseMinisatOutput)
import BooleanAlgebra.Support.Minisat
import Gen

import Debug.Trace

-- prop_nameLiterals_inverts :: BooleanExprLit -> Bool
-- prop_nameLiterals_inverts term = let
--     nameMap :: NameMap
--     numberedTerm :: BooleanExprLitI
--     (nameMap, numberedTerm) = numberLiterals term
--     in trace ("prop_nameLiterals_inverts " ++ prettyBool term) $
--     nameLiterals nameMap numberedTerm == term

-- checkArbitrary @CNF
checkArbitrary :: forall a. (Arbitrary a, Show a, Eq a) => IO ()
checkArbitrary = quickCheck (\(x::a) -> x == x)

{-----------------------------------------------------------------------------}
-- Testing solvers against each other

cmpMap :: forall name. (Show name, Ord name) => Map name Bool -> Map name Bool -> IO ()
cmpMap ma mb = let
    left    = Map.difference ma mb
    right   = Map.difference mb ma
    showEntry :: name -> Bool -> IO ()
    showEntry name val = putStrLn $ "    " ++ show name ++ " = " ++ show val
    in do
        putStrLn "Left, but not right"
        Map.foldMapWithKey showEntry left
        putStrLn ""
        putStrLn "Right, but not right"
        Map.foldMapWithKey showEntry right

cmpSatResult :: (Show name, Ord name) => SatResult name -> SatResult name -> IO ()
cmpSatResult (Sat a) (Sat b)
    | a == b                = putStrLn "Ok: Both Sat"
    | otherwise             = putStrLn "Error: Different solutions" >> cmpMap a b
cmpSatResult Unsat Unsat    = putStrLn "Ok: Both Unsat"
cmpSatResult (Sat _) Unsat  = putStrLn "Error: Right Unsat"
cmpSatResult Unsat (Sat _)  = putStrLn "Error: Right Sat"

printError :: SatError String -> IO ()
printError err = do
    putStrLn $ "Sorry, there was an error: " ++ show err

-- Check a solver against minisat
checkSolver :: Solver s IO => s -> IO ()
checkSolver solver = do
    -- Better use a problem with at most one solution!
    let cnf = toCNF (sortList :: BooleanExpr String)
    print $ cnfStats cnf

    runSatT printError $ do
        result <- solve (Minisat "minisat") cnf
        result2 <- solve solver cnf

        liftIO $ do 
            putStrLn ""
            putStrLn $ replicate 80 '-'
            putStrLn ""
            cmpSatResult result result2

        return ()

{- Example usage of pretty-printer

>>> pretty $ toCNF (pidgeonHole' 3 :: BooleanExpr String)
-}
