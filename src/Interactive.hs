
{-# LANGUAGE AllowAmbiguousTypes #-}

-- Temporary module for playing around in ghci
module Interactive where

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import BooleanAlgebra
import BooleanAlgebra.Examples
import BooleanAlgebra.Format.Dimacs (toDimacs, toDimacsVars, parseMinisatOutput)
import qualified BooleanAlgebra.Support.Minisat as MS
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

-- FIXME: Eliminate MinisatResult
-- FIXME: Sat vs SAT
toSatResult :: MS.MinisatResult name -> IO (SATResult name)
toSatResult = \case
    MS.Error e      -> fail e
    MS.Unsat        -> return Unsat
    MS.Sat r        -> return $ Sat r

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

cmpSatResult :: (Show name, Ord name) => SATResult name -> SATResult name -> IO ()
cmpSatResult (Sat a) (Sat b)
    | a == b                = putStrLn "Ok: Both Sat"
    | otherwise             = putStrLn "Error: Different solutions" >> cmpMap a b
cmpSatResult Unsat Unsat    = putStrLn "Ok: Both Unsat"
cmpSatResult (Sat _) Unsat  = putStrLn "Error: Right Unsat"
cmpSatResult Unsat (Sat _)  = putStrLn "Error: Right Sat"

-- Check a solver against minisat
checkSolver :: (CNF String -> SATResult String) -> IO ()
checkSolver solver = do
    -- Better use a problem with at most one solution!
    let cnf = toCNF (sortList :: BooleanExpr String)
    print $ cnfStats cnf

    result <- MS.runMinisat "minisat" cnf >>= toSatResult
    let result2 = solver cnf

    putStrLn ""
    putStrLn $ replicate 80 '-'
    putStrLn ""
    cmpSatResult result result2

    return ()
