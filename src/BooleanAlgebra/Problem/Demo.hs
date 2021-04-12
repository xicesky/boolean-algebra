
{- |
Description     : Demos of problem encoding
Stability       : experimental

-}
module BooleanAlgebra.Problem.Demo where

import Prelude hiding (all, and, or, not, (&&), (||))

import Data.Foldable hiding (all, any, and, or)
import Data.List hiding (all, any, and, or)
import Control.Monad
import Text.Pretty.Simple

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import BooleanAlgebra
import BooleanAlgebra.Problem.Encoding
import Gen

-- i'll use my head when i want to
{-# ANN module "HLint: ignore Use head" #-}

{-----------------------------------------------------------------------------}
{-
Interactive:

-- Choosing a single variable (really hard)
testChooseInt01

-- 3x3 Latin square
print $ pretty $ solveAssignment $ lasqProb 3

-- 4x4 sudoku
solveSudoku

-- Testing EncodeM
print $ prettyBool $ snd $ runEncodeM $ lasqProbM 2

-}

encodeMDemo :: EncodeState
encodeMDemo = snd $ runEncodeM $ do
    v0 <- newChoiceVar "" [1..6]
    v1 <- newChoiceVar "named" [1..6]
    -- v2 <- newChoiceVar "named" [1..6]
    return ()

{-----------------------------------------------------------------------------}
-- test

-- chooseInt01 :: forall b. BooleanAlgebra b => b
-- chooseInt01 = existsUnique [1..3] (\x -> var $ "N" ++ show x)

-- Directly using choose, decodeChoose
testChooseInt01 :: Int
testChooseInt01 = let
    problem :: BooleanExpr String
    problem = choose "x" [1..9]
    in decodeChoose "x" [1..9] (simpleSolve problem)

-- Using runEncodeM / withSolution
testChooseInt02 :: Maybe Int
testChooseInt02 = fst $ runEncodeM $ do
    x <- newChoiceVar "x" [1..9]
    withSolution $ do
        getChoiceVal x

-- Using runEncodeM / withSolution, two variables & constraint
testChooseInt03 :: Maybe [Int]
testChooseInt03 = fst $ runEncodeM $ let
    structure = ["a", "b"]
    in do
        vars <- mapM (`newChoiceVar` [1..9]) structure
        (vars !! 0) &/=& (vars !! 1)
        withSolution $ do
            mapM getChoiceVal vars

{-----------------------------------------------------------------------------}
-- Matrix structure problems

type Matrix a = [[a]]

-- rows . rows = id
rows :: Matrix a -> [[a]]
rows = id

-- cols . cols = id
cols :: Matrix a -> [[a]]
cols = transpose

-- Boxes as in Sudoku
-- boxes n . boxes n = id
boxes :: Int -> Matrix a -> [[a]]
boxes n = unpack . map transpose . pack where
    pack :: [[a]] -> [[[[a]]]]
    pack = chop n . map (chop n)
    unpack :: [[[[a]]]] -> [[a]]
    unpack = map concat . concat

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = ys : chop n zs where
    (ys, zs) = splitAt n xs

{-----------------------------------------------------------------------------}
-- Latin square

lasqProb :: Int -> BooleanExpr String
lasqProb n = let

    -- 1. "Giving names" to variables in our structure
    ivar :: Int -> Int -> VarName
    ivar x y = "f(" ++ show x ++ "," ++ show y ++ ")"

    structure :: Matrix VarName
    structure =
        [   [ ivar x y | x <- [1..n] ]  -- single row
        | y <- [1..n]
        ]

    -- 2. Assigning the domain of variables
    vardom = [1..n]

    dom :: BooleanAlgebra b String => b String
    dom =
        all structure $ \row ->
        all row $ \cell ->
        choose cell vardom

    -- 3. Formulating rules
    ruleRows :: BooleanAlgebra b String => b String
    ruleRows =
        all (rows structure) $ \row ->
        allDifferent vardom row

    ruleCols :: BooleanAlgebra b String => b String
    ruleCols =
        all (cols structure) $ \col ->
        allDifferent vardom col

    prob :: BooleanExpr String
    prob = dom && ruleRows && ruleCols

    in prob

decodeLasq :: Int -> Map String Bool -> Matrix Int
decodeLasq n r = let
    -- FIXME duplication

    -- 1. "Giving names" to variables in our structure
    ivar :: Int -> Int -> VarName
    ivar x y = "f(" ++ show x ++ "," ++ show y ++ ")"

    structure :: Matrix VarName
    structure =
        [   [ ivar x y | x <- [1..n] ]  -- single row
        | y <- [1..n]
        ]

    -- 2. Domain of variables
    vardom = [1..n]

    -- decodeChoose :: String -> [Int] -> Map String Bool -> Int
    in fmap (fmap (\v -> decodeChoose v vardom r)) structure

solveLasq :: Int -> IO ()
solveLasq n = let
    problem :: BooleanExpr String
    problem = lasqProb n

    solution :: Matrix Int
    solution = decodeLasq n $ simpleSolve problem

    in print $ pretty solution

{-----------------------------------------------------------------------------}
-- Latin square using encodeM

lasqProbM :: Int -> EncodeM (Matrix (EVar Int))
lasqProbM n = let

    -- 1. "Giving names" to variables in our structure
    ivar :: Int -> Int -> VarName
    ivar x y = "f(" ++ show x ++ "," ++ show y ++ ")"

    structure :: EncodeM (Matrix (EVar Int))
    structure = mapM (mapM (`newChoiceVar` [1..n]))
        [   [ ivar x y | x <- [1..n] ]  -- single row
        | y <- [1..n]
        ]

    -- 2. Formulating rules
    ruleRows :: Matrix (EVar Int) -> EncodeM ()
    ruleRows structure =
        -- TODO Clumsy
        forM_ (rows structure) $ \row ->
        forM row $ \v1 ->
        forM row $ \v2 ->
        when (v1 /= v2) $
        v1 &/=& v2

    ruleCols :: Matrix (EVar Int) -> EncodeM ()
    ruleCols structure =
        -- TODO Clumsy
        forM_ (cols structure) $ \col ->
        forM col $ \v1 ->
        forM col $ \v2 ->
        when (v1 /= v2) $
        v1 &/=& v2

    in do
        s <- structure
        ruleRows s
        ruleCols s
        return s

solveLasqM :: Int -> IO ()
solveLasqM i = let
    solution :: Maybe (Matrix Int)
    solution = fst $ runEncodeM $ do
        m <- lasqProbM i
        withSolution $ do
            mapM (mapM getChoiceVal) m
    in do
        print $ pretty solution

{-----------------------------------------------------------------------------}

solveSudoku :: IO ()
solveSudoku = let
    problem :: BooleanExpr SCell
    problem = sudoku 2 2
    in print $ pretty $ solveAssignment problem
