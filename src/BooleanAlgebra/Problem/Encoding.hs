
module BooleanAlgebra.Problem.Encoding where

import Prelude hiding (all, and, or, not, (&&), (||))

import Data.Maybe (catMaybes)
import Data.Functor.Identity

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import BooleanAlgebra.Base.Class
import BooleanAlgebra.Base.Logic

-- FIXME just for experiments, remove
import BooleanAlgebra
import Gen
import Text.Pretty.Simple
import Data.Foldable hiding (all, any, and, or)
import Data.List hiding (all, any, and, or)

{-----------------------------------------------------------------------------}

encodeVar :: String -> String
encodeVar = show

varEquals :: Show a => String -> a -> String
varEquals v x = "P(" ++ encodeVar v ++ " == " ++ show x ++ ")"

choose :: BooleanAlgebra b String => String -> [Int] -> b String
choose v set = existsUnique set $ \x -> var $ varEquals v x

decodeChoose :: String -> [Int] -> Map String Bool -> Int
decodeChoose v set r = let
    findResult :: Int -> Maybe Int
    findResult x = let
        name = varEquals v x
        in case Map.lookup name r of
            Nothing     -> error $ "Missing result variable: " ++ show name
            Just False  -> Nothing
            Just True   -> Just x
    -- Find a unique true assignment
    in case catMaybes (findResult <$> set) of
        []      -> error $ "No valid assignment for: " ++ show v
        [x]     -> x
        xs      -> error $ "Ambiguous assignments: " ++ show v ++ " = " ++ show xs

{-----------------------------------------------------------------------------}
-- test

-- chooseInt01 :: forall b. BooleanAlgebra b => b
-- chooseInt01 = existsUnique [1..3] (\x -> var $ "N" ++ show x)

testChooseInt01 :: Int
testChooseInt01 = let
    problem :: BooleanExpr String
    problem = choose "x" [1..9]
    in decodeChoose "x" [1..9] (simpleSolve problem)

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

type Var = String

all :: (BooleanArithmetic b, Foldable t) => t a -> (a -> b) -> b
all t f = foldl' (\l r -> l `and` f r) true t

assign :: BooleanAlgebra b String => Var -> Int -> b String
assign v x = var $ v ++ "=" ++ show x

neq :: BooleanAlgebra b String => [Int] -> Var -> Var -> b String
neq dom v1 v2 =     -- Check domains are actually equal!
    forAll dom $ \x ->
    assign v1 x `excludes` assign v2 x

-- Basically the same as unique
allDifferent :: (BooleanAlgebra b String, Foldable t) => [Int] -> t Var -> b String
allDifferent dom t =
    all t $ \v1 ->
    all t $ \v2 ->  -- TODO: Only need ordered pairs
    given (v1 /= v2) $
    neq dom v1 v2

-- Latin square
lasqProb :: Int -> BooleanExpr String
lasqProb n = let

    -- 1. "Giving names" to variables in our structure    
    ivar :: Int -> Int -> Var
    ivar x y = "f(" ++ show x ++ "," ++ show y ++ ")"
    
    structure :: Matrix Var
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
        existsUnique vardom $ \v ->
        assign cell v

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

latinSquare :: Int -> IO () -- [[Int]]
latinSquare n = let
    problem :: BooleanExpr String
    problem = let
        numbers = [1..n]
        rows = [1..n]
        cols = [1..n]

        -- Enc. Property: Field x, y has number n
        p :: BooleanAlgebra b String => Int -> Int -> Int -> b String
        p x y n = var $ "P(" ++ show x ++ "," ++ show y ++ ")=" ++ show n

        in  foldr1 and
            [   -- Unique1: Cell -> Number
                forAll cols $ \x ->
                forAll rows $ \y ->
                existsUnique numbers $ \n ->
                p x y n
            ,   -- Unique1: Number -> Pos in Row
                forAll numbers $ \n ->
                forAll rows $ \y ->
                existsUnique cols $ \x ->
                p x y n
            ,   -- Unique1: Number -> Pos in Col
                forAll numbers $ \n ->
                forAll cols $ \x ->
                existsUnique rows $ \y ->
                p x y n
            ]
    in print $ pretty $ solveAssignment problem

solveSudoku :: IO ()
solveSudoku = let
    problem :: BooleanExpr SCell
    problem = sudoku 2 2
    in print $ pretty $ solveAssignment problem

{-----------------------------------------------------------------------------}

-- | Solve or throw an error
simpleSolve :: forall a. (Show a, Ord a) => BooleanExpr a -> Map a Bool
simpleSolve problem = let
    solution :: Either (SatError a) (SatResult a)
    solution = runIdentity $ runSatT (return . Left) $
        Right <$> solve BasicSolver (toCNF problem)
    in case solution of
        Left err            -> error (show err)
        Right Unsat         -> error "Unsat"
        Right (Sat result)  -> result

-- | Solve assignment problem
solveAssignment :: (Show a, Ord a) => BooleanExpr a -> [a]
solveAssignment problem = fmap fst $ filter snd $ Map.toList $ simpleSolve problem

{-
Interactive:

-- Choosing a single variable (really hard)
testChooseInt01

-- 3x3 Latin square
print $ pretty $ solveAssignment $ lasqProb 3

-- 4x4 sudoku
solveSudoku

-}
