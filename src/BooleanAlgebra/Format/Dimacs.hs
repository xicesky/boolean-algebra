
{-# LANGUAGE OverloadedStrings #-}

{- |
Description     : Boolean algebra datatype
Stability       : experimental

Encoding CNF to the DIMACS format.
See https://www.cs.ubc.ca/~hoos/SATLIB/benchm.html
-}
module BooleanAlgebra.Format.Dimacs
    (   -- * Generating DIMACS
        toDimacs
    ,   toDimacsVars

    ,   -- * Parsing results
        parseMinisatOutput
    ) where

import Data.Bool (bool)
import Data.Foldable (foldl')
import Control.Applicative
import Control.Monad

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as B

import Data.Attoparsec.ByteString.Char8

import Missing.Textual
import BooleanAlgebra.Base.Expression
import BooleanAlgebra.Transform.Variable
import BooleanAlgebra.Solver.Class

-- import Debug.Trace (trace)

{- | Represent a comment in DIMACS.

_Precondition_: The string may not contain any control characters
(such as newlines).
-}
dimacsComment :: Textual text => String -> text
dimacsComment c = tString "c " <> tString c <> tNewline

-- | Turn a literal into is DIMACS representation
dimacsLiteral :: Textual text => Literal Int -> text
dimacsLiteral (sign, varnum) = tIntDec $ bool negate id sign varnum

-- | Turn a disjunctive clause into is DIMACS representation
dimacsClause :: Textual text => [Literal Int] -> text
dimacsClause lits = foldl' 
    (\rest lit -> rest <> dimacsLiteral lit <> tChar ' ') mempty lits
    <> tChar '0' <> tNewline

-- | Turn a CNF into its DIMACS representation
toDimacs :: Textual text => CNF Int -> text
toDimacs cnf@(CNF (Conjunction xs))
    = problemLine <> clauses where
    numVars = maximum (variableNames cnf)
    numClauses = length xs
    problemLine = foldText' [ tString "p", tString "cnf", tIntDec numVars, tIntDec numClauses ]
    clauses = foldText (dimacsClause . unDisjunction) mempty xs

{- | Turn a CNF into its DIMACS represention

With comments for variables names
-}
toDimacsVars :: forall text. Textual text => CNF String -> text
toDimacsVars cnf = let
    Context (iton, ntoi) cnfi = buildContext cnf
    showEntry :: Int -> String -> text
    showEntry i name = dimacsComment $ "    " ++ show i ++ " -> " ++ show name
    in  dimacsComment "Variable map:"
    <>  Map.foldMapWithKey showEntry iton
    <>  toDimacs cnfi

-- Minisat output is not in CNF at all

parseLiteral :: Parser (Int, Bool)
parseLiteral = do
    i <- signed decimal
    skipSpace
    return (abs i, i > 0)

parseLiteralList :: Parser (Map Int Bool)
parseLiteralList = skipSpace >>
    Map.fromList <$> manyTill parseLiteral (char '0')

parseMinisatOutput' :: Parser (SatResult Int)
parseMinisatOutput' = skipSpace >> (
                (string "UNSAT" >> return Unsat)
    <|> Sat <$> (string "SAT"   >> parseLiteralList)
    )

-- | Parse output in the simple minisat format
parseMinisatOutput :: MonadError (SatError a) m => B.ByteString -> m (SatResult Int)
parseMinisatOutput text = -- trace ("parseMinisatOutput " ++ show text) $
    case parseOnly parseMinisatOutput' text of
        Left err    -> throwError $ ExternalSolverError err
        Right r     -> return r

{- TODO:
    Parse DIMACS problem file
    Parse actual DIMACS output (e.g. from glucose-simp)
    Output DIMACS solution file
    QDIMACS: http://www.qbflib.org/qdimacs.html

-}
