
{- |
Description     : Solve using Minisat
Stability       : experimental

Solve CNF using minisat.
-}
module BooleanAlgebra.Support.Minisat
    (   -- * Running minisat
        runMinisat
    ,   runMinisat'
    ,   -- * Re-exports
        MinisatResult(..)
    ) where

import System.IO (withFile, IOMode(..))
import System.IO.Temp (withSystemTempDirectory)

import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder)

import Missing.IO as MIO
import Missing.Textual
import BooleanAlgebra.Base.Expression
import BooleanAlgebra.Transform.Variable
import BooleanAlgebra.Format.Dimacs

invokeMinisat :: FilePath -> Builder -> IO (Either String B.ByteString)
invokeMinisat minisatFP input = withSystemTempDirectory "ba" $ \tempdir -> let
    inputFP = tempdir ++ "/input.cnf"
    outputFP = tempdir ++ "/output.minisat"
    in do
        MIO.writeFile inputFP input
        -- putStrLn $ textualToString input
        putStrLn "Invoking Minisat..." -- TODO remove
        (exitCode, stdOut, stdErr) <- readProcessWithExitCodeInt
            minisatFP [ inputFP, outputFP ] ""
        putStr stdOut
        case exitCode of
            -- Minisat uses weird exit codes
            n | n == 10 || n == 20
                -> MIO.readFile outputFP
            code -> return $ Left $ "minisat ExitCode " ++ show code ++ ": " ++ stdErr

-- | Run minisat (or compatible solver) on a CNF problem
runMinisat' :: FilePath -> CNF Int -> IO (MinisatResult Int)
runMinisat' minisat cnf =
    invokeMinisat minisat (unASCIIBuilder $ toDimacs cnf) >>= \case
        Left err -> return $ Error err
        Right output -> return $ parseMinisatOutput output

-- | Run minisat (or compatible solver) on a CNF problem, preserving variable names
runMinisat :: Ord name => FilePath -> CNF name -> IO (MinisatResult name)
runMinisat minisat cnf = let
    Context (_, ntoi) cnfi = buildContext cnf
    in runMinisat' minisat cnfi >>= \case
        Sat map -> return $ Sat $ fmap (map Map.!) ntoi
        Unsat   -> return Unsat
        Error e -> return $ Error e
