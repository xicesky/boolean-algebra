
{- |
Description     : Solve using Minisat
Stability       : experimental

Solve CNF using minisat.
-}
module BooleanAlgebra.Support.Minisat
    (   -- * Minisat type
        Minisat(..)
    ) where

import System.IO (withFile, IOMode(..))
import System.IO.Temp (withSystemTempDirectory)

import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder)

import Control.Monad.IO.Class

import Missing.IO as MIO
import Missing.Textual
import BooleanAlgebra.Base.Expression
import BooleanAlgebra.Transform.Variable
import BooleanAlgebra.Format.Dimacs
import BooleanAlgebra.Solver.Class

-- | Run the minisat executable, providing IO via temp files
invokeMinisat :: MonadIO m => FilePath -> Builder -> m (Either String B.ByteString)
invokeMinisat minisatFP input = liftIO $ withSystemTempDirectory "ba" $ \tempdir -> let
    inputFP = tempdir ++ "/input.cnf"
    outputFP = tempdir ++ "/output.minisat"
    in do
        MIO.writeFile inputFP input
        -- putStrLn $ textualToString input
        putStrLn "Invoking Minisat..." -- TODO remove
        -- TODO: Make minisat less verbose
        (exitCode, stdOut, stdErr) <- readProcessWithExitCodeInt
            minisatFP [ inputFP, outputFP ] ""
        putStr stdOut
        case exitCode of
            -- Minisat uses weird exit codes
            n | n == 10 || n == 20
                -> MIO.readFile outputFP
            code -> return $ Left $ "minisat ExitCode " ++ show code ++ ": " ++ stdErr

{-----------------------------------------------------------------------------}
-- Interface

{- | Minisat solver

Minisat is an external solver, thus you need a path to the actual binary.
To run it, use e.g. 'solve' from the 'Solver' interface.
-}
newtype Minisat = Minisat FilePath

instance MonadIO m => Solver Minisat m where
    solveInt :: Minisat -> Int -> CNF Int -> SatT a m (SatResult Int)
    solveInt (Minisat minisatPath) _ cnf =
        invokeMinisat minisatPath (unASCIIBuilder $ toDimacs cnf) >>= \case
            Left err        -> throwError $ ExternalSolverError err
            Right output    -> parseMinisatOutput output

-- -- | Run minisat on a CNF problem
-- runMinisat' :: (MonadError (SatError a) m, MonadIO m) =>
--     FilePath -> CNF Int -> m (SatResult Int)
-- runMinisat' minisat cnf =
--     invokeMinisat minisat (unASCIIBuilder $ toDimacs cnf) >>= \case
--         Left err        -> throwError $ ExternalSolverError err
--         Right output    -> parseMinisatOutput output

-- -- | Run minisat on a CNF problem, preserving variable names
-- runMinisat :: (MonadError (SatError name) m, MonadIO m, Ord name) =>
--     FilePath -> CNF name -> m (SatResult name)
-- runMinisat minisat cnf = let
--     Context (_, ntoi) cnfi = buildContext cnf
--     in runMinisat' minisat cnfi >>= mapResultNames ntoi
