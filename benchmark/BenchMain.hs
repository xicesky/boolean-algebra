
{-
Single executable for multiple benchmark variants

To run the cnf performance benchmark:
    stack bench boolean-algebra:bench:boolean-algebra-benchmarks \
        --benchmark-arguments 'cnfperf --output=$benchmark.html' \
        && open boolean-algebra-benchmarks.html

To run the cnf size comparison:
    stack bench boolean-algebra:bench:boolean-algebra-benchmarks \
        --benchmark-arguments 'cnfsize'

-}
module Main where

import Options.Applicative
import Criterion (Benchmark)
import Criterion.Main (runMode)
import Criterion.Main.Options
    ( Mode(..), MatchType(..)
    , parseWith, defaultConfig
    , describeWith
    --, regressionHelp, versionInfo
    )

import qualified CNFPerfBench
import qualified CNFSizeBench

data Cmd
    = CNFPerf Mode
    | CNFSize

{- TODO:
- cnfstats has no description...
    because Criterion's describeWith function doesn't set progDesc.
    sadly we can't even reimplement it, because the module doesn't export the required
    'regressionHelp' function.
-}
cmdParse :: Parser Cmd
cmdParse = subparser
    (   command "cnfperf"
        ( describeWith (CNFPerf <$> parseWith defaultConfig) )
    <>  command "cnfsize"
        ( info (pure CNFSize) (progDesc "CNF size comparsion benchmark"))
    )-- <|> pure (CNFPerf (Run defaultConfig Prefix []))

cmdInfo :: ParserInfo Cmd
cmdInfo = info (cmdParse <**> helper)
    (   header "boolean-algebra-bechnmarks - Benchmark suite"
    <>  progDesc
        (   "Run a specific benchmark type (see available commands)."
        <>  " Use the --help option on a specific command for details."
        )
    <>  fullDesc
    )

{-
NOTE: There seems to be no way to simply "run" a benchmark executable using stack, without
using the "bench" target. To test the argument parser, try something like this:
stack bench --no-run-benchmarks \
    && .stack-work/dist/*/*/build/boolean-algebra-benchmarks/boolean-algebra-benchmarks --help
-}
main :: IO ()
main = execParser cmdInfo >>= \case
    CNFPerf mode    -> runMode mode CNFPerfBench.benchmarks
    CNFSize         -> CNFSizeBench.run
