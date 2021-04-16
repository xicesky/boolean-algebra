
module CNFSizeBench where

import Control.Monad
import Text.Printf (printf)

import BooleanAlgebra
import Gen

problem :: BooleanExpr String
problem = pidgeonHole' 3

type SubjFun = BooleanExpr String -> CNF String

subjects :: [(String, SubjFun)]
subjects =
    [   ("toCNF", toCNF)
    ,   ("toCNF2", toCNF2)
    ]

-- TODO make some utilities for textual?
formatStat :: CNFStats -> [String]
formatStat (CNFStats nClauses minClauseLength maxClauseLength averageClauseLength) =
    [ printf "%-24s%5.f" "Clauses" nClauses
    , printf "%-24s%5.3f / %5.3f / %5.3f" "Length (min/avg/max)"
        minClauseLength averageClauseLength maxClauseLength
    ]

-- | Generate & pretty-print stats
runStat :: (String, SubjFun) -> IO ()
runStat (name, toCNF) = let
    cs :: CNFStats
    cs = cnfStats (toCNF problem)
    in do
        putStrLn $ unlines $ formatStat cs

run :: IO ()
run = forM_ subjects runStat
