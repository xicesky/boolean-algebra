
module BooleanAlgebra.Ideas where

import Data.Comp.Term
import Data.Comp.Ops
import Data.Comp.Render

import BooleanAlgebra.Util.THUtil
import BooleanAlgebra.Base.Expression
import BooleanAlgebra.Base.Pretty

{-----------------------------------------------------------------------------}
-- Extended boolean functions
-- You can think of these as "Syntactic sugar" for longer expressions
-- that just use ⊤,⊥,¬,∧,∨

data ExtOp
    = BImplies
    | BImpliedBy
    | BEq
    | BXor
    | BNand
    | BNor
    | BXnor
    deriving (Show, Eq, Ord)

data BooleanExtOp e = BooleanExtOp ExtOp e e
    deriving (Functor)

$(deriveDefault [''BooleanExtOp] )

type EBooleanExpr = Term (BooleanBaseF :+: BooleanExtOp)

-- Shorthands
iBXor :: (BooleanExtOp :<: f) => Cxt h f a -> Cxt h f a -> Cxt h f a
iBXor = iBooleanExtOp BXor

-- Pretty-printing

bExtOpSymbol :: ExtOp -> String
bExtOpSymbol BImplies = "⇒"
bExtOpSymbol BImpliedBy = "⇐"
bExtOpSymbol BEq = "⇔"
bExtOpSymbol BXor = "⊻"
bExtOpSymbol BNand = "⊼"
bExtOpSymbol BNor = "⊽"
bExtOpSymbol BXnor = "⊙"

instance PrettyBool BooleanExtOp where
    prettyPrintBoolAlg :: Monad m => BooleanExtOp (Int -> ShowS) -> PrettyM m (Int -> ShowS)
    prettyPrintBoolAlg (BooleanExtOp op a b) = return $ \d -> showParen (d > prec) $
        a (prec+1) . showString (bExtOpSymbol op) . b (prec+1)
        where prec = 9

instance Render BooleanExtOp

-- TODO: desugar

exampleExpr06 :: EBooleanExpr
exampleExpr06 = iTrue `iBXor` (iFalse `iBAnd` iTrue)

-- pslBE $ reduceEB exampleExpr06
-- ((⊤∨(⊥∧⊤))∧(¬⊤∨¬(⊥∧⊤)))

{-----------------------------------------------------------------------------}
-- Tseitin transformation to CNF
-- Adds new variables to avoid term explosion
-- https://en.wikipedia.org/wiki/Tseytin_transformation

-- Idea: Can we express this via "let z = ... in ..." expressions?
-- Idea: Handle extended operations
-- Idea: Preserve annotations which trace back to extended ops

{-----------------------------------------------------------------------------}
-- SAT Solver
-- solves boolean expressions in CNF

{- 
Idea: Preserve annotations, so we can transform them back to
arbitrary expressions for general constraint solving
-}

{-----------------------------------------------------------------------------}
-- Idea: Parser accepting usual variants (e.g. & or && for ∧ )

{-----------------------------------------------------------------------------}
-- Idea: Simplifier using basic transformations
--  Might want to use Data.Comp.TermRewriting

-- exampleExpr0X :: BooleanExpr String
-- exampleExpr0X = bVar "a" `bAnd` bVar "a"

-- exampleExpr0X :: BooleanExpr String
-- exampleExpr0X = bVar "a" `bAnd` bNot (bVar "a")

{-----------------------------------------------------------------------------}
-- Really advanced idea:
-- Extend boolean algebra with propositions over formulas
--  P("a = b")
-- Formulas need some internal logic to decompose them...
-- Then we can prove some theorems!
