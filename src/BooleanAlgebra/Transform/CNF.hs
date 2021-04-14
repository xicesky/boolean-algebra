
{- |
Description     : Conjunctive normal form
Stability       : experimental

Transformations to CNF (conjunctive normal form), e.g. for
using a SAT-solver.

-}
module BooleanAlgebra.Transform.CNF
    (   -- * Transformation to CNF
        toCNF, toCNF2

    ,   -- * Stats for CNF
        CNFStats(..), cnfStats

    ,   -- * Explicit CNF transforms
        distributeToCNF
    ,   tseitinTransformM
    ,   tseitinTransform'

    ) where

import Prelude hiding (and, or, not, (&&), (||))

import Data.Kind (Type)
import Data.Void
import Data.Functor.Identity

-- recursion-schemes
import Data.Functor.Foldable

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Missing.Misc (Alg)
import Term.Term
import Term.Inject

import BooleanAlgebra.Base.Class
import BooleanAlgebra.Base.Expression
import BooleanAlgebra.Transform.Simplify
import BooleanAlgebra.Transform.Variable

{-# ANN module "HLint: ignore Use camelCase" #-}

{-----------------------------------------------------------------------------}
-- Stats of CNF clauses

data CNFStats = CNFStats
    {   nClauses :: Int
    ,   minClauseLength :: Int
    ,   maxClauseLength :: Int
    ,   averageClauseLength :: Float
    }
    deriving (Show, Eq)

-- | Collect some statistics of a CNF representation.
cnfStats :: CNF a -> CNFStats
cnfStats (CNF (Conjunction [])) = CNFStats  -- Empty CNF
    {   nClauses = 0
    ,   minClauseLength = 0
    ,   maxClauseLength = 0
    ,   averageClauseLength = 0
    }
cnfStats (CNF (Conjunction xs)) = CNFStats
    {   nClauses = length xs
    ,   minClauseLength = minimum clens
    ,   maxClauseLength = maximum clens
    ,   averageClauseLength = average clens
    } where
    clens :: [Int]
    clens = fmap length xs
    average :: (Real a, Fractional b) => [a] -> b
    average xs = realToFrac (sum xs) / realToFrac (length xs)

{-----------------------------------------------------------------------------}
-- Transform to CNF by distribution

-- Distributes disjunctions over conjunctions:
--      a ∨ (b ∧ c) = (a ∨ b) ∧ (a ∨ c)

distributeToCNF :: forall a. TermLit BFlOps Void a -> CNF a
distributeToCNF = cata distr . unTermLit where
    distr :: Alg (TermF BFlOps Void (Literal a)) (CNF a)
    distr (ConstT x) = absurd x
    distr (VariableT x) = CNF $ (pure . pure) x
    distr (RecT (UnaryOp op _)) = absurd op
    distr (RecT (BinaryOp op _ _)) = absurd op
    distr (RecT (FlatOp op xs)) = case op of
        BConjunction -> CNF $ joinConjunction $ Conjunction $ unCNF <$> xs
        BDisjunction -> CNF $ fmap joinDisjunction . distributeDisjunction $ Disjunction $ unCNF <$> xs


{- | Basic conversion to CNF

Doesn't add any variables, but the number of clauses may explode!
-}
toCNF :: (t a :<: Term BOps Bool a) => t a -> CNF a
toCNF = distributeToCNF . simplify

{-----------------------------------------------------------------------------}
-- Tseitin-Transformation

-- Idea: Handle extended operations
-- Idea: Preserve annotations which trace back to extended ops

cnf_just :: Literal a -> CNF a
cnf_just a = CNF $ pure $ pure a

cnf_iff :: Literal a -> Literal a -> CNF a
cnf_iff a b = CNF $ Conjunction             -- a ⇔ b ≡
    [   Disjunction [ not a, b ]            -- a ⇒ b ∧
    ,   Disjunction [ a, not b ]            -- a ⇐ b
    ]

cnf_iff_and :: Literal a -> Literal a -> Literal a -> CNF a
cnf_iff_and z a b = CNF $ Conjunction
    [   Disjunction [ not z, a ]            -- z ⇒ a
    ,   Disjunction [ not z, b ]            -- z ⇒ b
    ,   Disjunction [ z, not a, not b ]     -- ¬z ⇒ ¬a ∨ ¬b
    ]

cnf_iff_or :: Literal a -> Literal a -> Literal a -> CNF a
cnf_iff_or z a b = CNF $ Conjunction
    [   Disjunction [ not z, a, b ]         -- z ⇒ a ∨ b
    ,   Disjunction [ z, not a ]            -- ¬z ⇒ ¬a
    ,   Disjunction [ z, not b ]            -- ¬z ⇒ ¬b
    ]

cnf_all :: [CNF a] -> CNF a
cnf_all = CNF . joinConjunction . Conjunction . fmap unCNF

tseitinTransformM :: forall m. Monad m => Term BOps Void Int -> GenNameT String m (CNF Int)
tseitinTransformM term = do
    (zTerm, cnf) <- cata tt term
    return $ cnf_all [cnf, cnf_just zTerm]
    where
    tt :: Alg (TermF BOps Void Int) (GenNameT String m (Literal Int, CNF Int))
    tt t = sequenceA t >>= \case
        ConstT v    -> absurd v
        VariableT v -> return ((True, v), CNF $ Conjunction [])
        RecT (UnaryOp BooleanNot (v, cnf)) -> return (not v, cnf)
        RecT (BinaryOp BooleanAnd (va, cnfa) (vb, cnfb)) ->
            newNamedWithPrefix "zA_" >>= \z -> let
            t' = cnf_all
                [   cnfa
                ,   cnfb
                ,   cnf_iff_and (True, z) va vb
                ]
            in return ((True, z), t')
        RecT (BinaryOp BooleanOr (va, cnfa) (vb, cnfb)) ->
            newNamedWithPrefix "zO_" >>= \z -> let
            t' = cnf_all
                [   cnfa
                ,   cnfb
                ,   cnf_iff_or (True, z) va vb
                ]
            in return ((True, z), t')
        RecT (FlatOp op _) -> absurd op

-- tseitinTransform' :: Term BOps Bool a -> Either Bool (Term BOps Void a)
tseitinTransform' :: Term BOps Void String -> CNF String
tseitinTransform' = liftNames 1 tseitinTransformM

{- | Transformation to CNF

This uses the tseitin transformation and will thus create new variables.
The result is solveable /exactly if/ the original expression is solvable.

The number of clauses generated is linear in the size of the original
expression.
-}
toCNF2 :: Term BOps Bool String -> CNF String
toCNF2 term = case constantFold term of
    Left True   -> CNF $ Conjunction []
    Left False  -> CNF $ Conjunction [ Disjunction [] ]
    Right term' -> tseitinTransform' term'
