
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-} 

-- Extensions that we are likely to use
{-# LANGUAGE TypeFamilies #-}

module BooleanAlgebra where
{-

-- i stopped maintaining the list of exports for now
-- this module is getting too large anyway, so we'll refactor it later
-- for now it is easier to export everything, so we can play in ghci

module BooleanAlgebra
    ( BooleanValue
    , BooleanExpr
    , simplePretty
    , simplifyPrimitive

    , bImplies
    , bImpliedBy
    , bEq
    , bXor
    , bNand
    , bNor
    , bXnor

    , tryTo
    , tIdempotence
    , tComplement

    , exampleExpr01
    , exampleExpr02
    , exampleExpr03
    , exampleExpr04

    -- Temporary exports for playing in ghci
    , Fix(..)
    , Algebra
    , Coalgebra
    , cata
    , ana
    , simp

    ) where
-}

{- Inspiration:
    https://tuprints.ulb.tu-darmstadt.de/2759/1/rkibria-dissertation-final-korrigiert1.pdf
    https://bartoszmilewski.com/2017/02/28/f-algebras/
    https://www-ps.informatik.uni-kiel.de/~sebf/projects/sat-solver/Control/Monad/Constraint/Boolean.lhs.html
    Recursion schemes: https://blog.sumtypeofway.com/archive.html

    Packages to use:
        https://hackage.haskell.org/package/compdata
        https://hackage.haskell.org/package/recursion-schemes

    TODO:
        - Get rid of Data.Fix (use compdata instead)
-}

import Data.Functor.Classes (Eq1(..))
import Data.Fix (Fix(..), foldFix, unfoldFix)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM2)

{-----------------------------------------------------------------------------}


-- Fix :: f (Fix f) -> Fix f
-- unFix :: Fix f -> f (Fix f)
-- foldFix :: (f a -> a) -> Fix f -> a
-- unfoldFix :: (a -> f a) -> a -> Fix f

-- We like to use the names "Algebra", "Coalgebra", "cata", "ana"
type Algebra f a = f a -> a 
type Coalgebra f a = a -> f a

cata :: Functor f => Algebra f a -> Fix f -> a 
cata = foldFix

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana = unfoldFix

{-----------------------------------------------------------------------------}
-- Thinking

{-----------------------------------------------------------------------------}

-- Boolean Values
data BooleanValue = BTrue | BFalse
    deriving (Eq, Show)

{-
    Functor for boolean expressions
        vn          Type of variable names
        boolexp     F-Parameter (Recursion via algebra)
-}
data BooleanF vn boolexp
    = BFVal BooleanValue
    | BFVariable vn
    | BFNot boolexp
    | BFAnd boolexp boolexp
    | BFOr boolexp boolexp
    deriving (Eq, Show)

instance Functor (BooleanF vn) where
    fmap _ (BFVal x) = BFVal x
    fmap _ (BFVariable vn) = BFVariable vn
    fmap f (BFNot e) = BFNot (f e)
    fmap f (BFAnd e1 e2) = BFAnd (f e1) (f e2)
    fmap f (BFOr e1 e2) = BFOr (f e1) (f e2)

instance Eq vn => Eq1 (BooleanF vn) where
    liftEq _ (BFVal a) (BFVal b) = a == b
    liftEq _ (BFVariable a) (BFVariable b) = a == b
    liftEq eq (BFNot a) (BFNot b) = a `eq` b
    liftEq eq (BFAnd a1 a2) (BFAnd b1 b2) = (a1 `eq` b1) && (a2 `eq` b2)
    liftEq eq (BFOr a1 a2) (BFOr b1 b2) = (a1 `eq` b1) && (a2 `eq` b2)
    liftEq _ _ _ = False

type BooleanExpr vn = Fix (BooleanF vn)

-- FIXME: Show instance for BooleanExpr

{-----------------------------------------------------------------------------}
-- It's annoying to write "Fix" everywhere, so here are some shortcuts

bTrue :: BooleanExpr vn
bTrue = Fix $ BFVal BTrue

bFalse :: BooleanExpr vn
bFalse = Fix $ BFVal BFalse

bVar :: vn -> BooleanExpr vn
bVar = Fix . BFVariable

bNot :: BooleanExpr vn -> BooleanExpr vn
bNot = Fix . BFNot

bAnd :: BooleanExpr vn -> BooleanExpr vn -> BooleanExpr vn
bAnd e1 e2 = Fix $ BFAnd e1 e2

bOr :: BooleanExpr vn -> BooleanExpr vn -> BooleanExpr vn
bOr e1 e2 = Fix $ BFOr e1 e2

{-----------------------------------------------------------------------------}
-- "Pretty" printer
-- TODO: could be prettier (and use Doc instead of string)
-- Find unicode symbols e.g. here:
--  https://en.wikipedia.org/wiki/List_of_logic_symbols

simplePretty :: Show vn => BooleanExpr vn -> String
simplePretty = cata prettyf where
    prettyf :: Show vn => BooleanF vn String -> String
    prettyf (BFVal BTrue) = "⊤"
    prettyf (BFVal BFalse) = "⊥"
    prettyf (BFVariable v) = show v
    prettyf (BFNot e) = "¬" ++ e
    prettyf (BFAnd e1 e2) = "(" ++ e1 ++ "∧" ++ e2 ++ ")"
    prettyf (BFOr e1 e2) = "(" ++ e1 ++ "∨" ++ e2 ++ ")"

exampleExpr01 :: BooleanExpr String
exampleExpr01 = (bNot (bVar "x" `bAnd` bVar "y")) `bAnd` bVar "z"

{-----------------------------------------------------------------------------}
-- Simplifier

-- ?? simp :: f (f a) -> Maybe (f a)
simp :: BooleanF vn (BooleanF vn a) -> Maybe (BooleanF vn a)
simp (BFNot (BFVal BTrue))      = Just (BFVal BFalse)   -- Def of ¬
simp (BFNot (BFVal BFalse))     = Just (BFVal BTrue)    -- Def of ¬
simp (BFAnd (BFVal BTrue) e)    = Just e                -- Identity of ∧
simp (BFAnd (BFVal BFalse) _)   = Just (BFVal BFalse)   -- Annulment of ∧
simp (BFAnd e (BFVal BTrue))    = Just e                -- Identity of ∧
simp (BFAnd _ (BFVal BFalse))   = Just (BFVal BFalse)   -- Annulment of ∧
simp (BFOr  (BFVal BTrue) _)    = Just (BFVal BTrue)    -- Annulment of ∨
simp (BFOr  (BFVal BFalse) e)   = Just e                -- Identity of ∨
simp (BFOr  _ (BFVal BTrue))    = Just (BFVal BTrue)    -- Annulment of ∨
simp (BFOr  e (BFVal BFalse))   = Just e                -- Identity of ∨
simp _ = Nothing

simplifyPrimitive :: BooleanExpr vn -> BooleanExpr vn
simplifyPrimitive = let
    simp' :: BooleanF vn (BooleanF vn (BooleanExpr vn)) -> BooleanF vn (BooleanExpr vn)
    simp' = (liftM2 fromMaybe) (fmap Fix) simp
    in Fix . (cata simp')

exampleExpr02 :: BooleanExpr String
exampleExpr02 = bNot ((bNot bTrue) `bAnd` (bNot bFalse))

{-----------------------------------------------------------------------------}
-- Instead of extending our syntax further, we will use
-- some helpers for more complex expressions
-- this will sadly not print in a pretty way, though.
-- see the compdata branch for an attempt at a better solution

-- a ⇒ b
bImplies :: BooleanExpr vn -> BooleanExpr vn -> BooleanExpr vn
bImplies a b = bNot a `bOr` b

-- a ⇐ b
bImpliedBy :: BooleanExpr vn -> BooleanExpr vn -> BooleanExpr vn
bImpliedBy a b = a `bOr` bNot b

-- a ⇔ b
bEq :: BooleanExpr vn -> BooleanExpr vn -> BooleanExpr vn
bEq a b = (a `bImplies` b) `bAnd` (b `bImplies` a)

-- a ⊕ b = a ⊻ b
bXor :: BooleanExpr vn -> BooleanExpr vn -> BooleanExpr vn
bXor a b = (a `bOr` b) `bAnd` (bNot a `bOr` bNot b)

-- a ⊼ b
bNand :: BooleanExpr vn -> BooleanExpr vn -> BooleanExpr vn
bNand a b = bNot (a `bAnd` b)

-- a ⊽ b
bNor :: BooleanExpr vn -> BooleanExpr vn -> BooleanExpr vn
bNor a b = bNot (a `bOr` b)

-- a ⊙ b = a ⇔ b
bXnor :: BooleanExpr vn -> BooleanExpr vn -> BooleanExpr vn
bXnor = bEq

{-----------------------------------------------------------------------------}
-- More laws and transformations

-- Most transformations return "Nothing" when they fail
-- Combine with this function to apply a transformation "if possible"
tryTo :: (a -> Maybe a) -> a -> a
tryTo f x = fromMaybe x (f x)

-- Idempotence a ∧ a = a, a ∨ a = a
tIdempotence :: Eq vn => BooleanExpr vn -> Maybe (BooleanExpr vn)
tIdempotence (Fix (BFAnd a b))
    | a == b    = Just a
    | otherwise = Nothing
tIdempotence (Fix (BFOr a b))
    | a == b    = Just a
    | otherwise = Nothing
tIdempotence _  = Nothing

-- Complement a ∧ ¬a = 0, a ∨ a = 1
tComplement :: Eq vn => BooleanExpr vn -> Maybe (BooleanExpr vn)
tComplement (Fix (BFAnd a (Fix (BFNot b))))
    | a == b    = Just bFalse
    | otherwise = Nothing
tComplement (Fix (BFAnd (Fix (BFNot a)) b))
    | a == b    = Just bFalse
    | otherwise = Nothing
tComplement (Fix (BFOr a (Fix (BFNot b))))
    | a == b    = Just bTrue
    | otherwise = Nothing
tComplement (Fix (BFOr (Fix (BFNot a)) b))
    | a == b    = Just bTrue
    | otherwise = Nothing
tComplement _   = Nothing

{-  TODO:
    Associativity & Commutivity (are they needed at all?)

    Double negation:
        ¬(¬a) = a
    Distributive:
        a ∧ (b ∨ c) = (a ∧ b) ∨ (a ∧ c)
        a ∨ (b ∧ c) = (a ∨ b) ∧ (a ∨ c)
    Absorptive:
        a ∧ (a ∨ b) = a
        a ∨ (a ∧ b) = a
    De Morgan's laws:
        ¬(a ∧ b) = (¬a ∨ ¬b)
        ¬(a ∨ b) = (¬a ∧ ¬b)

-}

exampleExpr03 :: BooleanExpr String
exampleExpr03 = bVar "a" `bAnd` bVar "a"

exampleExpr04 :: BooleanExpr String
exampleExpr04 = bVar "a" `bAnd` bNot (bVar "a")

{-----------------------------------------------------------------------------}
-- Basic conversion to CNF
-- doesn't add any variables

-- CNF in 3 steps:
--      Simplify primitives (see above)
--      Push in negations
--      Distribute disjunctions over conjunctions

-- pushNeg :: BooleanExpr v -> BooleanExpr v
-- pushNeg :: BooleanF vn (BooleanF vn a) -> Maybe (BooleanF vn a)
pushNeg :: BooleanExpr v -> BooleanExpr v
pushNeg = ana (unFix . f . fmap unFix . unFix) where
    f :: BooleanF v (BooleanF v (BooleanExpr v)) -> BooleanExpr v
    f (BFNot (BFAnd e1 e2)) = bOr (bNot e1) (bNot e2)
    f (BFNot (BFOr e1 e2)) = bAnd (bNot e1) (bNot e2)
    f (BFNot (BFNot e)) = e
    f other = Fix $ fmap Fix $ other

pushOr :: BooleanExpr v -> BooleanExpr v
pushOr = ana (unFix . f . fmap unFix . unFix) where
    f :: BooleanF v (BooleanF v (BooleanExpr v)) -> BooleanExpr v
    f (BFOr (BFAnd e1 e2) x) = bAnd (bOr e1 (Fix x)) (bOr e2 (Fix x))
    f (BFOr x (BFAnd e1 e2)) = bAnd (bOr (Fix x) e1) (bOr (Fix x) e2)
    f other = Fix $ fmap Fix $ other

-- Idea of how pushOr deals with both hands:
--    (a ∆ b) v (c ∆ d)
--    (a v (c ∆ d)) ∆ (b v (c ∆ d))
--    ((a v c) ∆ (a v d)) ∆ ((b v c) ∆ (b v d))

toCNF :: BooleanExpr v -> BooleanExpr v
toCNF = pushOr . pushNeg . simplifyPrimitive

-- Test it on this, e.g.: pushOr $ pushNeg $ exampleExpr05
exampleExpr05 :: BooleanExpr String
exampleExpr05 = bNot $ ((bNot $ bVar "a") `bOr` bVar "b") `bAnd` (bNot $ bVar "c" `bAnd` bVar "d")

{-----------------------------------------------------------------------------}
-- A CNF datatype

-- Start simple, then generalize using https://wiki.haskell.org/GHC/Type_families
data    Lit v = Pos v | Neg v               deriving (Show, Eq, Functor)
newtype CNF v = CNF { unCNF :: [[Lit v]] }  deriving (Show, Eq, Functor)

negateLit :: Lit v -> Lit v
negateLit (Pos v) = Neg v
negateLit (Neg v) = Pos v

data CNFHelper v
    = HLit (Lit v)
    | HDisj [Lit v]
    | HConj [[Lit v]]
    deriving (Show, Eq)

-- Collect whole subexpressions with common operator in lists
collectCNF :: forall v. BooleanExpr v -> CNF v
collectCNF = convert . cata collect where
    -- -- Correct type would be: ??? Fix (Not :+: Var) v -> Bool -> Lit
    -- collectLiteral :: BooleanExpr v -> Lit v
    -- collectLiteral (Fix (BFNot (Fix (BFVariable vn)))) = Neg vn
    -- collectLiteral (Fix (BFVariable vn)) = Pos vn
    -- collectLiteral e = error $ "cannot collect literal from " ++ show e

    -- -- Correct type: ??? Fix (BOr :+: Lit) v -> [Lit v]
    -- collectOr :: BooleanF v [Lit v] -> [Lit v]
    -- collectOr (BFOr e1 e2) = e1 ++ e2
    -- collectOr _ = error "FIXME"

    -- collectAnd :: BooleanF v [[Lit v]] -> [[Lit v]]
    -- collectAnd (BFAnd e1 e2) = e1 ++ e2
    -- collectAnd _ = error "FIXME"

    upgr :: CNFHelper v -> CNFHelper v
    upgr (HLit l) = HDisj [l]
    upgr (HDisj t) = HConj [t]
    upgr (HConj _) = error "fail"

    -- Compose above functions... how?
    collect :: BooleanF v (CNFHelper v) -> CNFHelper v
    collect (BFVariable vn) = HLit (Pos vn)
    collect (BFNot (HLit l)) = HLit (negateLit l)

    collect (BFOr (HConj _) _) = error $ "disj over conj"
    collect (BFOr _ (HConj _)) = error $ "disj over conj"
    collect (BFOr (l) (HLit r)) = collect (BFOr (l) (HDisj $ [r]))
    collect (BFOr (HLit l) (HDisj rs)) = HDisj $ l : rs
    collect (BFOr (HDisj ls) (HDisj rs)) = HDisj $ ls ++ rs

    collect (BFAnd (HConj ls) (HConj rs)) = HConj $ ls ++ rs
    collect (BFAnd l (HDisj r)) = collect $ BFAnd l $ upgr $ HDisj r
    collect (BFAnd l (HLit r)) = collect $ BFAnd l $ upgr $ HLit r
    collect (BFAnd l r) = collect (BFAnd (upgr l) r)

    collect (BFVal BTrue) = HConj []
    collect (BFVal BFalse) = HDisj []

    convert :: CNFHelper v -> CNF v
    convert (HConj x) = CNF x
    convert x = convert $ upgr x

toCNFData :: BooleanExpr v -> CNF v
toCNFData = collectCNF . toCNF

{-----------------------------------------------------------------------------}
-- Tseitin transformation to CNF
-- Adds new variables to avoid term explosion
-- https://en.wikipedia.org/wiki/Tseytin_transformation

-- TODO

{-----------------------------------------------------------------------------}
-- Fin

-- Further ideas: Maybe do something with
--  https://en.wikipedia.org/wiki/Karnaugh_map
--  https://en.wikipedia.org/wiki/Quine%E2%80%93McCluskey_algorithm
-- (these require a sat solver)
