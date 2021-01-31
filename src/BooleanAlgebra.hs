module BooleanAlgebra
    ( BooleanValue
    , BooleanExpr
    , simplePretty
    , simplifyPrimitive
    , exampleExpr01
    , exampleExpr02

    -- Temporary exports for playing in ghci
    , Fix(..)
    , Algebra
    , Coalgebra
    , cata
    , ana
    , simp

    ) where

{- Inspiration:
    https://tuprints.ulb.tu-darmstadt.de/2759/1/rkibria-dissertation-final-korrigiert1.pdf
    https://bartoszmilewski.com/2017/02/28/f-algebras/
    https://www-ps.informatik.uni-kiel.de/~sebf/projects/sat-solver/Control/Monad/Constraint/Boolean.lhs.html
    Recursion schemes: https://blog.sumtypeofway.com/archive.html
-}

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
-- exampleExpr01 = (BENot (BEVariable "x" `BEAnd` BEVariable "y")) `BEAnd` BEVariable "z"

{-----------------------------------------------------------------------------}
-- Simplifier

-- ?? simp :: f (f a) -> Maybe (f a)
simp :: BooleanF vn (BooleanF vn a) -> Maybe (BooleanF vn a)
simp (BFNot (BFVal BTrue))      = Just (BFVal BFalse)
simp (BFNot (BFVal BFalse))     = Just (BFVal BTrue)
simp (BFAnd (BFVal BTrue) e)    = Just e
simp (BFAnd (BFVal BFalse) _)   = Just (BFVal BFalse)
simp (BFAnd e (BFVal BTrue))    = Just e
simp (BFAnd _ (BFVal BFalse))   = Just (BFVal BFalse)
simp (BFOr  (BFVal BTrue) _)    = Just (BFVal BTrue)
simp (BFOr  (BFVal BFalse) e)   = Just e
simp (BFOr  _ (BFVal BTrue))    = Just (BFVal BTrue)
simp (BFOr  e (BFVal BFalse))   = Just e
simp _ = Nothing

simplifyPrimitive :: BooleanExpr vn -> BooleanExpr vn
simplifyPrimitive = let
    simp' :: BooleanF vn (BooleanF vn (BooleanExpr vn)) -> BooleanF vn (BooleanExpr vn)
    simp' = (liftM2 fromMaybe) (fmap Fix) simp
    in Fix . (cata simp')

exampleExpr02 :: BooleanExpr String
exampleExpr02 = bNot ((bNot bTrue) `bAnd` (bNot bFalse))

{-----------------------------------------------------------------------------}

