module BooleanAlgebra
    ( BooleanValue
    , BooleanExpr
    , simplePretty
    , simplifyPrimitive
    , exampleExpr01
    , exampleExpr02
    ) where

{-
{------------------------------------------------------------------------------
    TODO: Use F-Algebra and cata?
-}
import Data.Fix (Fix, foldFix)

type Algebra f a = f a -> a 
newtype Fix f = Fix { unFix :: f (Fix f) } 

cata :: Functor f => Algebra f a -> Fix f -> a 
cata = foldFix

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

------------------------------------------------------------------------------}

data BooleanValue = BTrue | BFalse
    deriving (Eq, Show)

data BooleanExpr vn
    = BEVal BooleanValue
    | BEVariable vn
    | BENot (BooleanExpr vn)
    | BEAnd (BooleanExpr vn) (BooleanExpr vn)
    | BEOr (BooleanExpr vn) (BooleanExpr vn)
    deriving (Eq, Show)

simplePretty :: Show vn => BooleanExpr vn -> String
simplePretty (BEVal BTrue) = "⊤"
simplePretty (BEVal BFalse) = "⊥"
simplePretty (BEVariable v) = show v
simplePretty (BENot e) = "¬" ++ simplePretty e
simplePretty (BEAnd e1 e2) = "(" ++ simplePretty e1 ++ "∧" ++ simplePretty e2 ++ ")"
simplePretty (BEOr e1 e2) = "(" ++ simplePretty e1 ++ "∨" ++ simplePretty e2 ++ ")"

exampleExpr01 :: BooleanExpr String
exampleExpr01 = (BENot (BEVariable "x" `BEAnd` BEVariable "y")) `BEAnd` BEVariable "z"

-- FIXME: This is part of cata actually
atChildren :: (BooleanExpr vn -> BooleanExpr vn) -> BooleanExpr vn -> BooleanExpr vn
atChildren f (BENot e) = BENot (f e)
atChildren f (BEAnd e1 e2) = BEAnd (f e1) (f e2)
atChildren f (BEOr e1 e2) = BEOr (f e1) (f e2)
atChildren _ e = e

-- FIXME: This is actually just cata
everywhere :: (BooleanExpr vn -> BooleanExpr vn) -> BooleanExpr vn -> BooleanExpr vn
everywhere f = f . atChildren (everywhere f)

-- Apply function everywhere as long as it doesn't return Nothing
-- This is kinda monadic
asLongAsPossible :: (BooleanExpr vn -> Maybe (BooleanExpr vn))
    -> BooleanExpr vn -> BooleanExpr vn
asLongAsPossible f = everywhere g where
    g x = maybe x (everywhere g) (f x)

simplifyPrimitive :: BooleanExpr vn -> BooleanExpr vn
simplifyPrimitive = asLongAsPossible simp where
    simp :: BooleanExpr vn -> Maybe (BooleanExpr vn)
    simp (BENot (BEVal BTrue)) = Just (BEVal BFalse)
    simp (BENot (BEVal BFalse)) = Just (BEVal BTrue)
    simp (BEAnd (BEVal BTrue) e) = Just e
    simp (BEAnd (BEVal BFalse) _) = Just (BEVal BFalse)
    simp (BEAnd e (BEVal BTrue)) = Just e
    simp (BEAnd _ (BEVal BFalse)) = Just (BEVal BFalse)
    simp (BEOr (BEVal BTrue) _) = Just (BEVal BTrue)
    simp (BEOr (BEVal BFalse) e) = Just e
    simp (BEOr _ (BEVal BTrue)) = Just (BEVal BTrue)
    simp (BEOr e (BEVal BFalse)) = Just e
    simp _ = Nothing

yes = BEVal BTrue
no = BEVal BFalse

exampleExpr02 :: BooleanExpr String
exampleExpr02 = BENot ((BENot yes) `BEAnd` (BENot no))
