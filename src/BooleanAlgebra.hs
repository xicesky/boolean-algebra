module BooleanAlgebra
    ( BooleanValue
    , BooleanExpr
    , simplePretty
    , simplifyPrimitive
    , exampleExpr01
    , exampleExpr02
    ) where

{- Inspiration:
    https://tuprints.ulb.tu-darmstadt.de/2759/1/rkibria-dissertation-final-korrigiert1.pdf
    https://bartoszmilewski.com/2017/02/28/f-algebras/
    https://www-ps.informatik.uni-kiel.de/~sebf/projects/sat-solver/Control/Monad/Constraint/Boolean.lhs.html
-}

{-----------------------------------------------------------------------------}

import Data.Fix (Fix(..), foldFix, unfoldFix)
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

-- Think about cata this way:
cataBE :: (BooleanF vn a -> a) -> BooleanExpr vn -> a
cataBE = cata

-- Transformation of exprs
transBE :: (BooleanF vn (BooleanExpr vn) -> BooleanExpr vn) -> BooleanExpr vn -> BooleanExpr vn
transBE = cataBE

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

-- How to match on nested stuff
-- e.g. Match ((BFVal BTrue) `BFOr` e)
stepOne :: BooleanExpr vn -> BooleanF vn (BooleanExpr vn)
stepOne = unFix

stepTwo :: BooleanF vn (BooleanExpr vn) -> BooleanF vn (BooleanF vn (BooleanExpr vn))
stepTwo = fmap unFix

simp :: BooleanF vn (BooleanF vn a) -> Maybe (BooleanF vn a)
simp (BFNot (BFVal BTrue)) = Just (BFVal BFalse)
simp (BFNot (BFVal BFalse)) = Just (BFVal BTrue)
simp (BFAnd (BFVal BTrue) e) = Just e
simp (BFAnd (BFVal BFalse) _) = Just (BFVal BFalse)
simp (BFAnd e (BFVal BTrue)) = Just e
simp (BFAnd _ (BFVal BFalse)) = Just (BFVal BFalse)
simp (BFOr (BFVal BTrue) _) = Just (BFVal BTrue)
simp (BFOr (BFVal BFalse) e) = Just e
simp (BFOr _ (BFVal BTrue)) = Just (BFVal BTrue)
simp (BFOr e (BFVal BFalse)) = Just e
simp _ = Nothing

repeatF :: (a -> Maybe a) -> a -> a
repeatF f x = case f x of
    Just y  -> repeatF f y      -- apply again
    Nothing -> x                --- done

-- with a = BooleanExpr vn
repeatFB1 :: (BooleanExpr vn -> Maybe (BooleanExpr vn)) -> BooleanExpr vn -> BooleanExpr vn
repeatFB1 f x = case f x of
    Just y  -> repeatF f y      -- apply again
    Nothing -> x                --- done


simplifyPrimitive :: BooleanExpr vn -> BooleanExpr vn
simplifyPrimitive e = let
    -- cata :: (f a -> a) -> Fix f -> a 
    --  with
    --      f = BooleanF vn
    --      a = BooleanF vn b
    myCata1 :: (BooleanF vn (BooleanF vn b) -> (BooleanF vn b)) -> Fix (BooleanF vn) -> (BooleanF vn b)
    myCata1 = cata
    
    --  with
    --      b = BooleanExpr vn
    myCata2 :: (BooleanF vn (BooleanF vn (BooleanExpr vn))
        -> (BooleanF vn (BooleanExpr vn)))
        -> Fix (BooleanF vn)                -- read: BooleanExpr vn
        -> (BooleanF vn (BooleanExpr vn))
    myCata2 = myCata1

    -- simp :: BooleanF vn (BooleanF vn a) -> Maybe (BooleanF vn a)
    simp2 :: BooleanF vn (BooleanF vn (BooleanExpr vn)) -> BooleanF vn (BooleanExpr vn)
    simp2 e = case simp e of
        Nothing -> fmap Fix e
        -- Repeating on children is not needed here
        --Just e  -> myCata2 simp2 $ Fix e    -- repeat application (including to children)
        Just e  -> simp2 $ fmap unFix e

    simp3 :: Fix (BooleanF vn) -> (BooleanF vn (BooleanExpr vn))
    simp3 = myCata2 simp2

    simp4 :: BooleanExpr vn -> BooleanExpr vn
    simp4 = Fix . simp3

    in simp4 e

exampleExpr02 :: BooleanExpr String
exampleExpr02 = bNot ((bNot bTrue) `bAnd` (bNot bFalse))


{-
-- FIXME: This is actually just fmap on f-alg
atChildren :: (BooleanExpr vn -> BooleanExpr vn) -> BooleanExpr vn -> BooleanExpr vn
atChildren f (BENot e) = BENot (f e)
atChildren f (BEAnd e1 e2) = BEAnd (f e1) (f e2)
atChildren f (BEOr e1 e2) = BEOr (f e1) (f e2)
atChildren _ e = e

-- FIXME: This is actually just cata on f-alg
everywhere :: (BooleanExpr vn -> BooleanExpr vn) -> BooleanExpr vn -> BooleanExpr vn
everywhere f = f . atChildren (everywhere f)

-- Apply function everywhere as long as it doesn't return Nothing
-- This is kinda monadic
asLongAsPossible :: (BooleanExpr vn -> Maybe (BooleanExpr vn))
    -> BooleanExpr vn -> BooleanExpr vn
asLongAsPossible f = everywhere g where
    g x = maybe x (everywhere g) (f x)
-- becomes: g x = maybe x (cata g) (f x)

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
-}

{-
simplifyPrimitive e
=== asLongAsPossible simp e
=== everywhere g e where g x = maybe x (everywhere g) (simp x)
    -- IS: cata g e
=== (g . atChildren (everywhere g)) e where ...     -- simplifies children first
    -- IS 
    -- say (atChildren (everywhere g) e) yields e
=== g e where ...
=== maybe e (everywhere g) (simp e) where ...       -- simplifies e
    -- say simp e = Just y
=== maybe e (everywhere g) (Just y) where ...
=== everywhere g y where ...                        -- repeats, simplifying root+children
-}
