
-- Playing around with nondeterministic evaluation
-- from the paper https://www-ps.informatik.uni-kiel.de/~sebf/data/pub/atps09.pdf

module Nondeterminism where

import Prelude hiding (Bounded)
import Control.Applicative
import Control.Monad

-- 1. Introduction

{- If "m a" represents nondeterministic computation of a,
    then anyof represents nondeterministic choice from a list.
-}
anyof :: MonadPlus m => [a] -> m a
anyof [] = mzero
anyof (x : xs) = anyof xs <|> return x

-- 2. Monadic Backtracking
-- Lists as nondeterministic evaluation

demoLists :: [Int]
demoLists = let
    input :: [Int]
    input = [1..10]
    in anyof input

-- 2.1. Difference Lists

newtype DiffList a = DiffList { dlConcatL :: [a] -> [a] }

--  dlConcatL :: DiffList a -> [a] -> [a]
--  a `dlConcatL` b === (dlToList a) ++ b

dlEmpty :: DiffList a
dlEmpty = DiffList id

dlSingleton :: a -> DiffList a
dlSingleton x = DiffList (x:)

dlConcat :: DiffList a -> DiffList a -> DiffList a
dlConcat (DiffList fl) (DiffList fr) = DiffList $ fl . fr

dlToList :: DiffList a -> [a]
dlToList (DiffList f) = f []

-- Inefficient!! (++) takes O(n)
listToDl :: [a] -> DiffList a
listToDl xs = DiffList (xs ++)

-- Added by MD
instance Show a => Show (DiffList a) where
    show dl = "listToDl " ++ show (dlToList dl)

-- Added by MD
instance Functor DiffList where
    fmap :: (a -> b) -> DiffList a -> DiffList b
    fmap f a = listToDl $ fmap f $ dlToList a   -- too slow

instance Applicative DiffList where
    pure :: a -> DiffList a
    pure = dlSingleton                          -- at least we can use this?

    (<*>) :: DiffList (a -> b) -> DiffList a -> DiffList b
    (<*>) fs as = listToDl $ (dlToList fs) <*> (dlToList as)    -- too slow

instance Alternative DiffList where
    empty :: DiffList a
    empty = dlEmpty

    (<|>) :: DiffList a -> DiffList a -> DiffList a
    (<|>) = dlConcat

-- Example
exampleDL = dlSingleton 5 `dlConcat` dlSingleton 6 `dlConcat` dlSingleton 8

-- 2.2. CPS Computations

newtype CPS c a = CPS { unCPS :: forall b. (a -> c b) -> c b }

-- CPS :: (forall b. (a -> c b) -> c b) -> CPS c a
-- unCPS :: CPS c a -> (a -> c b) -> c b

-- MD: Could this just be "Applicative"? yield looks like "pure"
-- class Computation c where
--     yield :: a -> c a

-- runCPS :: Computation c => CPS c a -> c a
-- runCPS a = a `unCPS` yield

runCPS :: Applicative c => CPS c a -> c a
runCPS a = a `unCPS` pure

cpsReturn :: a -> CPS c a
cpsReturn x = CPS $ \cont -> cont x

cpsBind :: CPS c a -> (a -> CPS c b) -> CPS c b
cpsBind a f = CPS $ \cont -> a `unCPS` (\x -> f x `unCPS` cont)
--cpsBind a f = CPS $ \cont -> unCPS a (\a -> unCPS (f a) cont)

instance Functor (CPS c) where
    fmap :: (a -> b) -> CPS c a -> CPS c b
    fmap f a = a `cpsBind` \a -> cpsReturn (f a)

instance Applicative (CPS c) where
    pure :: a -> CPS c a
    pure = cpsReturn

    (<*>) :: CPS c (a -> b) -> CPS c a -> CPS c b
    (<*>) cf ca =
        cpsBind cf $ \f -> 
        cpsBind ca $ cpsReturn . f

instance Monad (CPS c) where
    -- return = pure
    (>>=) :: CPS c a -> (a -> CPS c b) -> CPS c b
    (>>=) = cpsBind

-- This is just "Alternative"
-- class Nondet n where
--     failure :: n a
--     choice :: n a -> n a -> n a

-- failure :: Alternative n => n a
-- failure = empty

-- choice :: Alternative n => n a -> n a -> n a
-- choice = (<|>)

instance Alternative n => Alternative (CPS n) where
    empty :: CPS n a
    empty = CPS $ \cont ->
        empty

    (<|>) :: CPS n a -> CPS n a -> CPS n a
    (<|>) a b = CPS $ \cont ->
        (<|>) (a `unCPS` cont) (b `unCPS` cont)

instance Alternative n => MonadPlus (CPS n)

-- 2.3. Efficient backtracking

-- instance Computation DiffList where
--     yield = pure

-- "instance Nondet DiffList" is not needed, DiffList is Alternative, see above

backtrack :: CPS DiffList a -> [a]
backtrack = dlToList . runCPS

-- 3. Different search strategies
-- 3.1. Breadth-first search

newtype Levels n a = Levels { levels :: [n a] }

runLevels :: Alternative n => Levels n a -> n a
runLevels = foldr (<|>) empty . levels
-- runLevels = foldr choice failure . levels

levelsReturn :: Applicative n => a -> Levels n a
levelsReturn x = Levels $ [pure x]

levelsEmpty :: Levels n a
levelsEmpty = Levels $ []

levelsChoice :: Alternative n => Levels n a -> Levels n a -> Levels n a
levelsChoice a b = Levels $ empty : merge (levels a) (levels b)

merge :: Alternative n => [n a] -> [n a] -> [n a]
merge []     ys     = ys
merge xs     []     = xs
merge (x:xs) (y:ys) = (x <|> y) : merge xs ys

instance Functor n => Functor (Levels n) where
    fmap :: (a -> b) -> Levels n a -> Levels n b
    fmap f (Levels as) = Levels $ fmap (fmap f) as

instance Applicative n => Applicative (Levels n) where
    pure :: a -> Levels n a
    pure = levelsReturn

    (<*>) :: Levels n (a -> b) -> Levels n a -> Levels n b
    (<*>) f a = error "NYI"     -- FIXME

instance Alternative n => Alternative (Levels n) where
    empty :: Levels n a
    empty = levelsEmpty

    (<|>) :: Levels n a -> Levels n a -> Levels n a
    (<|>) = levelsChoice

levelSearch :: CPS (Levels DiffList) a -> [a]
levelSearch = dlToList . runLevels . runCPS

-- MD: This runs without DList as well...
inefficientLevelSearch :: CPS (Levels []) a -> [a]
inefficientLevelSearch = runLevels . runCPS

-- 3.2. Iterative deepening depth-first search

newtype Bounded n a = Bounded { boundedBy :: Int -> n a }

-- MD: Is this right!?!?!? FIXME
boundedReturn :: Applicative n => a -> Bounded n a
boundedReturn a = Bounded $ \_ -> pure a

boundedEmpty :: Alternative n => Bounded n a
boundedEmpty = Bounded $ \_ -> empty

boundedChoice :: Alternative n => Bounded n a -> Bounded n a -> Bounded n a
boundedChoice a b = Bounded $ \x ->
    if x == 0 then empty
    else a `boundedBy` (x-1) <|> b `boundedBy` (x-1)

instance Functor n => Functor (Bounded n) where
    fmap :: (a -> b) -> Bounded n a -> Bounded n b
    fmap f (Bounded as) = Bounded $ fmap (fmap f) as

instance Applicative n => Applicative (Bounded n) where
    pure :: a -> Bounded n a
    pure = boundedReturn

    (<*>) :: Bounded n (a -> b) -> Bounded n a -> Bounded n b
    (<*>) f a = error "NYI"     -- FIXME: I don't know how this would have to work, apply by levels?

instance Alternative n => Alternative (Bounded n) where
    empty :: Bounded n a
    empty = boundedEmpty

    (<|>) :: Bounded n a -> Bounded n a -> Bounded n a
    (<|>) = boundedChoice

-- MD: Idea: seperate out a function to search depth [a,b[

levelIter :: forall n a. Alternative n => Int -> CPS (Bounded n) a -> Levels n a
levelIter step a = Levels $ [(a `unCPS` yieldB) `boundedBy` d | d <- [0,step..]]
    where
        yieldB :: a -> Bounded n a
        yieldB x = Bounded $ \d -> if d < step then pure x else empty

iterDepth :: Alternative n => Int -> CPS (Bounded n) a -> n a
iterDepth step = runLevels . levelIter step

iterDepthSearch :: Int -> CPS (Bounded DiffList) a -> [a]
iterDepthSearch step = dlToList . iterDepth step

-- MD: "Maybe" just looks for one solution :)
singleSearch :: CPS (Levels Maybe) a -> Maybe a
singleSearch = runLevels . runCPS

pytriple :: MonadPlus m => m (Int, Int, Int)
pytriple = do
    a <- anyof [1..]
    b <- anyof [a+1..]
    c <- anyof [b+1..]
    guard $ a * a + b * b == c * c
    return (a, b, c)

-- pytriple2 :: [(Int, Int, Int)]
-- pytriple2 = do
--     a <- [1..10]
--     b <- [1..10]
--     c <- [1..10]
--     guard $ a * a + b * b == c * c
--     return (a, b, c)

{-----------------------------------------------------------------------------}

-- Exploring CPS

data MyCPS r a = MyCPS { unMyCPS :: (a -> r) -> r }

-- MyCPS :: (forall r. (a -> r) -> r) -> MyCPS a
-- unMyCPS :: MyCPS a -> (a -> r) -> r

toMyCPS :: a -> MyCPS r a
toMyCPS x = MyCPS $ \cont -> cont x

fromMyCPS :: MyCPS a a -> a
fromMyCPS (MyCPS f) = f id      -- "continue with id"

myCPSBind :: MyCPS r a -> (a -> MyCPS r b) -> MyCPS r b
myCPSBind (MyCPS ca) acb = MyCPS $ \cont -> let
    -- ca :: (a -> r) -> r
    -- acb :: a -> MyCPS r b
    -- "acb :: a -> (b -> r) -> r"
    -- cont :: (b -> r)
    in ca $ \a ->
        unMyCPS (acb a) cont -- :: r

{- From https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style
chainCPS :: forall a b r. ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
chainCPS s f = \k -> let
    -- s :: (a -> r) -> r
    -- f :: a -> ((b -> r) -> r)
    -- k :: b -> r
    fff :: a -> r
    fff x = f x k
    in s fff
-- chainCPS s f = \k -> s ( \x -> f x k )
-- chainCPS s f = s f  -- Occurs check: cannot construct the infinite type: r ~ (b -> r) -> r
-}

instance Functor (MyCPS r) where
    fmap :: (a -> b) -> MyCPS r a -> MyCPS r b
    fmap f (MyCPS ca) = MyCPS $ \cont -> let
        -- f :: a -> b
        -- ca :: (a -> r) -> r
        -- cont :: (b -> r) -> r
        in ca $ cont . f

instance Applicative (MyCPS r) where
    pure :: a -> MyCPS r a
    pure = toMyCPS

    (<*>) :: MyCPS r (a -> b) -> MyCPS r a -> MyCPS r b
    -- Long form
    -- (<*>) cf ca =
    --     myCPSBind cf $ \f -> 
    --     myCPSBind ca $ \a ->
    --     toMyCPS (f a)
    (<*>) (MyCPS cf) (MyCPS ca) = MyCPS $ \cont -> let
        -- cf :: ((a -> b) -> r) -> r
        -- ca :: (a -> r) -> r
        -- cont :: b -> r
        in  cf $ \f ->  -- f :: a -> b
            ca $ cont . f

instance Monad (MyCPS r) where
    -- return = pure
    (>>=) :: MyCPS r a -> (a -> MyCPS r b) -> MyCPS r b
    (>>=) = myCPSBind

myCC :: forall a b r. ((a -> MyCPS r b) -> MyCPS r a) -> MyCPS r a
myCC f = MyCPS $ \cont -> let
    -- f :: (a -> (b -> r) -> r) -> (a -> r) -> r
    -- f :: (a -> MyCPS r b) -> MyCPS r a
    -- cont :: (a -> r)

    -- ret :: a -> (b -> r) -> r
    ret :: a -> MyCPS r b
    ret a = MyCPS $ \_ -> cont a

    -- f ret cont
    in (unMyCPS (f ret)) cont

