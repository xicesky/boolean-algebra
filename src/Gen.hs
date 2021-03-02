
-- | Let's try to generate some formulas for Sudoku :)
module Gen where

import Prelude hiding (and, or, not, (&&), (||))

import BooleanAlgebra

{-----------------------------------------------------------------------------}
-- Easy examples

neatExample01 :: forall b. BooleanAlgebra b => b
neatExample01 = forAll [1..9] $ (`is` odd) || (`is` even)

neatExample02 :: forall b. BooleanAlgebra b => b
neatExample02 = exists [1..9] $ not (`is` odd)

chooseInt01 :: forall b. BooleanAlgebra b => b
chooseInt01 = existsUnique [1..3] (\x -> var $ "N" ++ show x)

-- !!! @toCNF (chooseInt02 :: BooleanExpr)@ generates 729 clauses á 7 literals
-- (at least with the "stupid" simplifier)
chooseInt02 :: forall b. BooleanAlgebra b => b
chooseInt02 = existsUnique' [1..3] (\x -> var $ "N" ++ show x)

sortList :: forall b. BooleanAlgebra b => b
sortList = let
    -- | The n-th number is at the position i in the sorted list
    isAt :: BooleanAlgebra b => Int -> Int -> b
    isAt n i = var $ "N" ++ show n ++ "P" ++ show i

    -- | Three numbers, three indices
    ns :: [Int]
    ns = [1..3]

    in foldr1 and
        [   -- Each number has a unique position
            forAll ns $ \number ->
            existsUnique ns $ \position ->
            number `isAt` position
        ]


{-----------------------------------------------------------------------------}
-- Sudoku WIP

sudoku :: BooleanAlgebra b => b
sudoku = let
    ns = [1..2]

    -- | Field x, y has number z
    p :: BooleanAlgebra b => Int -> Int -> Int -> b
    p x y z = var $ "P" ++ show x ++ "_" ++ show y ++ "_" ++ show z

    in foldr1 and
        [   exists ns $ forAll [3..4] $ p 0 -- forAll ns $ exists ns $ p 0
        ]

{- Idea: Encode the pidgeonhole problem!
-}

{- Idea: Encode "Who owns the zebra":
https://drive.google.com/file/d/1WRUQSKIHKLpEv3_OlcB6P4Z8G2-sismi/view
-}

{- Idea: Build isomorphisms between problem domain and SAT, so
we can easily encode the problem and decode the result.

Idea: Use isos to illustrate the solving process.
We could inspect certain variable assignments during solving,
or even interpret clauses.
interesting:
    - Facts found (e.g. assigned number)
    - Learned clauses
    - Implication graph...
    - Implications that lead to each fact
-}
