
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-} 

-- Extensions for compdata usage
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module BooleanAlgebra.Pretty where

import Data.Comp.Term
import Data.Comp.Ops
import Data.Comp.Sum (inject)
import Data.Comp.Algebra
    (   Alg, Coalg, RAlg, RCoalg
    ,   cata, ana, para, apo
    )
import Data.Comp.Render (Render(..), stringTree)

import Data.Comp.Derive
--import Data.Comp.Derive.Show

import Data.Tree (Tree(..))
import Data.Tree.View (showTree)

import Data.Bool (bool)
import Data.List (intersperse)

import BooleanAlgebra.THUtil
import BooleanAlgebra.Base

{-----------------------------------------------------------------------------}
-- "Pretty" printer
-- uses unicode symbols, but doesn't generate valid haskell expressions
-- Src: https://en.wikipedia.org/wiki/List_of_logic_symbols

{- TODO:
    Use https://hackage.haskell.org/package/prettyprinter-1.2.0.1#readme
-}

class (Functor f, Render f) => PrettyBool f where
    -- showsPrec for our pretty printer
    prettyPrintBoolAlg :: Alg f (Int -> ShowS)

-- Lift prettyPrintBoolAlg over sums of functors
$(deriveLiftSum [''PrettyBool])

-- Class for other types, related to boolean expressions
-- (like BooleanExprSimp below)
class PrettyAlmostBool a where
    prettyPrintAB :: a -> Int -> ShowS
    prettyTree :: a -> Tree String

prettyBool :: PrettyAlmostBool a => a -> String
prettyBool e = prettyPrintAB e 0 ""

printBool :: PrettyAlmostBool a => a -> IO ()
printBool = putStrLn . prettyBool

drawBool :: PrettyAlmostBool a => a -> IO ()
drawBool = putStrLn . showTree . prettyTree

{-----------------------------------------------------------------------------}
-- Utilities

-- Generalized show function for constructors
-- Copied over from Data.Comp.Derive.Show for manual ShowF instances
showCon :: String -> [String] -> String
showCon con [] = con
showCon con args = "(" ++ con ++ " " ++ unwords args ++ ")"

-- | Concatenate a list of ShowS using a seperator
ccShowList :: String -> String -> String -> [ShowS] -> ShowS
ccShowList begin sep end list = let
    sList = foldr (.) id $ intersperse (showString sep) list
    in showString begin . sList . showString end

{- | Concatenate lists of precedence-dependent arguments with an operator
-}
ccListOp :: (Int, String) -> (Int -> ShowS) -> [Int -> ShowS] -> Int -> ShowS
ccListOp _          empty []    = empty
ccListOp _          _     [e]   = e
ccListOp (prec, op) _     es    = \d -> let
    listOfShowS :: [ShowS]
    listOfShowS = fmap ($ prec+1) es
    in showParen (d > prec) $ ccShowList "" op "" listOfShowS

{-----------------------------------------------------------------------------}
-- Instances for basic boolean expressions

instance PrettyBool BooleanValue where
    prettyPrintBoolAlg :: BooleanValue (Int -> ShowS) -> Int -> ShowS
    prettyPrintBoolAlg BTrue _ = showString "⊤"
    prettyPrintBoolAlg BFalse _ = showString "⊥"

instance Render BooleanValue

instance PrettyBool BooleanVariable where
    prettyPrintBoolAlg :: BooleanVariable (Int -> ShowS) -> Int -> ShowS
    prettyPrintBoolAlg (BVariable s) _ = showString s

instance Render BooleanVariable

instance PrettyBool BooleanNot where
    prettyPrintBoolAlg :: BooleanNot (Int -> ShowS) -> Int -> ShowS
    prettyPrintBoolAlg (BNot e) d = showParen (d > prec) $
        showString "¬" . e (prec+1)
        where prec = 10

instance Render BooleanNot

instance PrettyBool BooleanAnd where
    prettyPrintBoolAlg :: BooleanAnd (Int -> ShowS) -> Int -> ShowS
    prettyPrintBoolAlg (BAnd a b) d = showParen (d > prec) $
        a (prec+1) . showString "∧" . b (prec+1)
        where prec = 6

instance Render BooleanAnd

instance PrettyBool BooleanOr where
    prettyPrintBoolAlg :: BooleanOr (Int -> ShowS) -> Int -> ShowS
    prettyPrintBoolAlg (BOr a b) d = showParen (d > prec) $
        a (prec+1) . showString "∨" . b (prec+1)
        where prec = 3

instance Render BooleanOr

-- All our normal terms are pretty-printable
instance PrettyBool e => PrettyAlmostBool (Term e) where
    prettyPrintAB :: Term e -> Int -> ShowS
    prettyPrintAB = cata prettyPrintBoolAlg
    prettyTree :: Term e -> Tree String
    prettyTree = stringTree

-- Non-recursive terms can be pretty-printed for any param type
instance PrettyAlmostBool (BooleanValue a) where
    prettyPrintAB :: BooleanValue a -> Int -> ShowS
    prettyPrintAB = prettyPrintBoolAlg . fmap undefined
    prettyTree = stringTreeAlg . fmap undefined

-- Non-recursive terms can be pretty-printed for any param type
instance PrettyAlmostBool (BooleanVariable a) where
    prettyPrintAB :: BooleanVariable a -> Int -> ShowS
    prettyPrintAB = prettyPrintBoolAlg . fmap undefined
    prettyTree = stringTreeAlg . fmap undefined

{-----------------------------------------------------------------------------}
-- Instances for simplified expressions

-- Pretty-print types like: Either (BooleanValue ()) (Term BooleanExprSimpF)
-- actually all instances with "Either a b" are pretty-printable, if both a and b are.
-- We see the "Either" instance as "untagged" because we assume that a and b are disjunct.
instance (PrettyAlmostBool a, PrettyAlmostBool b) => PrettyAlmostBool (Either a b) where
    prettyPrintAB :: Either a b -> Int -> ShowS
    prettyPrintAB (Left v) = prettyPrintAB v
    prettyPrintAB (Right e) = prettyPrintAB e
    prettyTree (Left v ) = Node "Left"  $ [prettyTree v]
    prettyTree (Right e) = Node "Right" $ [prettyTree e]

{-----------------------------------------------------------------------------}
-- Instances for boolean literals

instance PrettyBool BooleanLit where
    prettyPrintBoolAlg :: BooleanLit (Int -> ShowS) -> Int -> ShowS
    prettyPrintBoolAlg (BooleanLit positive s) _
        = (bool (showString "¬") id positive) . (showString s)

instance Render BooleanLit

-- Non-recursive terms can be pretty-printed for any param type
instance PrettyAlmostBool (BooleanLit a) where
    prettyPrintAB :: BooleanLit a -> Int -> ShowS
    prettyPrintAB = prettyPrintBoolAlg . fmap undefined
    prettyTree = stringTreeAlg . fmap undefined

{-----------------------------------------------------------------------------}
-- Instances for aggregate form

{- TODO: Fix bugs in compdata

Bug #1:
    show-ing terms of BooleanCD is wrong and puts quotes where none belong:
        putStrLn $ show exampleExpr03
        (BooleanCD [["(BooleanLit True \"a\")","(BooleanLit False \"b\")"],["(BooleanLit False \"c\")","(BooleanLit True \"d\")"]])
    
    Data.Comp.Derive.Show handles arguments this way:
        mkShow :: (Bool, ExpQ) -> ExpQ
        mkShow (isFArg, var)
            | isFArg = var
            | otherwise = [| show $var |]
    Apparently [[e]] is not a functor argument - this can lead to other bugs!

    This one will be hard to get right, but should accept nested functors:
        data Meh e = Meh e          deriving Functor
        data Muh e = Muh (Meh e)    deriving Functor

Bug #2:
    compdata should use showsPrec instead of show
    1. Performance (maybe not that relevant, ghc rewrites a lot of that stuff)
    2. Precedence is important, don't put parens everywhere

-}

-- Custom instance of ShowF - workaround for a bug in compdata
instance ShowF Conjunction where
    showF (Conjunction xs) = let
        ccList :: [ShowS] -> ShowS
        ccList = ccShowList "[" ", " "]"
        strCDs :: ShowS
        strCDs = ccList . fmap (++) $ xs
        in (showCon "Conjunction") [strCDs ""]

instance ShowF Disjunction where
    showF (Disjunction xs) = let
        ccList :: [ShowS] -> ShowS
        ccList = ccShowList "[" ", " "]"
        strCDs :: ShowS
        strCDs = ccList . fmap (++) $ xs
        in (showCon "Disjunction") [strCDs ""]

-- Custom instance of ShowConstr
-- FIXME: not pretty, due to the defintion of ShowConstr
instance ShowConstr Conjunction where
    showConstr :: Conjunction a -> String
    showConstr _ = "Conjunction []"

-- Custom instance of ShowConstr
-- FIXME: not pretty, due to the defintion of ShowConstr
instance ShowConstr Disjunction where
    showConstr :: Disjunction a -> String
    showConstr _ = "Disjunction []"

-- Pretty-printer for Conjunction
instance PrettyBool Conjunction where
    prettyPrintBoolAlg :: Conjunction (Int -> ShowS) -> Int -> ShowS
    prettyPrintBoolAlg (Conjunction ts)
        = showConjs ts where
            showConjs :: [Int -> ShowS] -> Int -> ShowS
            showConjs = ccListOp (6, "∧") empty where
                empty = prettyPrintAB BTrue

-- Pretty-printer for Disjunction
instance PrettyBool Disjunction where
    prettyPrintBoolAlg :: Disjunction (Int -> ShowS) -> Int -> ShowS
    prettyPrintBoolAlg (Disjunction ts)
        = showDisjs ts where
            showDisjs :: [Int -> ShowS] -> Int -> ShowS
            showDisjs = ccListOp (3, "∨") empty where
                empty = prettyPrintAB BFalse

-- Special instance, BooleanCD is just "a little different"
instance Render Conjunction
instance Render Disjunction

{-----------------------------------------------------------------------------}
-- Instances for CNF

-- Pretty-printer for CNF
-- Idea: Maybe print each disjunction on a seperate line
instance PrettyAlmostBool a => PrettyAlmostBool (Conjunction a) where
    prettyPrintAB :: Conjunction a -> Int -> ShowS
    prettyPrintAB = prettyPrintBoolAlg . fmap prettyPrintAB
    prettyTree = stringTreeAlg . fmap prettyTree

instance PrettyAlmostBool a => PrettyAlmostBool (Disjunction a) where
    prettyPrintAB :: Disjunction a -> Int -> ShowS
    prettyPrintAB = prettyPrintBoolAlg . fmap prettyPrintAB
    prettyTree = stringTreeAlg . fmap prettyTree
