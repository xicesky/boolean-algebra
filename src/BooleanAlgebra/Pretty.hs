
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

import Data.Comp.Derive
--import Data.Comp.Derive.Show
import Data.Comp.Show ()            -- for the Show instance
import Data.Comp.Equality ()        -- for the Eq instance

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

class Functor f => PrettyBool f where
    -- showsPrec for our pretty printer
    prettyPrintBoolAlg :: Alg f (Int -> ShowS)

-- Lift prettyPrintBoolAlg over sums of functors
$(deriveLiftSum [''PrettyBool])

-- Class for other types, related to boolean expressions
-- (like BooleanExprSimp below)
class PrettyAlmostBool a where
    prettyPrintAB :: a -> Int -> ShowS

prettyBool :: PrettyAlmostBool a => a -> String
prettyBool e = prettyPrintAB e 0 ""

{-----------------------------------------------------------------------------}
-- Utilities

-- Generalized show function for constructors
-- Copied over from Data.Comp.Derive.Show for manual ShowF instances
showCon :: String -> [String] -> String
showCon con [] = con
showCon con args = "(" ++ con ++ " " ++ unwords args ++ ")"

-- Utility function: Concatenate a list of ShowS using a seperator
ccShowList :: String -> String -> String -> [ShowS] -> ShowS
ccShowList begin sep end list = let
    sList = foldr (.) id $ intersperse (showString sep) list
    in showString begin . sList . showString end

{- Utility function:
Concatenate lists of precedence-dependent arguments with an operator
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

instance PrettyBool BooleanVariable where
    prettyPrintBoolAlg :: BooleanVariable (Int -> ShowS) -> Int -> ShowS
    prettyPrintBoolAlg (BVariable s) _ = showString s

instance PrettyBool BooleanNot where
    prettyPrintBoolAlg :: BooleanNot (Int -> ShowS) -> Int -> ShowS
    prettyPrintBoolAlg (BNot e) d = showParen (d > prec) $
        showString "¬" . e (prec+1)
        where prec = 10

instance PrettyBool BooleanAnd where
    prettyPrintBoolAlg :: BooleanAnd (Int -> ShowS) -> Int -> ShowS
    prettyPrintBoolAlg (BAnd a b) d = showParen (d > prec) $
        a (prec+1) . showString "∧" . b (prec+1)
        where prec = 6

instance PrettyBool BooleanOr where
    prettyPrintBoolAlg :: BooleanOr (Int -> ShowS) -> Int -> ShowS
    prettyPrintBoolAlg (BOr a b) d = showParen (d > prec) $
        a (prec+1) . showString "∨" . b (prec+1)
        where prec = 3

-- All our normal terms are pretty-printable
instance PrettyBool e => PrettyAlmostBool (Term e) where
    prettyPrintAB :: Term e -> Int -> ShowS
    prettyPrintAB = cata prettyPrintBoolAlg

-- Non-recursive terms can be pretty-printed for any param type
instance PrettyAlmostBool (BooleanValue a) where
    prettyPrintAB :: BooleanValue a -> Int -> ShowS
    prettyPrintAB = prettyPrintBoolAlg . fmap undefined

-- Non-recursive terms can be pretty-printed for any param type
instance PrettyAlmostBool (BooleanVariable a) where
    prettyPrintAB :: BooleanVariable a -> Int -> ShowS
    prettyPrintAB = prettyPrintBoolAlg . fmap undefined

{-----------------------------------------------------------------------------}
-- Instances for simplified expressions

-- Pretty-print types like: Either (BooleanValue ()) (Term BooleanExprSimpF)
-- actually all instances with "Either a b" are pretty-printable, if both a and b are.
-- We see the "Either" instance as "untagged" because we assume that a and b are disjunct.
instance (PrettyAlmostBool a, PrettyAlmostBool b) => PrettyAlmostBool (Either a b) where
    prettyPrintAB :: Either a b -> Int -> ShowS
    prettyPrintAB (Left v) = prettyPrintAB v
    prettyPrintAB (Right e) = prettyPrintAB e

{-----------------------------------------------------------------------------}
-- Instances for boolean literals

instance PrettyBool BooleanLit where
    prettyPrintBoolAlg :: BooleanLit (Int -> ShowS) -> Int -> ShowS
    prettyPrintBoolAlg (BooleanLit positive s) _
        = (bool (showString "¬") id positive) . (showString s)

-- Non-recursive terms can be pretty-printed for any param type
instance PrettyAlmostBool (BooleanLit a) where
    prettyPrintAB :: BooleanLit a -> Int -> ShowS
    prettyPrintAB = prettyPrintBoolAlg . fmap undefined

{-----------------------------------------------------------------------------}
-- Instances for aggregate form

-- Pretty-printer for BooleanCD
instance PrettyBool BooleanCD where
    prettyPrintBoolAlg :: BooleanCD (Int -> ShowS) -> Int -> ShowS
    prettyPrintBoolAlg (BooleanCD cds) d
        = showCDs cds d where
            showDisjs :: [Int -> ShowS] -> Int -> ShowS
            showDisjs = ccListOp (3, "∨") empty where
                empty = prettyPrintAB BFalse

            showConjs :: [Int -> ShowS] -> Int -> ShowS
            showConjs = ccListOp (6, "∧") empty where
                empty = prettyPrintAB BTrue

            showCDs :: [[Int -> ShowS]] -> Int -> ShowS
            showCDs = showConjs . fmap showDisjs

{-----------------------------------------------------------------------------}
-- Instances for CNF

