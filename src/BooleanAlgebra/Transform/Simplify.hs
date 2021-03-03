
module BooleanAlgebra.Transform.Simplify where

import Data.Bool (bool)

import Data.Comp
import Data.Comp.Derive

import BooleanAlgebra.Util.THUtil
import BooleanAlgebra.Util.Util
import BooleanAlgebra.Base.Expression
import BooleanAlgebra.Transform.IntermediateForms

{-----------------------------------------------------------------------------}
-- Simplifier   (Step 1 of toCNF)

{- simpBool pushes boolean values outwards
    and mostly eliminates them.
    If any values BTrue/BFalse remain, they are the
    topmost expression (i.e. if the whole expression is True/False).
-}
class Functor f => SimpBool f where
    simpBool :: Alg f BooleanExprSimp

-- Lift simpBool over sums of functors
$(deriveLiftSum [''SimpBool])

instance SimpBool BooleanValue where
    simpBool :: BooleanValue BooleanExprSimp -> BooleanExprSimp
    simpBool BTrue      = Left BTrue
    simpBool BFalse     = Left BFalse

instance SimpBool BooleanVariable where
    simpBool :: BooleanVariable BooleanExprSimp -> BooleanExprSimp
    simpBool = Right . inject . constmap
    -- simpBool (BVariable v)  = Right $ iBVar v

instance SimpBool BooleanNot where
    simpBool :: BooleanNot BooleanExprSimp -> BooleanExprSimp
    simpBool (BNot (Left BTrue))    = Left BFalse
    simpBool (BNot (Left BFalse))   = Left BTrue
    simpBool (BNot (Right other))   = Right $ iBNot other

instance SimpBool BooleanOp where
    simpBool :: BooleanOp BooleanExprSimp -> BooleanExprSimp
    simpBool (BAnd (Left BTrue) e)  = e
    simpBool (BAnd (Left BFalse) e) = Left BFalse
    simpBool (BAnd e (Left BTrue))  = e
    simpBool (BAnd e (Left BFalse)) = Left BFalse
    simpBool (BAnd (Right a) (Right b)) = Right $ iBAnd a b

    simpBool (BOr (Left BTrue) e)   = Left BTrue
    simpBool (BOr (Left BFalse) e)  = e
    simpBool (BOr e (Left BTrue))   = Left BTrue
    simpBool (BOr e (Left BFalse))  = e
    simpBool (BOr (Right a) (Right b)) = Right $ iBOr a b

simplifyPrimitive :: SimpBool f => Term f -> BooleanExprSimp
simplifyPrimitive = cata simpBool

{-----------------------------------------------------------------------------}
-- Boolean literals     (Step 2 of toCNF)
-- Literal = Variable + optional Negation

-- pushNeg eliminates negations by pushing them inwards
-- and turning variables into literals
class Functor f => PushNeg f where
    pushNeg :: Alg f (Bool -> BooleanExprLit)

-- Lift pushNeg over sums of functors
$(deriveLiftSum [''PushNeg])

instance PushNeg BooleanVariable where
    pushNeg :: BooleanVariable (Bool -> BooleanExprLit) -> (Bool -> BooleanExprLit)
    pushNeg (BVariable s) positive = iBooleanLit $ bool (-s) s positive

instance PushNeg BooleanNot where
    pushNeg :: BooleanNot (Bool -> BooleanExprLit) -> (Bool -> BooleanExprLit)
    pushNeg (BNot lit) positive = lit (not positive)

instance PushNeg BooleanOp where
    pushNeg :: BooleanOp (Bool -> BooleanExprLit) -> (Bool -> BooleanExprLit)
    pushNeg (BAnd a b) False = iBOr (a False) (b False)
    pushNeg (BAnd a b) True  = iBAnd (a True) (b True)
    pushNeg (BOr a b) False = iBAnd (a False) (b False)
    pushNeg (BOr a b) True  = iBOr (a True) (b True)

-- Idea: PushNeg can operate on extended operation (see below)
pushNegations :: PushNeg f => Term f -> BooleanExprLit
pushNegations e = cata pushNeg e True

-- We usually want to apply it to BooleanExprSimp directly
-- usage e.g.: prettyBool $ pushNegations' $ simplifyPrimitive $ exampleExpr05
pushNegations' :: PushNeg f => MaybeTrivial (Term f) -> MaybeTrivial BooleanExprLit
pushNegations' = fmap pushNegations

{- TODO:
Maybe pushNegations can be decomposed into two functions
    mkLit :: Bool -> Variable -> Literal
    pushNegations :: (Bool -> Variable -> a) -> Term (...) -> Term (... :+: a)
So we can get the function
     pushNegForm :: BooleanExprSimp -> BooleanExprSimp
for free

TODO: Function to turn literals back into BooleanVariable :+: BooleanNot
-}

{-----------------------------------------------------------------------------}
-- Complete simplification step

simplify :: SimpBool f => Term f -> MaybeTrivial BooleanExprLit
simplify = pushNegations' . simplifyPrimitive
