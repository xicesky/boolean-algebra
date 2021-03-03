
module BooleanAlgebra.Transform.Simplify where

import Prelude hiding ((!!), lookup)

import Data.Bool (bool)
import Data.Functor.Identity (Identity(..))
import Control.Monad.Reader

import Container
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import Data.Comp
import Data.Comp.Derive

import BooleanAlgebra.Util.THUtil
import BooleanAlgebra.Util.Util
import BooleanAlgebra.Base.Expression
import BooleanAlgebra.Transform.IntermediateForms

import BooleanAlgebra.Transform.Variable

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

-- | Monad used for the transformation
type PushNegM m = ReaderT (String -> Int) m
    -- (String -> Int) -> Bool -> m a

-- Tiny little helper
-- withPNM :: Monad m => ((String -> Int) -> Bool -> PushNegM m a) -> PushNegM m a
-- withPNM f = do
--     (r, n) <- ask
--     f r n

-- pushNeg eliminates negations by pushing them inwards
-- and turning variables into literals
class (Functor f, Traversable f) => PushNeg f where
    pushNeg :: Monad m => AlgM (PushNegM m) f (Bool -> BooleanExprLit)

-- Lift pushNeg over sums of functors
$(deriveLiftSum [''PushNeg])

instance PushNeg BooleanVariable where
    --pushNeg :: BooleanVariable (Bool -> BooleanExprLit) -> (Bool -> BooleanExprLit)
    pushNeg :: Monad m => AlgM (PushNegM m) BooleanVariable (Bool -> BooleanExprLit)
    pushNeg (BVariable s) = do
        resolve <- ask
        let (i :: Int) = resolve s
        return $ \positive -> iBooleanLit $ bool (-i) i positive

instance PushNeg BooleanNot where
    pushNeg :: Monad m => AlgM (PushNegM m) BooleanNot (Bool -> BooleanExprLit)
    pushNeg (BNot lit) = return $ \positive -> lit (not positive)

instance PushNeg BooleanOp where
    --pushNeg :: BooleanOp (Bool -> BooleanExprLit) -> (Bool -> BooleanExprLit)
    pushNeg :: Monad m => AlgM (PushNegM m) BooleanOp (Bool -> BooleanExprLit)
    pushNeg (BAnd a b) = return $ \case
        False   -> iBOr  (a False) (b False)
        True    -> iBAnd (a True)  (b True)
    pushNeg (BOr a b) = return $ \case
        False   -> iBAnd (a False) (b False)
        True    -> iBOr  (a True)  (b True)

pushNegationsM :: (Monad m, PushNeg f, BooleanVariable :<: f) => Term f -> m ([String], BooleanExprLit)
pushNegationsM e = do
    let (names, map) = makeVariableMap e
    f <- runReaderT (cataM pushNeg e) (map !!)
    return (names, f True)

-- Idea: PushNeg can operate on extended operation (see below)
pushNegations :: (PushNeg f, BooleanVariable :<: f) => Term f -> ([String], BooleanExprLit)
pushNegations = runIdentity . pushNegationsM

-- We usually want to apply it to BooleanExprSimp directly
-- usage e.g.: prettyBool $ pushNegations' $ simplifyPrimitive $ exampleExpr05
pushNegations' :: (PushNeg f, BooleanVariable :<: f) => MaybeTrivial (Term f) -> ([String], MaybeTrivial BooleanExprLit)
pushNegations' = traverse pushNegations

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

simplify :: SimpBool f => Term f -> ([String], MaybeTrivial BooleanExprLit)
simplify = pushNegations' . simplifyPrimitive
