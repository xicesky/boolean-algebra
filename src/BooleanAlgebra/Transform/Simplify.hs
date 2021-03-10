
module BooleanAlgebra.Transform.Simplify where

--import Prelude hiding ((!!), lookup)

import Data.Kind (Type)
import Data.Void
import Data.Bool (bool)

-- recursion-schemes
import Data.Functor.Foldable

import Missing.Misc (Alg)
import Term.Term
import Term.Inject

import BooleanAlgebra.Base.Expression

{-----------------------------------------------------------------------------}
-- Constant folding

{- | Fold constants in a term.

TODO: Create a version that works on flattened terms.
-}
constantFold' :: Term BOps Bool a -> Either Bool (Term BOps Void a)
constantFold' = cata fRec where
    fRec :: Alg (TermF BOps Bool a) (Either Bool (Term BOps Void a))
    fRec (ConstT v) = Left v
    fRec (VariableT v) = Right $ Var v
    fRec (RecT (UnaryOp BooleanNot t)) = case t of
        Left b      -> Left $ not b
        Right t'    -> Right $ BNot t'  -- We could have "Right $ not t'" here, woot?
    fRec (RecT (BinaryOp BooleanAnd l r)) = sAnd l r where
        sAnd (Right a)      (Right b)   = Right $ BAnd a b
        sAnd (Left True)    rhs         = rhs
        sAnd (Left False)   _           = Left False
        sAnd lhs            rhs         = sAnd rhs lhs -- symm.
    fRec (RecT (BinaryOp BooleanOr l r)) = sOr l r where
        sOr  (Right a)      (Right b)   = Right $ BOr a b
        sOr  (Left True)    _           = Left True
        sOr  (Left False)   rhs         = rhs
        sOr  lhs            rhs         = sOr rhs lhs -- symm.
    fRec (RecT (FlatOp op _)) = absurd op

-- Generalized to arbitrary inputs
constantFold :: (t a :<: Term BOps Bool a) => t a -> Either Bool (Term BOps Void a)
constantFold = constantFold' . inject

{-----------------------------------------------------------------------------}
-- Push negations up to variables, creating literals

pushNegations' :: Term BOps Void a -> TermLit BNOps Void a
pushNegations' term = cata pushNeg term True where
    pushNeg :: Alg (TermF BOps Void a) (Bool -> TermLit BNOps Void a)
    pushNeg (ConstT x) = absurd x
    pushNeg (VariableT x) = \b -> TermLit $ Var (b, x)
    pushNeg (RecT (UnaryOp BooleanNot t)) = t . not
    pushNeg (RecT (BinaryOp op l r)) = \b -> TermLit $
        BBOp (bool invertOp id b op) (unTermLit $ l b) (unTermLit $ r b)
    pushNeg (RecT (FlatOp op _)) = absurd op

{-----------------------------------------------------------------------------}
-- Flattening

flatten' :: forall uop val var. ProperOpTag uop
     => Term (Op uop BooleanBOp Void) val var -> Term (Op uop Void BooleanFlatOp) val var
flatten' = cata flat where
    flat :: Alg (TermF (Op uop BooleanBOp Void) val var) (Term (Op uop Void BooleanFlatOp) val var)
    flat (ConstT v) = Val v
    flat (VariableT v) = Var v
    --flat (RecT (UnaryOp op _)) = absurd op
    flat (RecT (UnaryOp op t)) = BUOp op t
    flat (RecT (BinaryOp BooleanAnd a b)) = case (a, b) of
        (BConj l, BConj r)  -> BConj (l ++ r)
        (BConj l, rhs    )  -> BConj (l ++ [rhs])
        (lhs    , BConj r)  -> BConj (lhs : r)
        (lhs    , rhs    )  -> BConj [lhs, rhs]
    flat (RecT (BinaryOp BooleanOr a b)) = case (a, b) of
        (BDisj l, BDisj r)  -> BDisj (l ++ r)
        (BDisj l, rhs    )  -> BDisj (l ++ [rhs])
        (lhs    , BDisj r)  -> BDisj (lhs : r)
        (lhs    , rhs    )  -> BDisj [lhs, rhs]
    flat (RecT (FlatOp op _)) = absurd op

{- | 'flatten'' Generalized to arbitrary inputs.

Sometimes GHC will try to fix 'val' and 'var' to arbitrary types. You can avoid
that by using type applications, e.g.:

>>> flatten @Bool @String $ constantFold demo2b
-}
flatten :: forall val var t. (t :<: Term BNOps val var) => t -> Term BFlOps val var
flatten = flatten' . inject

{-----------------------------------------------------------------------------}
-- Simplifier
-- Doesn't currently simplify a lot

simplify :: (t a :<: Term BOps Bool a) => t a -> TermLit BFlOps Void a
simplify term = case constantFold term of
    Left True -> TermLit $ BConj []
    Left False -> TermLit $ BDisj []
    Right b -> (TermLit . flatten . unTermLit . pushNegations') b
