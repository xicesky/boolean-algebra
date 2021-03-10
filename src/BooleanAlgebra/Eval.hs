
module BooleanAlgebra.Eval
    (
    ) where

import Prelude hiding (and, or, not, (&&), (||))

import Data.Kind (Type)
import Data.Void

import Missing.Void
import Term.Term
import Term.Substitution
import Term.Inject

import BooleanAlgebra.Base.Class
import BooleanAlgebra.Base.Expression
import BooleanAlgebra.Transform.Simplify

{-----------------------------------------------------------------------------}
-- Evaluation

class Eval f name where
    eval :: (name -> Bool) -> f name -> Bool

instance Ord name => Eval (Term BOps Bool) name where
    eval lookup term = let     -- constantFold already does it, and we can prove it :)
        saturated :: Term BOps Bool Void
        saturated = substVars (Val . lookup) term
        in case constantFold' saturated of
            Left b  -> b
            Right x -> absurd' x

instance Ord name => Eval (TermLit BNOps Void) name where
    eval :: (name -> Bool) -> TermLit BNOps Void name -> Bool
    eval lookup term = let
        lookupLit :: Literal name -> Term BNOps Bool Void
        lookupLit (b, name) = Val $ lookup name `xor` b
        saturated :: Term BNOps Bool Void
        saturated = substVars lookupLit $ inject $ unTermLit term
        term' :: Term BOps Bool Void
        term' = inject saturated
        in case constantFold' term' of
            Left b  -> b
            Right x -> absurd' x
