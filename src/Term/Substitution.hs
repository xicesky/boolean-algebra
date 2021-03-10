

{- |
Description     : Variable substitution
Stability       : experimental

Utility for substituting variables with arbitrary terms
(of the same type).
-}
module Term.Substitution
    (   substVars
    ) where

import Term.Term

-- | Treat variables as holes filled with terms
appTerm :: Term op val (Term op val var) -> Term op val var
appTerm (Val x) = Val x
appTerm (Var x) = x
appTerm (Rec f) = Rec $ fmap appTerm f

-- | Substitute terms for variables
substVars :: (var -> Term op val var') -> Term op val var -> Term op val var'
substVars f = appTerm . fmap f
