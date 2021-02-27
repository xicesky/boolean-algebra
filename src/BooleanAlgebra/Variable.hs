
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-} 

-- Extensions for compdata usage
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE GADTs                  #-}

module BooleanAlgebra.Variable where

import Data.Comp.Term
import Data.Comp.Ops
import Data.Comp.Sum (inject)
import Data.Comp.Algebra
    (   Alg, Coalg, RAlg, RCoalg
    ,   cata, ana, para, apo
    ,   Hom, CxtFun
    ,   appCxt, appHom
    )
import Data.Comp.Generic (subterms')

import BooleanAlgebra.THUtil
import BooleanAlgebra.Base

-- Fetch the names of all the variables
variableNames :: forall f. (Foldable f, BooleanVariable :<: f) =>
    Term f -> [String]
variableNames = fmap varName . subterms'

{-----------------------------------------------------------------------------}
-- Variable substitution

-- TODO: Monadic version will come in handy!

newtype Subst f g = Subst (Hom f g)

{- | substVars replaces occurances of @BooleanVariable@ by another term t.
-}
class (Functor f, Functor g) => SubstVar f g where
    substVarAlg :: Alg f (Subst BooleanVariable g -> Term g)

-- FIXME: I don't like overlappable instances
-- But there is no way to avoid them here

instance {-# OVERLAPPABLE #-} (SubstVar f h, SubstVar g h) => SubstVar (f :+: g) h where
    substVarAlg = caseF substVarAlg substVarAlg

-- The "default" instance. We can avoid overlappable here
instance {-# OVERLAPPABLE #-} (Functor f, Functor g, f :<: g) => SubstVar f g where
    substVarAlg fs subst = Term . inj . fmap ($ subst) $ fs

-- The specialized instance for variables
instance (Functor g) => SubstVar BooleanVariable g where
    substVarAlg bv (Subst (hom :: Hom BooleanVariable g))
        = appCxt . hom . fmap undefined $ bv
    -- FIXME: replace "fmap undefined" by varIsConst :: ∀a b. BooleanVariable a -> BooleanVariable b

substVar :: forall f g. SubstVar f g
    => Hom BooleanVariable g -> Term f -> Term g
substVar hom f = cata substVarAlg f (Subst hom)
