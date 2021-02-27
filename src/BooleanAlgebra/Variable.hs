
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

import Prelude hiding (lookup)
import Control.Monad.Reader
import Data.Functor.Identity (Identity(..))

import Data.Comp.Term
import Data.Comp.Ops
import Data.Comp.Sum (inject)
import Data.Comp.Algebra
import Data.Comp.Generic (subterms')

import Container

import BooleanAlgebra.THUtil
import BooleanAlgebra.Base

-- Fetch the names of all the variables
variableNames :: forall f. (Foldable f, BooleanVariable :<: f) =>
    Term f -> [String]
variableNames = fmap varName . subterms'

{-----------------------------------------------------------------------------}
-- Variable substitution

{- | A substitution from constructors in f to terms of g.
    Wrapped in a newtype to avoid issues with impredicative polymorphism.
    You can think of it as:
    >>> type Subst m f g = forall a. f a -> m (Context g a)
-}
newtype Subst m f g = Subst (HomM m f g)

-- | A Monad used to run the substitution.
type VarSubstM g m = ReaderT (Subst m BooleanVariable g) m

-- | Lift a non-monadic term homomorphism to a monadic one
liftHom :: Monad m => Hom f g -> HomM m f g
liftHom hom = return . hom

{- | substVars replaces occurances of @BooleanVariable@ by another term t.
-}
class (Traversable f, Functor g) => SubstVar f g where
    substVarAlg :: Monad m => AlgM (VarSubstM g m) f (Term g)
    -- :: f (Term g) -> VarSubstM g m (Term g)

-- FIXME: I don't like overlappable instances
-- But there is no way to avoid them here

instance {-# OVERLAPPABLE #-} (SubstVar f h, SubstVar g h) => SubstVar (f :+: g) h where
    substVarAlg = caseF substVarAlg substVarAlg

-- The "default" instance. We can avoid overlappable here
instance {-# OVERLAPPABLE #-} (Traversable f, Functor g, f :<: g) => SubstVar f g where
    substVarAlg (fs :: f (Term g)) = return $ Term . inj $ fs

-- The specialized instance for variables
instance (Functor g) => SubstVar BooleanVariable g where
    substVarAlg :: forall m. Monad m => AlgM (VarSubstM g m) BooleanVariable (Term g)
    substVarAlg (fs :: BooleanVariable (Term g)) = do
            (Subst hom :: Subst m BooleanVariable g) <- ask
            (gtg :: Context g (Term g)) <- lift $ hom fs
            return $ appCxt gtg

substVarM :: forall m f g. (Monad m, SubstVar f g)
    => HomM m BooleanVariable g -> Term f -> m (Term g)
substVarM hom f = runReaderT (cataM substVarAlg f) (Subst hom)

substVar :: forall f g. SubstVar f g
    => Hom BooleanVariable g -> Term f -> Term g
substVar hom f = runIdentity $ substVarM (liftHom hom) f

{-----------------------------------------------------------------------------}
-- Substitute variables using replacements from a map

-- | Flexible monadic version
substituteM :: forall g m map.
    ( Functor g, Monad m
    , MapLike map, (String, Term g) ~ ElemT map     -- Keys are Strings, Values are Terms
    , SubstVar BooleanBaseF g
    ) => map                                        -- Maps names to new terms
    -> (forall a. String -> m (Context g a))        -- Action on unmapped variables
    -> Term BooleanBaseF                            -- Term to transform
    -> m (Term g)
substituteM map err = substVarM hom where
    hom :: BooleanVariable a -> m (Context g a)
    hom (BVariable s) = case lookup s map of
        Just expr   -> return $ toCxt expr
        Nothing     -> err s

-- | Partial substitution (leaves unmapped variables in place)
substituteM' :: forall m map.
    ( Monad m
    , MapLike map, (String, Term BooleanBaseF) ~ ElemT map
    , SubstVar BooleanBaseF BooleanBaseF
    ) => map
    -> Term BooleanBaseF
    -> m (Term BooleanBaseF)
substituteM' map = substituteM map (return . iBVar)

-- | Non-monadic version of substituteM'
substitute' :: forall map.
    ( MapLike map, (String, Term BooleanBaseF) ~ ElemT map -- Keys are Strings, Values are Terms
    , SubstVar BooleanBaseF BooleanBaseF
    ) => map                                        -- Maps names to new terms
    -> Term BooleanBaseF                            -- Term to transform
    -> Term BooleanBaseF
substitute' map = runIdentity . substituteM' map
