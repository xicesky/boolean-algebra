
{-# LANGUAGE UndecidableInstances   #-}

module BooleanAlgebra.Transform.Variable where

import Prelude hiding (lookup, (!!))
import Data.Tuple (swap)
import Data.Bool (bool)
import Data.Void
import Control.Monad.Reader
import Data.Functor.Identity (Identity(..))
import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashSet as HashSet

import Data.Comp.Term
import Data.Comp.Ops
import Data.Comp.Sum (inject)
import Data.Comp.Algebra
import Data.Comp.Generic (subterms')

import Container

import BooleanAlgebra.Util.THUtil
import BooleanAlgebra.Base.Expression

{-----------------------------------------------------------------------------}

class HasVariables t where
    -- | Fetch the names of all the variables
    variableNames :: t -> HashSet String

-- | Fetch the all the numbered literals
numberedLiterals :: forall f. (Foldable f, BooleanLitI :<: f) =>
    Term f -> HashSet Int
numberedLiterals = fromList . fmap unLit . subterms'

-- | Fetch the all "names" of the numbered literals
numberedVariables :: forall f. (Foldable f, BooleanLitI :<: f) =>
    Term f -> HashSet Int
numberedVariables = fromList . fmap (abs . unLit) . subterms'

-- Cant make a default because ghc will complain about (Term f)
variableNamesDefault :: forall f. (Foldable f, BooleanVariable :<: f) =>
    Term f -> HashSet String
variableNamesDefault = fromList . fmap varName . subterms'

{-----------------------------------------------------------------------------}

-- | Fetch the names of all the literals
literalNamesDefault :: forall f. (Foldable f, BooleanLit :<: f) =>
    Term f -> HashSet String
literalNamesDefault = fromList . fmap litName . subterms'

instance (Foldable f, BooleanVariable :<: f) => HasVariables (Term f) where
    variableNames = variableNamesDefault

instance HasVariables BooleanExprLit where
    variableNames = literalNamesDefault

instance HasVariables BooleanExprFlatLit where
    variableNames = literalNamesDefault

instance HasVariables CNF where
    variableNames = fromList . fmap litName . (toList <=< toList)

{-----------------------------------------------------------------------------}
-- Variable substitution

{- | A substitution from constructors in f to terms of g.

Wrapped in a newtype to avoid issues with impredicative polymorphism.

You can think of it as:

> type Subst m f g = forall a. f a -> m (Context g a)
-}
newtype Subst m f g = Subst (HomM m f g)

-- | A Monad used to run the substitution.
type VarSubstM g m = ReaderT (Subst m BooleanVariable g) m

-- | Lift a non-monadic term homomorphism to a monadic one
liftHom :: Monad m => Hom f g -> HomM m f g
liftHom hom = return . hom

{- | Class of terms in which variables can be substituted
-}
class (Traversable f, Functor g) => SubstVar f g where
    -- Replaces occurances of 'BooleanVariable' by another term t.
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

-- | Monadic variable substitution catamorphism.
substVarM :: forall m f g. (Monad m, SubstVar f g)
    => HomM m BooleanVariable g -> Term f -> m (Term g)
substVarM hom f = runReaderT (cataM substVarAlg f) (Subst hom)

-- | Variable substitution catamorphism.
--
-- Non-monadic version of 'substVarM'
substVar :: forall f g. SubstVar f g
    => Hom BooleanVariable g -> Term f -> Term g
substVar hom f = runIdentity $ substVarM (liftHom hom) f

{-----------------------------------------------------------------------------}
-- Substitute variables using replacements from a map

-- | Substitute variables using replacements from a map.
--
-- Flexible monadic version
substituteM :: forall f g m map.
    ( Traversable f, Functor g, Monad m
    , MapLike map, (String, Term g) ~ ElemT map     -- Keys are Strings, Values are Terms
    , SubstVar f g
    ) => map                                        -- Maps names to new terms
    -> (forall a. String -> m (Context g a))        -- Action on unmapped variables
    -> Term f                                       -- Term to transform
    -> m (Term g)
substituteM map err = substVarM hom where
    hom :: BooleanVariable a -> m (Context g a)
    hom (BVariable s) = case lookup s map of
        Just expr   -> return $ toCxt expr
        Nothing     -> err s

-- | Substitute variables using replacements from a map.
--
-- Partial substitution (leaves unmapped variables in place)
substituteM' :: forall f m map.
    ( Traversable f, Monad m
    , MapLike map, (String, Term f) ~ ElemT map
    , SubstVar f f
    , BooleanVariable :<: f
    ) => map
    -> Term f
    -> m (Term f)
substituteM' map = substituteM map (return . iBVar)

-- | Substitute variables using replacements from a map.
--
-- Non-monadic version of 'substituteM''
substitute' :: forall f map.
    ( Traversable f
    , MapLike map, (String, Term f) ~ ElemT map
    , SubstVar f f
    , BooleanVariable :<: f
    ) => map
    -> Term f
    -> Term f
substitute' map = runIdentity . substituteM' map

{-----------------------------------------------------------------------------}
-- Substitutions for BooleanLit

-- | A Monad used to run the substitution of literals.
type LitSubstM g m = ReaderT (Subst m BooleanLit g) m

class (Traversable f, Functor g) => SubstLit f g where
    substLitAlg :: Monad m => AlgM (LitSubstM g m) f (Term g)

instance {-# OVERLAPPABLE #-} (SubstLit f h, SubstLit g h) => SubstLit (f :+: g) h where
    substLitAlg = caseF substLitAlg substLitAlg

instance {-# OVERLAPPABLE #-} (Traversable f, Functor g, f :<: g) => SubstLit f g where
    substLitAlg (fs :: f (Term g)) = return $ Term . inj $ fs

-- The specialized instance for BooleanLit
instance (Functor g) => SubstLit BooleanLit g where
    substLitAlg :: forall m. Monad m => AlgM (LitSubstM g m) BooleanLit (Term g)
    substLitAlg (fs :: BooleanLit (Term g)) = do
            (Subst hom :: Subst m BooleanLit g) <- ask
            (gtg :: Context g (Term g)) <- lift $ hom fs
            return $ appCxt gtg

-- | Monadic literal substitution catamorphism.
substLitM :: forall m f g. (Monad m, SubstLit f g)
    => HomM m BooleanLit g -> Term f -> m (Term g)
substLitM hom f = runReaderT (cataM substLitAlg f) (Subst hom)

{-----------------------------------------------------------------------------}
-- Substitutions for BooleanLitI

-- | A Monad used to run the substitution of literals.
type LitISubstM g m = ReaderT (Subst m BooleanLitI g) m

class (Traversable f, Functor g) => SubstLitI f g where
    substLitIAlg :: Monad m => AlgM (LitISubstM g m) f (Term g)

instance {-# OVERLAPPABLE #-} (SubstLitI f h, SubstLitI g h) => SubstLitI (f :+: g) h where
    substLitIAlg = caseF substLitIAlg substLitIAlg

instance {-# OVERLAPPABLE #-} (Traversable f, Functor g, f :<: g) => SubstLitI f g where
    substLitIAlg (fs :: f (Term g)) = return $ Term . inj $ fs

-- The specialized instance for BooleanLit
instance (Functor g) => SubstLitI BooleanLitI g where
    substLitIAlg :: forall m. Monad m => AlgM (LitISubstM g m) BooleanLitI (Term g)
    substLitIAlg (fs :: BooleanLitI (Term g)) = do
            (Subst hom :: Subst m BooleanLitI g) <- ask
            (gtg :: Context g (Term g)) <- lift $ hom fs
            return $ appCxt gtg

-- | Monadic literal substitution catamorphism.
substLitIM :: forall m f g. (Monad m, SubstLitI f g)
    => HomM m BooleanLitI g -> Term f -> m (Term g)
substLitIM hom f = runReaderT (cataM substLitIAlg f) (Subst hom)

{-----------------------------------------------------------------------------}
-- Substitute literals with numbers and back

-- substLitM :: forall m f g. (Monad m, SubstLit f g)
--     => HomM m BooleanLit g -> Term f -> m (Term g)
-- substLitM hom f = runReaderT (cataM substLitAlg f) (Subst hom)

type NameMap = HashMap Int String

buildNameMaps :: forall f. (Foldable f, BooleanLit :<: f)
    => Term f -> (NameMap, HashMap String Int)
buildNameMaps term = let
    indexed :: [(Int, String)]
    indexed = (zip [1..] . HashSet.toList . literalNamesDefault) term
    in (fromList indexed, fromList $ fmap swap indexed)

numberLiterals :: forall f g. (SubstLit f g, BooleanLit :<: f, BooleanLitI :<: g)
    => Term f -> (NameMap, Term g)
numberLiterals term = let
    (nameMap, repMap) = buildNameMaps term

    -- hom :: BooleanLit a -> Identity (Context g a)
    hom :: HomM Identity BooleanLit g
    hom (BooleanLit sign name) = let
        number :: Int
        number = bool (-1) 1 sign * (repMap !! name)    -- has to exists, we just built it
        in return $ iBooleanLitI number

    in (nameMap, runIdentity $ substLitM hom term)

nameLiterals :: forall f g. (SubstLitI f g, BooleanLitI :<: f, BooleanLit :<: g)
    => NameMap -> Term f -> Term g
nameLiterals nameMap term = let
    -- HomM m BooleanLitI g -> Term f -> m (Term g)
    --hom :: HomM Identity BooleanLitI g
    hom :: BooleanLitI a -> Identity (Context g a)
    hom (BooleanLitI number) = let
        sign :: Bool
        sign = number > 0
        name :: String
        name = nameMap !! abs number
        in return $ iBooleanLit sign name
    in runIdentity $ substLitIM hom term

{-----------------------------------------------------------------------------}
-- Idea: compare terms for α-Equivalence
