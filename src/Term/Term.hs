
{-# LANGUAGE PatternSynonyms #-}

{- |
Description     : General term represenation
Stability       : experimental

These terms do /not/ support variable binders (like lambdas) - but they
can compose arbitrary unary and binary operators, variable names and values.

-}
module Term.Term
    (   -- * General terms
        Term
    ,   Op(..)

    ,   -- * Recursion-schemes
        Fix4(..)
    ,   TermF(..)

    ,   -- * Pattern synonyms
        pattern Val
    ,   pattern Var
    ,   pattern Rec
    ,   pattern BUOp
    ,   pattern BBOp
    ,   pattern BFlOp

    ,    -- * Dirty details
        ProperRecT(..)
    ,   ProperOpTag(..)
    ,   Associativity(..)
    ,   isLeftAssociative
    ,   isRightAssociative
    ) where

import Data.Kind (Type)
import Data.Void
import Data.Functor.Classes
import Data.Traversable (foldMapDefault)
import Control.Monad (ap)
import Text.Show (showListWith)

-- recursion-schemes
import Data.Functor.Foldable

import Missing.Void

{-----------------------------------------------------------------------------}
-- Terms, part 0: Fixpoint

-- | Fixpoint type for 'Functor's of 4rth order.
newtype Fix4 f a b c = Fix4 { unFix4 :: f a b c (Fix4 f a b c) }

{- 
TODO: Fix4 can actually even be an instance of Show2, but we would need
a new @Show3@ instance for a constraint @Show3 (f a)@, which is not part of
@Data.Functor.Classes@ in base.
-}
instance Show2 (f a b) => Show1 (Fix4 f a b) where
    liftShowsPrec :: forall c. (Int -> c ->  ShowS) -> ([c] -> ShowS)
        -> Int -> Fix4 f a b c -> ShowS
    liftShowsPrec showC showListC = go where
        go :: Int -> Fix4 f a b c -> ShowS
        go d (Fix4 f) = liftShowsPrec2 showC showListC go (showListWith (go 0)) d f

instance Show1 (f a b c) => Show (Fix4 f a b c) where
    showsPrec :: Int -> Fix4 f a b c -> ShowS
    showsPrec d (Fix4 f) = liftShowsPrec showsPrec showList d f

instance Eq1 (f a b c) => Eq (Fix4 f a b c) where
    (==) (Fix4 fa) (Fix4 fb) = eq1 fa fb

-- Recursion-schemes base functor
type instance Base (Fix4 f a b c) = f a b c

instance Functor (f a b c) => Recursive (Fix4 f a b c) where
    project = unFix4

instance Functor (f a b c) => Corecursive (Fix4 f a b c) where
    embed = Fix4

-- IsoVoid
instance (Functor (f a b c), IsoVoid (f a b c Void)) => IsoVoid (Fix4 f a b c) where
    absurd' (Fix4 f) = absurd' $ fmap (absurd' :: Fix4 f a b c -> Void) f


{-----------------------------------------------------------------------------}
-- Terms, part 1: Term structure

{- TODO: This creates existential types, which might cause problems later.
Solve this by removing the class alltogether and augmenting all the
type signatures.
-}

-- | A class for proper operator implementations
class (Show1 op, Eq1 op, Functor op, Foldable op, Traversable op) => ProperRecT op where

{- | Base functor for the general term representation.

This type is usually only encountered when using the recursion
functions from 'recursion-schemes'.
-}
data TermF
    :: (Type -> Type) -> Type -> Type       -- Type parameters
    -> Type                                 -- Recursive term
    -> Type
    where
    ConstT      ::                  val ->      TermF op val var rec
    VariableT   ::                  var ->      TermF op val var rec
    RecT        :: ProperRecT op => op rec ->   TermF op val var rec

deriving instance (Eq (op r), Eq val, Eq var) => Eq (TermF op val var r)
deriving instance Functor (TermF op val var)

-- Note: Our show instance will print the expression in forms of patterns
-- This is contrary to what show usually does, but much easier to use
instance Show val => Show2 (TermF op val) where
    liftShowsPrec2 :: forall var a.
           (Int -> var -> ShowS) -> ([var] -> ShowS)
        -> (Int -> a -> ShowS) -> ([a] -> ShowS)
        -> Int -> TermF op val var a -> ShowS
    liftShowsPrec2 showVar _ showA showAL = go where
        prec :: Int
        prec = 10   -- same precedence for everyone
        go :: Int -> TermF op val var a -> ShowS
        go d = \case
            ConstT v    -> showParen (d > prec) $
                showString "Val " . shows v
            VariableT v -> showParen (d > prec) $
                showString "Var " . showVar d v
            RecT v      -> liftShowsPrec showA showAL d v

instance (Show val, Show var) => Show1 (TermF op val var) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Eq val, Eq var) => Eq1 (TermF op val var) where
    liftEq _ (ConstT a) (ConstT b) = a == b
    liftEq _ (VariableT a) (VariableT b) = a == b
    liftEq f (RecT ra) (RecT rb) = liftEq f ra rb
    liftEq _ _ _ = False

instance Foldable (TermF op val var) where
   foldMap = foldMapDefault

instance Traversable (TermF op val var) where
    traverse :: Applicative f => (a -> f b) -> TermF op val var a -> f (TermF op val var b)
    traverse f = \case
        (ConstT v)      -> pure (ConstT v)
        (VariableT v)   -> pure (VariableT v)
        (RecT op)       -> RecT <$> traverse f op

{- Terms without values or variables don't exist, iff
    op doesn't hold any constants.
-}
instance IsoVoid (op Void) => IsoVoid (TermF op Void Void Void) where
    absurd' :: TermF op Void Void Void -> b
    absurd' (ConstT v) = absurd v
    absurd' (VariableT v) = absurd v
    absurd' (RecT op) = absurd' op

{-----------------------------------------------------------------------------}

{- | General term representation.

A @Term op val var@ has operators of type @op@, holds values of type @val@
and variables of type @var@. Any of those tags can be set to 'Void' to
disable the corresponding variant, e.g. @Term op Void var@ hold no constant
values, but may hold variables and operators.
-}
type Term = Fix4 TermF

instance Foldable (Term op val) where
   foldMap = foldMapDefault

instance Traversable (Term op val) where
    traverse :: Applicative f => (a -> f b) -> Term op val a -> f (Term op val b)
    traverse f = \case
        Fix4 (ConstT v)     -> pure $ embed $ ConstT v
        Fix4 (VariableT v)  -> embed . VariableT <$> f v
        Fix4 (RecT v)       -> embed . RecT <$> traverse (traverse f) v


{-----------------------------------------------------------------------------}
-- Functor, Applicative and Monad on Variables

instance Functor (Term op val) where
    fmap :: (a -> b) -> Term op val a -> Term op val b
    fmap f (Fix4 term) = case term of
        ConstT x    -> Fix4 $ ConstT x
        VariableT v -> Fix4 $ VariableT (f v)
        RecT op     -> Fix4 $ RecT $ (fmap . fmap) f op

instance Applicative (Term op val) where
    pure = return
    (<*>) = ap

instance Monad (Term op val) where
    return :: a -> Term op val a
    return = Fix4 . VariableT
    (>>=) :: Term op val a -> (a -> Term op val b) -> Term op val b
    Fix4 (VariableT v)  >>= f   = f v
    Fix4 (ConstT x)     >>= _   = Fix4 $ ConstT x
    Fix4 (RecT op)      >>= f   = Fix4 $ RecT $ fmap (>>= f) op

{-----------------------------------------------------------------------------}
-- Terms, part 2: Operators

{- | General operator representation.

A @Op uop bop flop@ has /unary/ operators of type @uop@, binary operators of
type @bop@ and flattened (arbitrary arity) operators of type @flop@.

Similarly to 'Term', any of those tags can be set to 'Void' to
disable the corresponding variant.
-}
data Op
    :: Type -> Type -> Type -> Type -> Type
    where
    UnaryOp     :: ProperOpTag uop =>   uop  -> rec ->         Op uop bop flop rec
    BinaryOp    :: ProperOpTag bop =>   bop  -> rec -> rec ->  Op uop bop flop rec
    FlatOp      :: ProperOpTag flop =>  flop -> [rec] ->       Op uop bop flop rec

deriving instance Functor (Op uop bop flop)
deriving instance Foldable (Op uop bop flop)
deriving instance Traversable (Op uop bop flop)

{-----------------------------------------------------------------------------}

-- | Binary operator associativity
data Associativity
    = NonAssociative
    | LeftAssociative
    | RightAssociative
    | FullyAssociative

isLeftAssociative :: Associativity -> Bool
isLeftAssociative LeftAssociative = True
isLeftAssociative FullyAssociative = True
isLeftAssociative _ = False

isRightAssociative :: Associativity -> Bool
isRightAssociative RightAssociative = True
isRightAssociative FullyAssociative = True
isRightAssociative _ = False

class (Show o, Eq o, Ord o) => ProperOpTag o where
    opPrec :: o -> Int
    opAssoc :: o -> Associativity
    opAssoc _ = NonAssociative
    opName :: o -> String
    opName = show

instance ProperOpTag Void where
    opPrec = absurd

instance (ProperOpTag a, ProperOpTag b, ProperOpTag c) => ProperRecT (Op a b c)
{-----------------------------------------------------------------------------}

instance Show1 (Op uop bop flop) where
    liftShowsPrec :: forall a. (Int -> a -> ShowS) -> ([a] -> ShowS)
        -> Int -> Op uop bop flop a -> ShowS
    liftShowsPrec showsRec _ d = let
        prec :: Int
        prec = 10   -- same precedence for everyone in show
        in \case
        UnaryOp o t     -> showParen (d > prec) $
            showString (opName o ++ " ") . showsRec (prec+1) t
        BinaryOp o a b  -> showParen (d > prec) $
            showsRec (prec+1) a . showString (" `" ++ opName o ++ "` ") . showsRec (prec+1) b
        FlatOp o xs     -> showParen (d > prec) $
            showString (opName o) .
            showListWith (showsRec 0) xs

instance Eq1 (Op a b c) where
    liftEq :: (x -> x' -> Bool) -> Op a b c x -> Op a b c x' -> Bool
    liftEq f (UnaryOp o x) (UnaryOp o' x') = (o == o') && f x x'
    liftEq f (BinaryOp o a b) (BinaryOp o' a' b')
        = (o == o') && f a a' && f b b'
    liftEq f (FlatOp o xs) (FlatOp o' xs')
        = (o == o') && liftEq f xs xs'
    liftEq _ _ _ = False

instance IsoVoid (Op uop bop Void Void) where
    absurd' (UnaryOp _ r) = absurd r
    absurd' (BinaryOp _ a _) = absurd a     -- which Void would you like? the left one?
    absurd' (FlatOp op _) = absurd op

{-----------------------------------------------------------------------------}
-- And a few pattern synonyms for brevity

pattern Val :: forall (op :: Type -> Type) val var.
    val -> Term op val var
pattern Var :: forall (op :: Type -> Type) val var.
    var -> Term op val var
pattern Rec :: forall (op :: Type -> Type) val var.
    () => ProperRecT op
    => op (Term op val var) -> Term op val var

pattern BUOp
    :: () => (ProperRecT (Op uop bop flop), ProperOpTag uop)
    => uop
    -> Term (Op uop bop flop) val var
    -> Term (Op uop bop flop) val var
pattern BBOp
    :: () => (ProperRecT (Op uop bop flop), ProperOpTag bop)
    => bop
    -> Term (Op uop bop flop) val var
    -> Term (Op uop bop flop) val var
    -> Term (Op uop bop flop) val var
pattern BFlOp
    :: () => (ProperRecT (Op uop bop flop), ProperOpTag flop)
    => flop
    -> [Term (Op uop bop flop) val var]
    -> Term (Op uop bop flop) val var

pattern Val v       = Fix4 (ConstT v)
pattern Var v       = Fix4 (VariableT v)
pattern Rec v       = Fix4 (RecT v)

pattern BUOp o a    = Fix4 (RecT (UnaryOp o a))
pattern BBOp o a b  = Fix4 (RecT (BinaryOp o a b))
pattern BFlOp o xs  = Fix4 (RecT (FlatOp o xs))

{-# COMPLETE Var, Val, Rec #-}
{-# COMPLETE Var, Val, BUOp, BBOp, BFlOp #-}

{-----------------------------------------------------------------------------}
-- Utilities

{- TODO: quickCheck compatibility / shrinkTerm

Provide helpers to implement @Arbitrary@ instances for terms. This should
probably be in a support module.

shrinkTerm: We have to use the quickcheck class for recursion into values. This
means we need a full Arbitrary instance. So first clean up the instances from
BooleanAlgebra.Support.Arbitrary, put them here and then implement generic
shrinking.

-- Produce smaller subsets of a term.

Useful for generating tests, e.g. using QuickCheck's @Arbitrary@.

shrinkTerm :: Term (Op uop bop flop) val var -> [Term (Op uop bop flop) val var]
shrinkTerm (BUOp op t) = t : [BUOp op t' | t' <- shrinkTerm t]
shrinkTerm (BBOp op a b) = a : b : [BBOp op a' b' | (a', b') <- shrinkTerm (a, b)]
shrinkTerm _ = []

-}
