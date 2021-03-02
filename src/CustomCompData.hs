
{-# LANGUAGE UndecidableInstances   #-}

{- Compdata version with multi-param kinds:
    Given f :: Type -> Type -> Type and g: Type -> Type ->
    we get (f :+: g) :: Type -> Type -> Type
    with (f :+: g) v behaving like (f v :+: g v)
-}

module CustomCompData where

import Data.Kind (Type)

import Control.Applicative hiding (Const)
import Control.Monad hiding (mapM, sequence)

import Data.Foldable
import Data.Traversable
--import Unsafe.Coerce

-- Some settings for HLint
{-# ANN module "HLint: ignore Use fmap" #-}
{-# ANN module "HLint: ignore Use <$>" #-}

{-----------------------------------------------------------------------------}
-- SubsumeCommon

data Pos = Here | Le Pos | Ri Pos | Sum Pos Pos
data Emb = Found Pos | NotFound | Ambiguous
data Proxy a = P    -- FIXME use Proxy from Base

type family Choose (e1 :: Emb) (r :: Emb) :: Emb where
    Choose (Found x) (Found y) = Ambiguous
    Choose Ambiguous y = Ambiguous
    Choose x Ambiguous = Ambiguous
    Choose (Found x) y = Found (Le x)
    Choose x (Found y) = Found (Ri y)
    Choose x y = NotFound

type family Sum' (e1 :: Emb) (r :: Emb) :: Emb where
    Sum' (Found x) (Found y) = Found (Sum x y)
    Sum' Ambiguous y = Ambiguous
    Sum' x Ambiguous = Ambiguous
    Sum' NotFound y = NotFound
    Sum' x NotFound = NotFound

type family ComprPos (p :: Pos) :: Pos where
    ComprPos Here = Here
    ComprPos (Le p) = Le (ComprPos p)
    ComprPos (Ri p) = Ri (ComprPos p)
    ComprPos (Sum l r) = CombineRec (ComprPos l) (ComprPos r)

type family CombineRec l r where
    CombineRec l r = CombineMaybe (Sum l r) (Combine l r)

type family CombineMaybe (p :: Pos) (p' :: Maybe Pos) where
    CombineMaybe p (Just p') = p'
    CombineMaybe p p'        = p

type family Combine (l :: Pos) (r :: Pos) :: Maybe Pos where
    Combine (Le l) (Le r) = Le' (Combine l r)
    Combine (Ri l) (Ri r) = Ri' (Combine l r)
    Combine (Le Here) (Ri Here) = Just Here
    Combine l r = Nothing

-- | 'Ri' lifted to 'Maybe'.
type family Ri' (p :: Maybe Pos) :: Maybe Pos where
    Ri' Nothing = Nothing
    Ri' (Just p) = Just (Ri p)

-- | 'Le' lifted to 'Maybe'.
type family Le' (p :: Maybe Pos) :: Maybe Pos where
    Le' Nothing = Nothing
    Le' (Just p) = Just (Le p)

type family ComprEmb (e :: Emb) :: Emb where
    ComprEmb (Found p) = Check (Dupl p) (ComprPos p)
    ComprEmb e = e

-- | Helper type family for 'ComprEmb'.
type family Check (b :: Bool) (p :: Pos) where
    Check False p = Found p
    Check True  p = Ambiguous

-- | This type family turns a list of /atomic position/ into a list of
-- /simple positions/ by recursively splitting each position of the
-- form @Sum p1 p2@ into @p1@ and @p2@.
type family ToList (s :: [Pos]) :: [Pos] where
    ToList (Sum p1 p2 ': s) = ToList (p1 ': p2 ': s)
    ToList (p ': s) = p ': ToList s
    ToList '[] = '[]

-- | This type checks whether the argument (atomic) position has
-- duplicates.
type Dupl s = Dupl' (ToList '[s])

-- | This type family checks whether the list of positions given as an
-- argument contains any duplicates.
type family Dupl' (s :: [Pos]) :: Bool where
    Dupl' (p ': r) = OrDupl' (Find p r) r
    Dupl' '[] = False

-- | This type family checks whether its first argument is contained
-- its second argument.
type family Find (p :: Pos) (s :: [Pos]) :: Bool where
    Find p (p ': r)  = True
    Find p (p' ': r) = Find p r
    Find p '[] = False

-- | This type family returns @True@ if the first argument is true;
-- otherwise it checks the second argument for duplicates.
type family OrDupl' (a :: Bool) (b :: [Pos]) :: Bool where
    OrDupl'  True  c  = True
    OrDupl'  False c  = Dupl' c

{-----------------------------------------------------------------------------}
-- Term

data Cxt :: Type -> (Type -> Type -> Type) -> Type -> Type -> Type where
    Term ::  f v (Cxt h f v a) -> Cxt h f v a
    Hole :: a -> Cxt Hole f v a

-- | Phantom type that signals that a 'Cxt' might contain holes.
data Hole
-- | Phantom type that signals that a 'Cxt' does not contain holes.
data NoHole

type Context = Cxt Hole

-- | A term is a context with no holes.
type Term f v = Cxt NoHole f v ()

-- | This function unravels the given term at the topmost layer.
unTerm :: Term f v -> f v (Term f v)
unTerm (Term t) = t

{-| Convert a functorial value into a context.  -}
simpCxt :: Functor (f v) => f v a -> Context f v a
{-# INLINE simpCxt #-}
simpCxt = Term . fmap Hole

instance Functor (f v) => Functor (Cxt h f v) where
    fmap :: (a -> b) -> Cxt h f v a -> Cxt h f v b
    fmap f = run
        where run (Hole v) = Hole (f v)
              run (Term t) = Term (fmap run t)

instance Functor (f v) => Applicative (Context f v) where
    pure = Hole
    (<*>) = ap

instance Functor (f v) => Monad (Context f v) where
    return = Hole
    m >>= f = run m
        where run (Hole v) = f v
              run (Term t) = Term (fmap run t)

instance Foldable (f v) => Foldable (Cxt h f v) where
    foldr op c a = run a c
        where run (Hole a) e = a `op` e
              run (Term t) e = foldr run e t

    foldl op = run
        where run e (Hole a) = e `op` a
              run e (Term t) = foldl run e t

    fold (Hole a) = a
    fold (Term t) = foldMap fold t

    foldMap f = run
        where run (Hole a) = f a
              run (Term t) = foldMap run t

instance Traversable (f v) => Traversable (Cxt h f v) where
    traverse f = run
        where run (Hole a) = Hole <$> f a
              run (Term t) = Term <$> traverse run t

    sequenceA (Hole a) = Hole <$> a
    sequenceA (Term t) = Term <$> traverse sequenceA t

    mapM f = run
        where run (Hole a) = liftM Hole $ f a
              run (Term t) = liftM Term $ mapM run t

    sequence (Hole a) = liftM Hole a
    sequence (Term t) = liftM Term $ mapM sequence t

{-----------------------------------------------------------------------------}
-- Ops

-- Sums
infixr 6 :+:

-- |Formal sum of signatures (functors).
data (f :+: g) v e = Inl (f v e)
                 | Inr (g v e)

{-| Utility function to case on a functor sum, without exposing the internal
  representation of sums. -}
caseF :: (f v a -> b) -> (g v a -> b) -> (f :+: g) v a -> b
{-# INLINE caseF #-}
caseF f g x = case x of
                Inl x -> f x
                Inr x -> g x

fromInl :: (f :+: g) v e -> Maybe (f v e)
fromInl = caseF Just (const Nothing)

fromInr :: (f :+: g) v e -> Maybe (g v e)
fromInr = caseF (const Nothing) Just

instance (Functor (f v), Functor (g v)) => Functor ((f :+: g) v) where
    fmap f (Inl e) = Inl (fmap f e)
    fmap f (Inr e) = Inr (fmap f e)

instance (Foldable (f v), Foldable (g v)) => Foldable ((f :+: g) v) where
    fold (Inl e) = fold e
    fold (Inr e) = fold e
    foldMap f (Inl e) = foldMap f e
    foldMap f (Inr e) = foldMap f e
    foldr f b (Inl e) = foldr f b e
    foldr f b (Inr e) = foldr f b e
    foldl f b (Inl e) = foldl f b e
    foldl f b (Inr e) = foldl f b e
    foldr1 f (Inl e) = foldr1 f e
    foldr1 f (Inr e) = foldr1 f e
    foldl1 f (Inl e) = foldl1 f e
    foldl1 f (Inr e) = foldl1 f e

instance (Traversable(f v), Traversable (g v)) => Traversable ((f :+: g) v) where
    traverse f (Inl e) = Inl <$> traverse f e
    traverse f (Inr e) = Inr <$> traverse f e
    sequenceA (Inl e) = Inl <$> sequenceA e
    sequenceA (Inr e) = Inr <$> sequenceA e
    mapM f (Inl e) = Inl `liftM` mapM f e
    mapM f (Inr e) = Inr `liftM` mapM f e
    sequence (Inl e) = Inl `liftM` sequence e
    sequence (Inr e) = Inr `liftM` sequence e

type family Elem (f :: Type -> Type -> Type) (g :: Type -> Type -> Type) :: Emb where
    Elem f f = Found Here
    Elem (f1 :+: f2) g =  Sum' (Elem f1 g) (Elem f2 g)
    Elem f (g1 :+: g2) = Choose (Elem f g1) (Elem f g2)
    Elem f g = NotFound

class Subsume (e :: Emb) (f :: Type -> Type -> Type) (g :: Type -> Type -> Type) where
  inj'  :: Proxy e -> f v a -> g v a
  prj'  :: Proxy e -> g v a -> Maybe (f v a)

instance Subsume (Found Here) f f where
    inj' _ = id
    prj' _ = Just

instance Subsume (Found p) f g => Subsume (Found (Le p)) f (g :+: g') where
    inj' _ = Inl . inj' (P :: Proxy (Found p))
    prj' _ (Inl x) = prj' (P :: Proxy (Found p)) x
    prj' _ _       = Nothing

instance Subsume (Found p) f g => Subsume (Found (Ri p)) f (g' :+: g) where
    inj' _ = Inr . inj' (P :: Proxy (Found p))
    prj' _ (Inr x) = prj' (P :: Proxy (Found p)) x
    prj' _ _       = Nothing

instance (Subsume (Found p1) f1 g, Subsume (Found p2) f2 g)
    => Subsume (Found (Sum p1 p2)) (f1 :+: f2) g where
    inj' _ (Inl x) = inj' (P :: Proxy (Found p1)) x
    inj' _ (Inr x) = inj' (P :: Proxy (Found p2)) x

    prj' _ x = case prj' (P :: Proxy (Found p1)) x of
                 Just y -> Just (Inl y)
                 _      -> case prj' (P :: Proxy (Found p2)) x of
                             Just y -> Just (Inr y)
                             _      -> Nothing

infixl 5 :<:

type f :<: g = (Subsume (ComprEmb (Elem f g)) f g)
inj :: forall f g v a . (f :<: g) => f v a -> g v a
inj = inj' (P :: Proxy (ComprEmb (Elem f g)))

proj :: forall f g v a . (f :<: g) => g v a -> Maybe (f v a)
proj = prj' (P :: Proxy (ComprEmb (Elem f g)))

infixl 5 :=:

type f :=: g = (f :<: g, g :<: f)

{-----------------------------------------------------------------------------}
-- Algebra

type Alg f v a = f v a -> a

free :: forall f v h a b . (Functor (f v)) => Alg f v b -> (a -> b) -> Cxt h f v a -> b
free f g = run
    where run :: Cxt h f v a -> b
          run (Hole x) = g x
          run (Term t) = f (fmap run t)

cata :: forall f v a . (Functor (f v)) => Alg f v a -> Term f v -> a
cata f = run
    where run :: Term f v -> a
          run  = f . fmap run . unTerm

cata' :: forall f v h a. (Functor (f v)) => Alg f v a -> Cxt h f v a -> a
cata' f = free f id

appCxt :: (Functor (f v)) => Context f v (Cxt h f v a) -> Cxt h f v a
-- appCxt = cata' Term
appCxt (Hole x) = x
appCxt (Term t) = Term (fmap appCxt t)

{-| This type represents a context function. -}
type CxtFun f g v = forall a h. Cxt h f v a -> Cxt h g v a

{-| This type represents a signature function.-}
type SigFun f g v = forall a. f v a -> g v a

{-| This type represents a term homomorphism. -}
type Hom f g v = SigFun f (Context g) v

appHom :: forall f g v. (Functor (f v), Functor (g v)) => Hom f g v -> CxtFun f g v
appHom f = run where
    run :: CxtFun f g v
    run (Hole x) = Hole x
    run (Term t) = appCxt (f (fmap run t))

appHom' :: forall f g v. (Functor (g v)) => Hom f g v -> CxtFun f g v
appHom' f = run where
    run :: CxtFun f g v
    run (Hole x) = Hole x
    run (Term t) = appCxt  (fmap run (f t))

compHom :: (Functor (g v), Functor (h v)) => Hom g h v -> Hom f g v -> Hom f h v
compHom f g = appHom f . g

compAlg :: (Functor (g v)) => Alg g v a -> Hom f g v -> Alg f v a
compAlg alg talg = cata' alg . talg

appSigFun :: (Functor (f v)) => SigFun f g v -> CxtFun f g v
appSigFun f = run
    where run (Term t) = Term $ f $ fmap run t
          run (Hole x) = Hole x

appSigFun' :: (Functor (g v)) => SigFun f g v -> CxtFun f g v
appSigFun' f = run
    where run (Term t) = Term $ fmap run  $ f t
          run (Hole x) = Hole x

{-| This function composes two signature functions. -}
compSigFun :: SigFun g h v -> SigFun f g v -> SigFun f h v
compSigFun f g = f . g

compSigFunHom :: (Functor (g v)) => SigFun g h v -> Hom f g v -> Hom f h v
compSigFunHom f g = appSigFun f . g

compHomSigFun :: Hom g h v -> SigFun f g v -> Hom f h v
compHomSigFun f g = f . g

compAlgSigFun :: Alg g v a -> SigFun f g v -> Alg f v a
compAlgSigFun f g = f . g

hom :: (Functor (g v)) => SigFun f g v -> Hom f g v
hom f = simpCxt . f

{-----------------------------------------------------------------------------}
-- Coalgebra

type Coalg f v a = a -> f v a

type CVCoalg f v a = a -> f v (Context f v a)

ana :: forall a f v. Functor (f v) => Coalg f v a -> a -> Term f v
ana f = run
    where run :: a -> Term f v
          run t = Term $ fmap run (f t)

ana' :: forall a f v. Functor (f v) => Coalg f v a -> a -> Term f v
ana' f t = build (run t)
    where run :: forall b . a -> Alg f v b -> b
          run t con = run' t where
              run' :: a ->  b
              run' t = con $ fmap run' (f t)
{-# INLINE [2] ana' #-}
build :: (forall a. Alg f v a -> a) -> Term f v
{-# INLINE [1] build #-}
build g = g Term

{-----------------------------------------------------------------------------}
-- Show

class ShowF f where
    showF :: f String -> String

-- $(derive [liftSum] [''ShowF])
instance (ShowF (f v), ShowF (g v)) => ShowF ((:+:) f g v) where
      showF t = caseF showF showF t

-- Gives rise to show via catamorphism
instance (Functor (f v), ShowF (f v), Show a) => Show (Cxt h f v a) where
    show = free showF show

---- Instances ----------------------------------------------------------------

instance (Functor (f v), ShowF (f v)) => ShowF (Cxt h f v) where
    showF (Hole s) = s
    showF (Term t) = showF $ fmap showF t

-- instance (ShowF f, Show p) => ShowF (f :&: p) where
--     showF (v :&: p) = showF v ++ " :&: " ++ show p

-- $(derive [makeShowF] [''Maybe, ''[], ''(,)])

{-----------------------------------------------------------------------------}
-- Sum

-- |Inject a term where the outermost layer is a sub signature. If the signature
-- @g@ is compound of /n/ atomic signatures, use @inject@/n/ instead.
inject :: (g :<: f) => g v (Cxt h f v a) -> Cxt h f v a
inject = inject_ inj

-- |Inject a term where the outermost layer is a sub signature. If the signature
-- @g@ is compound of /n/ atomic signatures, use @inject@/n/ instead.
inject_ :: SigFun g f v -> g v (Cxt h f v a) -> Cxt h f v a
inject_ f = Term . f

{-----------------------------------------------------------------------------}
--
