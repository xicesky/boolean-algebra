
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-}

-- Using template haskell for deriving some instances
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module RecursionSchemes () where

import Text.Show.Deriving -- for deriving Show1
import Data.Functor.Classes -- Argharghargh
import Control.Arrow ((>>>), (<<<))

data Lit
  = StrLit String
  | IntLit Int
  | Ident String
  deriving (Show, Eq)

-- data Expr
--   = Index Expr Expr
--   | Call Expr [Expr]
--   | Unary String Expr
--   | Binary Expr String Expr
--   | Paren Expr
--   | Literal Lit
--   deriving (Show, Eq)

data ExprF e
  = Index e e
  | Call e [e]
  | Unary String e
  | Binary e String e
  | Paren e
  | Literal Lit
  deriving (Show, Eq, Functor)

$(deriveShow1 ''ExprF)

-- Fixpoint
newtype Term f = In { out :: f (Term f) }

-- Eq
instance Eq1 f => Eq (Term f) where
    In a == In b = eq1 a b

-- Show
instance Show1 f => Show (Term f) where
    showsPrec d (In a) =
        showParen (d >= 11)
            $ showString "In "
            . showsPrec1 11 a

type Expr' = Term ExprF

-- Annoying smart constructors
eIndex a b = In $ Index a b
eCall a b = In $ Call a b
eUnary s a = In $ Unary s a
eBinary a s b = In $ Binary a s b
eParen = In . Paren

eStrLit = In . Literal . StrLit
eIntLit = In . Literal . IntLit
eIdent  = In . Literal . Ident

ex :: Expr'
ex = eParen $ eBinary (eParen $ eIdent "a")
     "=" (eBinary (eIntLit 2)  "*" (eParen $ eIdent "x"))

bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp f = f . In . fmap (bottomUp f) . out
-- bottomUp f = 
--     out
--     >>> fmap (bottomUp f)
--     >>> In
--     >>> f

topDown :: Functor a => (Term a -> Term a) -> Term a -> Term a
topDown f = In . fmap (topDown f) . out . f
-- topDown f = 
--     In
--     <<< fmap (bottomUp f)
--     <<< out
--     <<< f

-- "Mystery" function (cata)
cata :: Functor f => (f a -> a) -> Term f -> a
cata f = f . fmap (cata f) . out

bottomUp' :: Functor f => (Term f -> Term f) -> Term f -> Term f
bottomUp' f = cata (f . In)

-- ana is the same for topDown
ana :: Functor f => (a -> f a) -> a -> Term f
ana f = In . fmap (ana f) . f

topDown' :: Functor f => (Term f -> Term f) -> Term f -> Term f
topDown' f = ana (out . f)

-- applyExpr :: (Expr -> Expr) -> Expr -> Expr
-- -- base case: applyExpr is the identity function on constants
-- applyExpr f (Literal i) = Literal i

-- -- recursive cases: apply f to each subexpression
-- applyExpr f (Paren p) = Paren (f p)
-- applyExpr f (Index e i) = Index (f e) (f i)
-- applyExpr f (Call e args) = Call (f e) (map f args)
-- applyExpr f (Unary op arg) = Unary op (f arg)
-- applyExpr f (Binary l op r) = Binary (f l) op (f r)

-- flatten' :: Expr -> Expr
-- flatten' (Paren e) = flatten' e
-- flatten' x = applyExpr flatten' x
flatten' :: Expr' -> Expr'
flatten' = bottomUp' flattenTerm where
    flattenTerm (In (Paren e)) = e
    flattenTerm other = other

-- Example: Simple Tree
data TreeF a r = Node r a r | Leaf
    deriving (Show, Eq, Functor)
type Tree a = Term (TreeF a)

instance Show2 TreeF where
    liftShowsPrec2 sp1 _ sp2 _ _ (Leaf) = showString "Leaf"
    liftShowsPrec2 sp1 _ sp2 _ _ (Node a x b)
        = showString "(Node "
        . sp2 0 a
        . showString ", "
        . sp1 0 x
        . showString ", "
        . sp2 0 b
        . showChar ')'

instance Show a => Show1 (TreeF a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

node a x b = In $ Node a x b
leaf = In Leaf

-- TODO: This should be auto when TreeF is a bifunctor...?
-- Problem: Haskell wouldn't like an instance for a type alias
-- How would one actually write something like:
--      instance Functor (\x -> Term (TreeF x)) where ...
--
-- instance Functor Tree where
--     fmap :: (a -> b) -> Tree a -> Tree b
--     fmap = error "NYI"

traditionalDepth :: Tree a -> Int
traditionalDepth (In (Leaf)) = 0
traditionalDepth (In (Node l _ r)) = 1 + 
    max (traditionalDepth l) (traditionalDepth r)

depth :: Tree a -> Int
depth = cata depthAlg where
    depthAlg Leaf = 0
    depthAlg (Node left _ right) = 1 + max left right

exTree :: Tree Int
exTree = node (node (node leaf 1 leaf) 2 leaf) 3 $ node leaf 4 leaf

-- topDown' is actually kinda wrong, because we wanted a topDown _catamorphism_
-- This has to work something like this

topDownAlt :: forall f i d. Functor f => (f (i -> Term f) -> (i -> Term f)) -> Term f -> i -> Term f
topDownAlt f expr init = cata ff expr init where
    -- f :: Int -> f (Fix f) -> d
    ff :: f (i -> Term f) -> (i -> Term f)    -- i is passed top-down, d is passed bottom up
    ff subf i = f subf i


-- Fetch left subtree at depth i
fetchLeft :: Int -> Tree a -> Tree a
fetchLeft i tree = topDownAlt fetchAlg tree i where
    fetchAlg :: TreeF a (Int -> Tree a) -> Int -> Tree a
    fetchAlg e 0 = In $ fmap ($ 0) e
    -- fetchAlg (Leaf) 0 = leaf
    -- fetchAlg (Node l a r) 0 = node (l 0) a (r 0)
    fetchAlg (Leaf) _ = leaf
    fetchAlg (Node l _ r) i = l (i-1)
