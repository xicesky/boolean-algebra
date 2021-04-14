
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Description     : Pretty-printing for terms
Stability       : experimental

-}
module Term.Prettyprinter
    (   -- * Pretty printing
        PrettyTerm(..)
    ,   PrettyUnaryOp(..)
    ,   PrettyBinaryOp(..)
    ,   PrettyFlatOp(..)

    ,   -- * Options
        PrettyOptions(..)
    ,   defaultPrettyOptions

    ,   -- * Utilities
        Precedence
    ,   PrettyTerm1(..)
    ,   diag'
    ,   aList
    ,   parensP
    ,   backticks
    ,   backtick

    ,   -- * Re-exports
        Diag

    ) where

import Data.Void (Void)
import Data.String (IsString(..))
import Data.Bool (bool)

import Prettyprinter
-- For diagnosis - showing internal pretty-printer state
import Prettyprinter.Internal.Debug (diag, Diag)
import Missing.Prettyprinter

import Term.Term

{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}

{-----------------------------------------------------------------------------}
-- Main interface

-- TODO: This is not extensible, replace with dynamic option.
-- Dumb idea: Can we have open product types with an open "default value"? :)

-- | Options for the pretty-printer
data PrettyOptions = PrettyOptions
    {   produceValidHaskell :: Bool
        -- ^ 'True': produce valid Haskell expressions
        -- 'False': produce terser, human-readable output
    }

{- | Default options for the pretty-printer

Used when omitting options, e.g. by using 'pretty' on 'Term'
-}
defaultPrettyOptions :: PrettyOptions
defaultPrettyOptions = PrettyOptions True

{- | Class of pretty-printable terms
-}
class PrettyTerm t where
    {- | Pretty-print a term with specific options and precedence
    -}
    prettyTerm :: PrettyOptions -> Precedence -> t -> Doc ann
    default prettyTerm :: Show t =>
        PrettyOptions -> Precedence -> t -> Doc ann
    prettyTerm _ _ = viaShow

    {- | Pretty print a variable name

    Override when variables should printed differently than values,
    for example for @String@, when not producing haskell output.
    -}
    prettyVar :: PrettyOptions -> Precedence -> t -> Doc ann
    prettyVar = prettyTerm

    {- | Pretty-print a term with specific options
    -}
    prettyOpts :: PrettyOptions -> t -> Doc ann
    prettyOpts opts = prettyTerm opts 0

    {- | Default implementation for 'Pretty' instances

    Note: In contrast to 'pretty', this function prefers to produce
    valid haskell expressions.
    -}
    defaultPretty :: t -> Doc ann
    defaultPretty = prettyOpts defaultPrettyOptions
    -- = --

{- | Class of liftable pretty-printable terms.
-}
class PrettyTerm1 f where
    {- | Lifted pretty-printing of @f a@.

    Analogous to the relation between 'liftShow' to 'show', 'liftPrettyTerm1'
    runs the pretty-printer for a type @f a@, given a pretty-printing function
    for the type @a@.
    -}
    liftPrettyTerm1 :: forall a ann.
            (PrettyOptions -> Precedence -> a -> Doc ann)
        -> PrettyOptions -> Precedence -> f a -> Doc ann
    -- = --

{-----------------------------------------------------------------------------}
-- Fixpoint & Term instances

instance PrettyTerm1 (f a b c) => PrettyTerm (Fix4 f a b c) where
    prettyTerm :: forall ann.
        PrettyOptions -> Precedence -> Fix4 f a b c -> Doc ann
    prettyTerm opts d (Fix4 f) =
        liftPrettyTerm1 prettyTerm opts d f

instance (PrettyTerm val, PrettyTerm var, PrettyTerm1 op)
    => PrettyTerm1 (TermF op val var) where

    liftPrettyTerm1 prettyR opts d = \case
        ConstT v        -> parensP d 10 $
            if produceValidHaskell opts
                then fromString "Val" <+> prettyTerm opts d v
                else prettyTerm opts d v
        VariableT v     ->
            if produceValidHaskell opts
                then parensP d 10 $ fromString "Var" <+> prettyVar opts 11 v
                else prettyVar opts d v
        RecT v          -> liftPrettyTerm1 prettyR opts d v

instance (PrettyUnaryOp uop, PrettyBinaryOp bop, PrettyFlatOp flop)
    => PrettyTerm1 (Op uop bop flop) where
    liftPrettyTerm1 prettyR opts d = \case
        UnaryOp o t     -> parensP d (opPrec o) $
            prettyUnaryOp (prettyR opts) opts d o t
        BinaryOp o a b  -> parensP d (opPrec o) $
            prettyBinaryOp (prettyR opts) opts d o a b
        FlatOp o xs     -> parensP d (opPrec o) $ group $
            prettyFlatOp (prettyR opts) opts d o xs

instance (PrettyTerm val, PrettyTerm var, PrettyTerm1 op)
    => Pretty (Term op val var) where
    pretty = defaultPretty

{-----------------------------------------------------------------------------}
-- Operators

{- | Pretty-printing of unary operators
-}
class ProperOpTag o => PrettyUnaryOp o where
    {- | Pretty-print @o t@ for the unary operator @o@.

    The default implementation prints the operator as a prefix using
    'prettyPrefix'.
    -}
    prettyUnaryOp :: (Precedence -> t -> Doc ann)
        -> PrettyOptions -> Precedence -> o -> t -> Doc ann
    prettyUnaryOp prettyR opts d op arg = let
        {- This is a pretty dumb but effective heuristic to determine
            if "unicodePrefix" is actually implemented.
        -}
        needSpace = produceValidHaskell opts
            || show (haskellPrefix op) == show (unicodePrefix op)
        apnd = if needSpace then (<+>) else (<>)
        in prettyPrefix opts op `apnd` prettyR (opPrec op + 1) arg

    -- | Produce the prefix representation of the operator
    prettyPrefix :: PrettyOptions -> o -> Doc ann
    prettyPrefix opts op =
        if produceValidHaskell opts
            then haskellPrefix op
            else unicodePrefix op

    -- | Show the prefix operator as valid haskell expression
    haskellPrefix :: o -> Doc ann
    haskellPrefix op = fromString (opName op)

    -- | Show the prefix operator as terse unicode
    unicodePrefix :: o -> Doc ann
    unicodePrefix = haskellPrefix

{- | Pretty-printing of binary operators
-}
class ProperOpTag o => PrettyBinaryOp o where
    {- | Pretty-print @t1 o t2@ for the binary operator @o@.

    The default implementation prints the operator as a infix using
    'prettyInfix'.
    -}
    prettyBinaryOp :: (Precedence -> t -> Doc ann)
        -> PrettyOptions -> Precedence -> o -> t -> t -> Doc ann
    prettyBinaryOp prettyR opts d op l r = let
            prec = opPrec op
            assoc = opAssoc op
            lprec = bool (prec + 1) prec $ isLeftAssociative assoc
            rprec = bool (prec + 1) prec $ isRightAssociative assoc
        in prettyR lprec l
        <+> prettyInfix opts op
        <+> prettyR rprec r

    -- | Produce the infix representation of the operator
    prettyInfix :: PrettyOptions -> o -> Doc ann
    prettyInfix opts op =
        if produceValidHaskell opts
            then haskellInfix op
            else unicodeInfix op

    -- | Show the infix operator as valid haskell expression
    haskellInfix :: o -> Doc ann
    haskellInfix op = backticks (fromString $ opName op)

    -- | Show the infix operator as terse unicode
    unicodeInfix :: o -> Doc ann
    unicodeInfix = haskellInfix

{- | Pretty-printing of flattened operators
-}
class ProperOpTag o => PrettyFlatOp o where
    {- | Pretty-print @o t0 t1 ...@ for the flat operator @o@.

    The default implementation prints the operator followed by a list.
    -}
    prettyFlatOp :: (Precedence -> t -> Doc ann)
        -> PrettyOptions -> Precedence -> o -> [t] -> Doc ann
    prettyFlatOp prettyR opts d op args =
        group $ fromString (opName op)
        <+> aList (prettyR 0) args

{-----------------------------------------------------------------------------}
-- Instances for common variable / value types

instance PrettyTerm String where
    prettyTerm _ _ = viaShow
    prettyVar opts _
        | produceValidHaskell opts = viaShow
        | otherwise = fromString

instance PrettyTerm Void
instance PrettyUnaryOp Void
instance PrettyBinaryOp Void
instance PrettyFlatOp Void
