
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

import Term.Term

{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}

{-----------------------------------------------------------------------------}
-- Main interface

-- TODO: This is not extensible, replace with dynamic option.
-- Dumb idea: Can we have open product types with an open "default value"? :)
data PrettyOptions = PrettyOptions
    {   produceValidHaskell :: Bool
    }

defaultPrettyOptions :: PrettyOptions
defaultPrettyOptions = PrettyOptions True

type Precedence = Int

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

class PrettyTerm1 f where
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

class ProperOpTag o => PrettyUnaryOp o where
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

    prettyPrefix :: PrettyOptions -> o -> Doc ann
    prettyPrefix opts op =
        if produceValidHaskell opts
            then haskellPrefix op
            else unicodePrefix op

    haskellPrefix :: o -> Doc ann
    haskellPrefix op = fromString (opName op)

    unicodePrefix :: o -> Doc ann
    unicodePrefix = haskellPrefix

class ProperOpTag o => PrettyBinaryOp o where
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

    prettyInfix :: PrettyOptions -> o -> Doc ann
    prettyInfix opts op =
        if produceValidHaskell opts
            then haskellInfix op
            else unicodeInfix op

    haskellInfix :: o -> Doc ann
    haskellInfix op = backticks (fromString $ opName op)

    unicodeInfix :: o -> Doc ann
    unicodeInfix = haskellInfix

class ProperOpTag o => PrettyFlatOp o where
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

{-----------------------------------------------------------------------------}

-- Diagnosis tool
diag' :: Doc () -> Diag ()
diag' = diag . fuse Deep

-- Nested, comma-aligned list
aList :: (a -> Doc ann) -> [a] -> Doc ann
aList pp xs = flatAlt multiline singleline where
    sep = ", "
    docs = fmap pp xs
    multiline = nest 4 $ line
        -- FIXME: encloseSep uses 'cat' which will sometimes display
        -- elements in the same line. We want all or nothing.
        <> encloseSep "[ " (line <> "]") sep docs
    singleline = encloseSep "[" "]" sep docs

-- | Show parens depending on precedence
parensP :: Precedence -> Precedence -> Doc ann -> Doc ann
parensP d prec
    | d > prec  = parens
    | otherwise = id

-- | >>> backticks "x"
-- `x`
backticks :: Doc ann -> Doc ann
backticks = enclose backtick backtick

-- | >>> backtick
-- `
backtick :: Doc ann
backtick = fromString "`"
