
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Description     : Pretty-printing for terms
Stability       : experimental

-}
module Term.Prettyprinter
    (   -- * Pretty printing
        PrettyTerm(..)

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

import Prettyprinter
-- For diagnosis - showing internal pretty-printer state
import Prettyprinter.Internal.Debug (diag, Diag)

import Term.Term

{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}

{-----------------------------------------------------------------------------}

data PrettyOptions = PrettyOptions
    {   produceValidHaskell :: Bool
    }

defaultPrettyOptions :: PrettyOptions
defaultPrettyOptions = PrettyOptions True

type Precedence = Int

class PrettyTerm t where
    {- | Pretty-print a term with specific options

    Note: In contrast to 'pretty', this function prefers to produce
    valid haskell expressions.
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

    {- | Default implementation for 'Pretty' instances
    -}
    defaultPretty :: t -> Doc ann
    defaultPretty = prettyTerm defaultPrettyOptions 0
    -- = --

class PrettyTerm1 f where
    liftPrettyTerm1 :: forall a ann.
            (PrettyOptions -> Precedence -> a -> Doc ann)
        -> PrettyOptions -> Precedence -> f a -> Doc ann
    -- = --

{-----------------------------------------------------------------------------}

instance PrettyTerm1 (f a b c) => PrettyTerm (Fix4 f a b c) where
    prettyTerm :: forall ann.
        PrettyOptions -> Precedence -> Fix4 f a b c -> Doc ann
    prettyTerm opts d (Fix4 f) =
        liftPrettyTerm1 prettyTerm opts d f

instance (PrettyTerm val, PrettyTerm var, PrettyTerm1 op)
    => PrettyTerm1 (TermF op val var) where

    liftPrettyTerm1 prettyR opts d = \case
        ConstT v        -> parensP d 10 $
            fromString "Val" <+> prettyTerm opts d v
        VariableT v     -> parensP d 10 $
            fromString "Var" <+> prettyVar opts d v
        RecT v          -> liftPrettyTerm1 prettyR opts d v

instance PrettyTerm1 (Op uop bop flop) where
    liftPrettyTerm1 prettyR opts d = \case
        UnaryOp o t     -> parensP d (opPrec o) $
            -- FIXME: let operator handle it
            fromString (opName o) <+> prettyR opts (opPrec o + 1) t
        BinaryOp o a b  -> parensP d (opPrec o) $
            prettyR opts (opPrec o + 1) a
            <+> backticks (fromString $ opName o)
            <+> prettyR opts (opPrec o + 1) b
        FlatOp o xs     -> parensP d (opPrec o) $ group $
            fromString (opName o) <+> aList (prettyR opts 0) xs

instance (PrettyTerm val, PrettyTerm var, PrettyTerm1 op)
    => Pretty (Term op val var) where
    pretty = defaultPretty

{-----------------------------------------------------------------------------}
-- Instances for common variable / value types

instance PrettyTerm String where
    prettyTerm _ _ = viaShow
    prettyVar _ _ s = fromString s

instance PrettyTerm Void

{-----------------------------------------------------------------------------}

-- Diagnosis tool
diag' :: Doc () -> Diag ()
diag' = diag . fuse Deep

-- Nested, comma-aligned list
aList :: (a -> Doc ann) -> [a] -> Doc ann
aList pp xs = flatAlt multiline singleline where
    lbr = flatAlt "[ " "["
    rbr = flatAlt (line <> "]") "]"
    sep = ", "
    docs = fmap pp xs
    multiline = nest 4 $ line
        <> encloseSep lbr rbr sep docs
    singleline = encloseSep lbr rbr sep docs

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
