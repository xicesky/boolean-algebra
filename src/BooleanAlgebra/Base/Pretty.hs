
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Description     : Pretty-printer
Stability       : experimental

Not yet implemented!
-}
module BooleanAlgebra.Base.Pretty
    (   -- * Temporary replacements
        prettyBool

    ,   -- * Re-exports
        pretty
    ,   IsString(..)
    ,   (<+>)
    ,   nest, hang, align
    ,   vsep

    ) where

import Data.String (IsString(..))
import Data.Bool (bool)
import Prettyprinter
import Term.Term
import Term.Prettyprinter
import BooleanAlgebra.Base.Expression

{-----------------------------------------------------------------------------}

-- | Pretty-print using boolean logic symbols
prettyBool :: PrettyTerm a => a -> Doc ann
prettyBool = prettyOpts (defaultPrettyOptions
    {   produceValidHaskell = False
    })

{-----------------------------------------------------------------------------}
-- Instances for term types

--pp :: (PrettyTerm1 a, PrettyTerm b, PrettyTerm c) => Fix4 TermF a b c -> Doc ann
-- pp :: (PrettyTerm1 a, PrettyTerm b, Pretty c) =>
--     PrettyOptions -> Precedence ->
--     Fix4 TermF a b (Literal c) -> Doc ann
-- pp = prettyTerm

instance (PrettyTerm1 op, PrettyTerm val, PrettyTerm var, Show var)
    => PrettyTerm (TermLit op val var) where
    prettyTerm opts d (TermLit t) = fromString "TermLit" <+> prettyTerm opts d t

instance (PrettyTerm1 op, PrettyTerm val, PrettyTerm var, Show var)
    => Pretty (TermLit op val var) where
    pretty = defaultPretty

instance PrettyTerm e => PrettyTerm (Conjunction e) where
    prettyTerm opts d (Conjunction xs) = group $
        fromString "Conjunction"
        <+> aList (prettyTerm opts 0) xs

instance PrettyTerm e => Pretty (Conjunction e) where
    pretty = defaultPretty

instance PrettyTerm e => PrettyTerm (Disjunction e) where
    prettyTerm opts d (Disjunction xs) = group $
        fromString "Disjunction"
        <+> aList (prettyTerm opts 0) xs

instance PrettyTerm e => Pretty (Disjunction e) where
    pretty = defaultPretty

instance (PrettyTerm name, Show name) => PrettyTerm (CNF name) where
    prettyTerm opts d (CNF cd) =
        fromString "CNF"
        <+> prettyTerm opts d cd

instance (PrettyTerm name, Show name) => Pretty (CNF name) where
    pretty = defaultPretty

-- Src: https://en.wikipedia.org/wiki/List_of_logic_symbols

instance PrettyUnaryOp BooleanUOp where
    haskellPrefix BooleanNot = fromString "not"
    unicodePrefix BooleanNot = fromString "¬"

instance PrettyBinaryOp BooleanBOp where
    haskellInfix BooleanAnd = fromString "&&"
    haskellInfix BooleanOr = fromString "||"
    unicodeInfix BooleanAnd = fromString "∧"
    unicodeInfix BooleanOr = fromString "∨"

instance PrettyFlatOp BooleanFlatOp

{-----------------------------------------------------------------------------}
-- Instances for base types

instance PrettyTerm Bool
instance (Show a, PrettyTerm a) => PrettyTerm (Literal a) where
    prettyVar opts d (sgn, var)
        | produceValidHaskell opts = viaShow (sgn, var)
        | otherwise = bool (unicodePrefix BooleanNot <>) id sgn $
            prettyVar opts d var
