
{-# LANGUAGE OverloadedStrings #-}

{- |
Description     : Pretty-printing utilities
Stability       : experimental

-}
module Missing.Prettyprinter where

import Data.String (IsString(..))

import Prettyprinter
-- For diagnosis - showing internal pretty-printer state
import Prettyprinter.Internal.Debug (diag, Diag)

{-----------------------------------------------------------------------------}

type Precedence = Int

{-----------------------------------------------------------------------------}

{-
TODO:
These functions use 'encloseSep', which internally uses 'cat', which will
sometimes display elements in the same line, even though our layout has
"decided" to print elements in seperate lines. (All-or-nothing)
In order to fix this, we would have to duplicate the code from Prettyprinter
here. The occurances are marked with: XXX encloseSep
-}

-- | Nested, left-aligned, comma-seperated list
aList :: (a -> Doc ann) -> [a] -> Doc ann
aList pp xs = flatAlt multiline singleline where
    sep = ", "
    docs = fmap pp xs
    multiline = nest 4 $ line
        -- XXX encloseSep
        <> encloseSep "[ " (line <> "]") sep docs
    singleline = encloseSep "[" "]" sep docs

-- | Nested, left-aligned, comma-seperated record
aRecord :: [Doc ann] -> Doc ann
aRecord docs = flatAlt multiline singleline where
    sep = ", "
    multiline = nest 4 $ line
        -- XXX encloseSep
        <> encloseSep "{ " (line <> "}") sep docs
    singleline = encloseSep "{" "}" sep docs

{-----------------------------------------------------------------------------}

-- | Show parentheses depending on precedence
parensP :: Precedence -> Precedence -> Doc ann -> Doc ann
parensP d prec
    | d > prec  = parens
    | otherwise = id

{-----------------------------------------------------------------------------}

-- | >>> backticks "x"
-- `x`
backticks :: Doc ann -> Doc ann
backticks = enclose backtick backtick

-- | >>> backtick
-- `
backtick :: Doc ann
backtick = fromString "`"

{-----------------------------------------------------------------------------}

-- | Diagnosis tool for showing the internal representation
diag' :: Doc () -> Diag ()
diag' = diag . fuse Deep
