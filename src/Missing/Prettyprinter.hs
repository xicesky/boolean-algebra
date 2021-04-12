
{-# LANGUAGE OverloadedStrings #-}

module Missing.Prettyprinter where

import Prettyprinter

-- FIXME: utils from Term.Prettyprinter should also go here

-- | Nested, left-aligned, comma-seperated record
aRecord :: [Doc ann] -> Doc ann
aRecord docs = flatAlt multiline singleline where
    sep = ", "
    multiline = nest 4 $ line
        -- FIXME: encloseSep uses 'cat' which will sometimes display
        -- elements in the same line. We want all or nothing.
        <> encloseSep "{ " (line <> "}") sep docs
    singleline = encloseSep "{" "}" sep docs
