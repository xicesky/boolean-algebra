
module BooleanAlgebra.Base.PrettySpec where

import Data.Char
import Data.Foldable

-- hspec & quickcheck
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Prettyprinter

import Missing.Textual
import Term.Prettyprinter
import BooleanAlgebra

{-----------------------------------------------------------------------------}
-- Utilities

isPrintable :: Foldable t => t Char -> Bool
isPrintable = all isPrint

{-----------------------------------------------------------------------------}

-- | Function produces printable text
prop_printable :: forall a t. Textual t => (a -> t) -> a -> Bool
prop_printable pp = isPrintable . textualToString . pp

-- | Pretty-printing function produces valid Haskell
prop_readable :: forall a. (Show a, Eq a, Read a) => (a -> String) -> a -> Property
prop_readable pp val = let
    result :: a
    result = read $ pp val
    in result === val

{-----------------------------------------------------------------------------}

spec_prettyBool :: Spec
spec_prettyBool = describe "prettyBool" $
    prop "produces printable strings" $
        prop_printable @(BooleanExpr String) @(Doc ()) prettyBool

spec_prettyTerm :: Spec
spec_prettyTerm = describe "prettyTerm" $
    -- TODO: Extend this to BooleanExpr, once we have a valid parser
    prop "produces parsable Haskell @String" $
        prop_readable @String (show . prettyTerm (PrettyOptions True) 0)

spec :: Spec
spec = do
    spec_prettyTerm
    spec_prettyBool
