
-- This seems to cause problems
{- - # OPTIONS_GHC -F -pgmF hspec-discover # - -}

import Test.Hspec

import qualified BooleanAlgebra.CNFSpec
import qualified BooleanAlgebra.VariableSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "BooleanAlgebra.CNF" BooleanAlgebra.CNFSpec.spec
    describe "BooleanAlgebra.Variables" BooleanAlgebra.VariableSpec.spec
