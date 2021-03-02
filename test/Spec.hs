
-- This seems to cause problems
{- - # OPTIONS_GHC -F -pgmF hspec-discover # - -}

import Test.Hspec

import qualified BooleanAlgebra.Transform.CNFSpec
import qualified BooleanAlgebra.Transform.VariableSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "BooleanAlgebra.CNF" BooleanAlgebra.Transform.CNFSpec.spec
    describe "BooleanAlgebra.Variables" BooleanAlgebra.Transform.VariableSpec.spec
