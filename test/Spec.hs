
-- This seems to cause problems
{- - # OPTIONS_GHC -F -pgmF hspec-discover # - -}

import Test.Hspec

import qualified Missing.Monad.NamingTSpec
import qualified BooleanAlgebra.Base.PrettySpec
import qualified BooleanAlgebra.Transform.CNFSpec
import qualified BooleanAlgebra.Transform.VariableSpec
import qualified BooleanAlgebra.Solver.ClassSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Missing.Monad.NamingT"                Missing.Monad.NamingTSpec.spec
    describe "BooleanAlgebra.Base.Pretty"           BooleanAlgebra.Base.PrettySpec.spec
    describe "BooleanAlgebra.Transform.CNF"         BooleanAlgebra.Transform.CNFSpec.spec
    describe "BooleanAlgebra.Transform.Variables"   BooleanAlgebra.Transform.VariableSpec.spec
    describe "BooleanAlgebra.Solver.ClassSpec"      BooleanAlgebra.Solver.ClassSpec.spec
