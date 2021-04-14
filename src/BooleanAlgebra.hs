
-- | Grouping module, re-exports all modules from this package
module BooleanAlgebra
    (   module BooleanAlgebra.Base.Class
    ,   module BooleanAlgebra.Base.Logic
    ,   module BooleanAlgebra.Base.Expression
    ,   module BooleanAlgebra.Base.Pretty
    ,   module BooleanAlgebra.Transform.Simplify
    ,   module BooleanAlgebra.Transform.CNF
    ,   module BooleanAlgebra.Transform.Variable
    ,   module BooleanAlgebra.Solver.Class
    ,   module BooleanAlgebra.Solver.Basic
    ,   module BooleanAlgebra.Support.Arbitrary
    ) where

{- Inspiration:
    https://tuprints.ulb.tu-darmstadt.de/2759/1/rkibria-dissertation-final-korrigiert1.pdf
    https://bartoszmilewski.com/2017/02/28/f-algebras/
    https://www-ps.informatik.uni-kiel.de/~sebf/projects/sat-solver/Control/Monad/Constraint/Boolean.lhs.html
    Recursion schemes: https://blog.sumtypeofway.com/archive.html
    Or as video: https://www.youtube.com/watch?v=Zw9KeP3OzpU
-}

import BooleanAlgebra.Base.Class
import BooleanAlgebra.Base.Logic
import BooleanAlgebra.Base.Expression
import BooleanAlgebra.Base.Pretty
import BooleanAlgebra.Transform.Simplify
import BooleanAlgebra.Transform.CNF
import BooleanAlgebra.Transform.Variable

{- TODO: Library / project organisation.

The following modules should probably be extras and put in a separate package.
-}
import BooleanAlgebra.Solver.Class
import BooleanAlgebra.Solver.Basic
import BooleanAlgebra.Support.Arbitrary
