
-- | Grouping module, re-exports all modules from this package
module BooleanAlgebra
    (   module BooleanAlgebra.Base.Class
    ,   module BooleanAlgebra.Base.Logic
    ,   module BooleanAlgebra.Base.Expression
    ,   module BooleanAlgebra.Base.Pretty
    ,   module BooleanAlgebra.Transform.Simplify
    ,   module BooleanAlgebra.Transform.Aggregate
    ,   module BooleanAlgebra.Transform.CNF
    ,   module BooleanAlgebra.Transform.Variable
    ,   module BooleanAlgebra.Support.Arbitrary
    ) where


import BooleanAlgebra.Base.Class
import BooleanAlgebra.Base.Logic
import BooleanAlgebra.Base.Expression
import BooleanAlgebra.Base.Pretty
import BooleanAlgebra.Transform.Simplify
import BooleanAlgebra.Transform.Aggregate
import BooleanAlgebra.Transform.CNF
import BooleanAlgebra.Transform.Variable

-- FIXME: Those should probably be "extras" and not in the main lib
import BooleanAlgebra.Support.Arbitrary
