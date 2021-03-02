
-- | Grouping module, re-exports all modules from this package
module BooleanAlgebra
    (   module BooleanAlgebra.Class
    ,   module BooleanAlgebra.Logic
    ,   module BooleanAlgebra.Base
    ,   module BooleanAlgebra.Pretty
    ,   module BooleanAlgebra.Simplify
    ,   module BooleanAlgebra.Aggregate
    ,   module BooleanAlgebra.CNF
    ,   module BooleanAlgebra.Variable
    ,   module BooleanAlgebra.Arbitrary
    ) where

import BooleanAlgebra.Class
import BooleanAlgebra.Logic
import BooleanAlgebra.Base
import BooleanAlgebra.Pretty
import BooleanAlgebra.Simplify
import BooleanAlgebra.Aggregate
import BooleanAlgebra.CNF
import BooleanAlgebra.Variable

-- FIXME: Those should probably be "extras" and not in the main lib
import BooleanAlgebra.Arbitrary
