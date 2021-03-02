
-- Utility module for Template Haskell usage in other boolean-algebra modules
module BooleanAlgebra.Util.THUtil 
    ( deriveOpts
    , deriveDefault
    , deriveLiftSum
    , deriveNoShow
    , deriveDebug
    ) where

import Data.Comp.Derive

{-----------------------------------------------------------------------------}
-- General options

-- We use Data.Comp.Derive.derive with the same list of options
-- for all out datatypes over and over. This is the list of options:

deriveOpts =
    [ makeTraversable
    , makeFoldable
    , makeEqF
    , makeShowF
    , makeShowConstr
    , smartConstructors
    --, smartAConstructors
    ]

-- This makes it much shorter to do the same thing over and over
deriveDefault = derive deriveOpts

-- This is used to lift our classes
deriveLiftSum = derive [liftSum]

-- For special cases where we implement ShowF ourselves
deriveNoShow = derive [ makeTraversable
    , makeFoldable
    , makeEqF
    -- , makeShowF
    -- , makeShowConstr
    , smartConstructors
    --, smartAConstructors
    ]

-- For debugging purposes only
deriveDebug = derive [ makeTraversable
    -- , makeFoldable
    , makeEqF
    -- , makeShowF
    -- , makeShowConstr
    -- , smartConstructors
    --, smartAConstructors
    ]
