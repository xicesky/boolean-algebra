
-- "Standard" extensions
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-} 

-- Extensions for compdata usage
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE GADTs                  #-}

module BooleanAlgebra.Variable where

import Data.Comp.Term
import Data.Comp.Ops
import Data.Comp.Sum (inject)
import Data.Comp.Algebra
    (   Alg, Coalg, RAlg, RCoalg
    ,   cata, ana, para, apo
    ,   Hom, CxtFun
    ,   appCxt, appHom
    )
import Data.Comp.Generic (subterms')

import BooleanAlgebra.THUtil
import BooleanAlgebra.Base
import BooleanAlgebra.Pretty
import BooleanAlgebra.Util.CompdataUtils

-- Fetch the names of all the variables
variableNames :: forall f. (Foldable f, BooleanVariable :<: f) =>
    Term f -> [String]
variableNames = fmap varName . subterms'

{-----------------------------------------------------------------------------}
-- Variable substitution

{- | substVars replaces occurances of @BooleanVariable@ by another term t.
    Note: This looks a little bit like desugaring, and can be specialized to
    specific variables.
-}
