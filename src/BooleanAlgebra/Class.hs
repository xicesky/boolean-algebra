
{-# LANGUAGE PatternSynonyms        #-}

module BooleanAlgebra.Class where

import Prelude hiding (and, or, not, (&&), (||))

class Boolean b where
    and :: b -> b -> b
    or :: b -> b -> b
    not :: b -> b

{- Note: I would love to have a PatternSynonym for the ops
    but it can't work for classes i guess
-}

infixr 3 &&
(&&) :: Boolean b => b -> b -> b
(&&) = and

infixr 2 ||
(||) :: Boolean b => b -> b -> b
(||) = or

truth :: Boolean b => b -> b
truth x = x || not x

falsity :: Boolean b =>  b -> b
falsity x = x && not x

-- FIXME: Strings are bad

class Boolean b => BooleanAlgebra b where
    var :: String -> b
