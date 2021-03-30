
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE DefaultSignatures      #-}

{- |
Description     : Generalized boolean algebras
Stability       : experimental

When using this module, consider hiding parts of the prelude:

>>> import Prelude hiding (and, or, not, (&&), (||))
>>> import qualified Prelude as P

-}
module BooleanAlgebra.Base.Class
    (   -- * Classes for booleans and boolean algebras
        PreBoolean(..)
    ,   Boolean(..)
    ,   BooleanArithmetic(..)
    ,   BooleanPreAlgebra(..)
    ,   BooleanAlgebra(..)

    ,   -- * Boolean operators
        (&&), (||)

    ,   -- * Utility functions
        truth, falsity

    ,   -- * Interpretable terms
        InterpretBooleanArithmetic(..)
    ,   InterpretBooleanAlgebra(..)

    ) where

import Prelude hiding (and, or, not, (&&), (||))
import qualified Prelude as P

{-----------------------------------------------------------------------------}

{- | Values that can be negated using @not@.

In Haskell we can't make boolean values use negative signs.
-}
class PreBoolean b where
    -- FIXME: How to provide an actual law without specifying eval?
    {- | Negation, ¬
        Guarantees: @not (not a) ~ a@.
    -}
    not :: b -> b

-- | Anything that has boolean operations
class PreBoolean b => Boolean b where
    {-# MINIMAL and, or #-}
    -- FIXME: How to provide an actual law without specifying eval?
    {- | Conjunction, ∧
    -}
    and :: b -> b -> b

    {- | Disjunction, ∨
    -}
    or :: b -> b -> b

    {- | Exclusive or, ⊕
    -}
    xor :: b -> b -> b
    xor a b = a && not b || not a && b

    {- | Exclusive nor, ⊙
    -}
    xnor :: b -> b -> b
    xnor a b = a && b || not a && not b

-- | Boolean /conjunction/ operator.
infixr 3 &&
(&&) :: Boolean b => b -> b -> b
(&&) = and

-- | Boolean /disjunction/ operator.
infixr 2 ||
(||) :: Boolean b => b -> b -> b
(||) = or

truth :: Boolean b => b -> b
truth x = x || not x

falsity :: Boolean b =>  b -> b
falsity x = x && not x

{-----------------------------------------------------------------------------}

-- | Boolean arithmetic can represent truth values
class Boolean b => BooleanArithmetic b where
    -- | Represent a Haskell 'Bool' in the target language
    fromBool :: Bool -> b
    default fromBool :: BooleanPreAlgebra b => Bool -> b
    fromBool x = ifthenelse x (truth $ var "?") (falsity $ var "?")

    -- | Shortcut for @fromBool True@
    true :: b
    true = fromBool True

    -- | Shortcut for @fromBool False@
    false :: b
    false = fromBool False

-- FIXME: Strings are bad
-- | Boolean pre-algebra has variables (but not necessarily values)
class Boolean b => BooleanPreAlgebra b where
    -- | Represent a variable in the target algebra
    var :: String -> b

-- | Full boolean algbra has both values and variables
class (BooleanArithmetic b, BooleanPreAlgebra b) => BooleanAlgebra b

{-----------------------------------------------------------------------------}

instance PreBoolean Bool where
    not = P.not

instance Boolean Bool where
    and = (P.&&)
    or = (P.||)

instance BooleanArithmetic Bool where
    fromBool = id

-- FIXME: Remove and re-export Data.Bool (bool)
ifthenelse :: Bool -> a -> a -> a
ifthenelse True a _ = a
ifthenelse False _ b = b

{-----------------------------------------------------------------------------}

-- | Terms that can be interpreted as boolean arithmetic
class InterpretBooleanArithmetic t where
    -- | Interpet a term @t@ in a boolean arithmetic.
    interpretArith :: forall a. BooleanArithmetic a => t -> a

-- | Terms that can be interpreted as boolean algebra
class InterpretBooleanAlgebra t where
    -- | Interpet a term @t@ in a boolean algebra.
    interpretAlg :: forall a. BooleanAlgebra a => t -> a
