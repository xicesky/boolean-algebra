
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FunctionalDependencies #-}

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
    ,   BooleanAlgebra(..)

    ,   -- * Boolean operators
        (&&), (||)

    ,   -- * Interpretable terms
        InterpretBooleanArithmetic(..)
    ,   InterpretBooleanAlgebra(..)

    ) where

import Prelude hiding (and, or, not, (&&), (||))
import qualified Prelude as P

{-
TODO:
    - Specify laws for class definitions where possible
-}

{-----------------------------------------------------------------------------}

{- | Values that can be negated using @not@.

In Haskell we can't make boolean values use negative signs.
-}
class PreBoolean b where
    {- | Negation, ¬
        Guarantees: @not (not a) ~ a@.
    -}
    not :: b -> b

-- | Anything that has boolean operations
class PreBoolean b => Boolean b where
    {-# MINIMAL and, or #-}
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

{-----------------------------------------------------------------------------}

-- | Boolean arithmetic can represent truth values
class Boolean b => BooleanArithmetic b where
    -- | Represent a Haskell 'Bool' in the target language
    fromBool :: Bool -> b

    -- | Shortcut for @fromBool True@
    true :: b
    true = fromBool True

    -- | Shortcut for @fromBool False@
    false :: b
    false = fromBool False

-- | Full boolean algbra has both values and variables
class BooleanArithmetic (b a) => BooleanAlgebra b a where
    -- | Represent a variable in the target algebra
    var :: a -> b a
    -- evalAlg :: b Bool -> Bool

{-----------------------------------------------------------------------------}

instance PreBoolean Bool where
    not = P.not

instance Boolean Bool where
    and = (P.&&)
    or = (P.||)

instance BooleanArithmetic Bool where
    fromBool = id

{-----------------------------------------------------------------------------}

-- | Terms that can be interpreted as boolean arithmetic
class InterpretBooleanArithmetic t where
    -- | Interpet a term @t@ in a boolean arithmetic.
    interpretArith :: forall a. BooleanArithmetic a => t -> a

-- | Terms that can be interpreted as boolean algebra
class InterpretBooleanAlgebra t v | t -> v
    where 
    -- | Interpet a term @t@ in a boolean algebra.
    interpretAlg :: forall a. BooleanAlgebra a v => t -> a v
