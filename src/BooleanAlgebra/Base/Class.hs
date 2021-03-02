
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE DefaultSignatures      #-}

{- | Generalized boolean algebras.

When using this, consider hiding parts of the prelude:

>>> import Prelude hiding (and, or, not, (&&), (||))
>>> import qualified Prelude as P

-}
module BooleanAlgebra.Base.Class where

import Prelude hiding (and, or, not, (&&), (||))
import qualified Prelude as P

{-----------------------------------------------------------------------------}

-- | Anything that has boolean operations
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

instance Boolean Bool where
    and = (P.&&)
    or = (P.||)
    not = P.not

instance BooleanArithmetic Bool where
    fromBool = id

-- FIXME: Remove and re-export Data.Bool (bool)
ifthenelse :: Bool -> a -> a -> a
ifthenelse True a _ = a
ifthenelse False _ b = b
