
-- Just playing around with Ideas from
--  https://www-ps.informatik.uni-kiel.de/~sebf/category/projects/index.html

module ConstrainedMonadic where

import Data.Kind (Type)
import Control.Applicative
import Control.Monad
import Control.Monad.State

class MonadPlus (Solver cs) => CStore cs where
    type Constraint cs
    type     Solver cs :: Type -> Type

    noConstraints :: cs
    assert        :: Constraint cs -> StateT cs (Solver cs) ()
    labeling      :: StateT cs (Solver cs) ()

class (MonadPlus m, CStore (Store m)) => MonadC m where
    type Store m

    (&>)     :: Constraint (Store m) -> m a -> m a
    solution :: m a -> StateT (Store m) (Solver (Store m)) a

    liftC    :: Solver (Store m) a -> m a

guardC :: MonadC m => Constraint (Store m) -> m ()
guardC c = c &> return ()

newtype Constr cs a = Constr { runConstr :: Solver cs (Constrained cs a) }

data Constrained cs a = Lifted a | Guarded (Constraint cs) (Constr cs a)

-- Added by MD: Definition of "return" before monad instance
--  so we can use it for functor/applicative
_csReturn :: CStore cs => a -> Constr cs a
_csReturn = Constr . return . Lifted

-- Added by MD: Definition of ">>=" before monad instance
--  so we can use it for functor/applicative
_csBind :: CStore cs => Constr cs a -> (a -> Constr cs b) -> Constr cs b
_csBind x f = Constr (runConstr x >>= bind) where
        bind (Lifted a)    = runConstr (f a)
        bind (Guarded c y) = return (Guarded c (y >>= f))

-- Added by MD
instance CStore cs => Functor (Constr cs) where
    fmap :: (a -> b) -> Constr cs a -> Constr cs b
    fmap f fa = fa `_csBind` (_csReturn . f)

-- Added by MD
instance CStore cs => Applicative (Constr cs) where
    pure :: a -> Constr cs a
    pure = _csReturn

    (<*>) :: Constr cs (a -> b) -> Constr cs a -> Constr cs b
    (<*>) cf ca = cf `_csBind` (\f -> fmap f ca)

instance CStore cs => Monad (Constr cs) where
    -- return = Constr . return . Lifted        -- provided by "pure" above
    (>>=) = _csBind
    -- x >>= f = Constr (runConstr x >>= bind) where
    --     bind (Lifted a)    = runConstr (f a)
    --     bind (Guarded c y) = return (Guarded c (y >>= f))

-- Added by MD: Pre-Definition of "mzero/mplus" before alternative/monadplus instance
_csMzero :: CStore cs => Constr cs a
_csMzero = Constr mzero

_csMPlus :: CStore cs => Constr cs a -> Constr cs a -> Constr cs a
_csMPlus x y = Constr (runConstr x `mplus` runConstr y)

instance CStore cs => Alternative (Constr cs) where
    empty :: Constr cs a
    empty = _csMzero
    (<|>) :: Constr cs a -> Constr cs a -> Constr cs a
    (<|>) = _csMPlus

instance CStore cs => MonadPlus (Constr cs) where
    -- mzero = Constr mzero
    -- x `mplus` y = Constr (runConstr x `mplus` runConstr y)

instance CStore cs => MonadC (Constr cs) where
    type Store (Constr cs) = cs

    c &> x = Constr (return (Guarded c x))

    solution x = lift (runConstr x) >>= solve where
        solve (Lifted a)    = return a
        solve (Guarded c y) = do assert c; solution y
    
    liftC x = Constr (x >>= return . Lifted)

