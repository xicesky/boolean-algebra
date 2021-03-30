
{- |
Description     : Solver interface
Stability       : experimental

Defines a common interface for internal and external sat solvers
-}
module BooleanAlgebra.Solver.Class
    (   -- * Sat solver result
        SatResult(..)
    ,   SatError(..)
    ,   Solver(..)

    ,   -- * Utilities
        SatT
    ,   mapResultNames

    ,   -- * Re-exports
        runSatT
    ,   MonadError(..)
    ) where

import Data.Maybe
import Data.Foldable

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- mtl / transformers
import Control.Monad.Error.Class
import Control.Monad.Except

import BooleanAlgebra.Base.Class
import BooleanAlgebra.Base.Expression
import BooleanAlgebra.Transform.Variable

{-----------------------------------------------------------------------------}

-- | Result of successfully running a sat solver.
data SatResult name
    =   Unsat
    |   Sat (Map name Bool)
    deriving (Show, Eq, Ord)

{- | Possible errors for sat operations

This includes errors that are only thrown under certain circumstances, for
example 'ExternalSolverError' for running an external solver.
-}
data SatError name
    =   ExternalSolverError String
    -- ^ Thrown when executing an external sat solver fails
    |   MissingVariable name
    -- ^ Thrown when a model is missing a variable
    deriving (Show, Eq, Ord)

{-----------------------------------------------------------------------------}

-- | Monad transformer for running sat solvers
type SatT name = ExceptT (SatError name)

runSatT :: Monad m =>
    (SatError name -> m a) -> SatT name m a -> m a
runSatT handleError inner = runExceptT inner >>= \case
    Left err    -> handleError err
    Right r     -> return r

{-----------------------------------------------------------------------------}

{- | General class for sat solvers

The monad @m@ parameter is used for external solvers that require, for example,
a 'MonadIO' constraint.
-}
class Monad m => Solver s m where
    solveInt ::
        s -> Int -> CNF Int -> SatT a m (SatResult Int)

    solve :: Ord name =>
        s -> CNF name -> SatT name m (SatResult name)
    solve s cnf = let
        Context (iton, ntoi) cnfi = buildContext cnf
        in do
            result <- solveInt s (maxVarNum iton) cnfi
            mapResultNames ntoi result

-- FIXME: This should be made easier by using Context, not harder
maxVarNum :: Map Int name -> Int
maxVarNum map = maximum $ 0 : Map.keys map

{- | Map the model given by a sat solver back to named variables.

Variables in the model that do not correspond to any variables names
are /ignored/.
-}
mapResultNames :: (Ord name, MonadError (SatError name) m) =>
    Map name Int -> SatResult Int -> m (SatResult name)
mapResultNames ntoi Unsat   = return Unsat
mapResultNames ntoi (Sat m) = Sat <$> Map.traverseWithKey lup ntoi where
    lup name index =
        maybe (throwError $ MissingVariable name)
        return (Map.lookup index m)
