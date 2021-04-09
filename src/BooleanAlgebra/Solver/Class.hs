
{- |
Description     : Solver interface
Stability       : experimental

Defines a common interface for internal and external sat solvers
-}
module BooleanAlgebra.Solver.Class
    (   -- * Sat solver result
        SatResult(..)
    ,   eqSat
    ,   SatError(..)
    ,   Solver(..)
    ,   solve'

    ,   -- * Utilities
        SatT
    ,   liftSolve
    ,   mapResultNames

    ,   -- * Re-exports
        runSatT
    ,   MonadError(..)
    ) where

import Data.Maybe
import Data.Foldable
import Data.Typeable (Typeable)
import Control.Exception

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- mtl / transformers
import Control.Monad.Error.Class
import Control.Monad.Except

import Missing.Monad.NamingT
import qualified Missing.Bimap as Bimap

import BooleanAlgebra.Base.Class
import BooleanAlgebra.Base.Expression
import BooleanAlgebra.Transform.Variable

import Debug.Trace

{-----------------------------------------------------------------------------}

-- | Result of successfully running a sat solver.
data SatResult name
    =   Unsat
    |   Sat (Map name Bool)
    deriving (Show, Eq, Ord)

-- | Compare two 'SatResult's for solvability, ignoring the model
eqSat :: SatResult name -> SatResult name -> Bool
eqSat Unsat Unsat       = True
eqSat (Sat _) (Sat _)   = True
eqSat _ _               = False

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

instance (Show name, Typeable name) => Exception (SatError name)

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
    {- | Solves the SAT problem given in CNF.

    @solveInt solver n cnf@ produces a SatResult wrapped in the 'SatT' monad
    for a given @solver@, a given maximum literal integer @n@ and a problem
    @cnf@. All variables v with @0 < v <= n@ that occur in @cnf@ will have
    assignments to 'Bool' if this functions returns a 'Sat' result.
    -}
    solveInt :: s -> Int -> CNF Int -> SatT a m (SatResult Int)

    solve :: (Show name, Monoid name, Ord name) =>
        s -> CNF name -> SatT name m (SatResult name)
    solve s cnf = liftSolve (solveInt s) cnf

{- | Lift a solver on @CNF Int@ to a solver on @CNF name@ for arbitrary names.

This function wraps a solver function like 'solveInt' by replacing variable
names with integers (greater than zero), then running the solver and finally
mapping the results back from @Int@ to the original variable names.

It is used to define the default implementation for 'solve'.
-}
liftSolve :: (Monad m, Show name, Monoid name, Ord name) =>
    (forall a. Int -> CNF Int -> SatT a m (SatResult Int))
    -> CNF name -> SatT name m (SatResult name)
liftSolve solveInt cnf = runNamingT 1 $ do
    cnfi <- traverse autoMapName cnf
    maxVar <- peekIndex
    result <- lift $ solveInt maxVar cnfi
    ntoi <- Bimap.toMapR <$> getNameMap
    -- traceM $ "resultnames ntoi: " ++ show ntoi
    -- traceM $ "resultnames result: " ++ show result
    lift $ mapResultNames ntoi result

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

-- | Simplified 'solve' method for interactive use
solve' :: Solver s IO => 
    s -> CNF String -> IO (Either (SatError String) (SatResult String))
solve' s cnf = runSatT (return . Left) $ Right <$> solve s cnf
