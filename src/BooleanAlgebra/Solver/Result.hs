
{- |
Description     : Solver result datatype
Stability       : experimental
-}
module BooleanAlgebra.Solver.Result 
    (   -- * Sat solver result
        SatResult(..)
    ,   SatError(..)
    ,   SatT
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

-- | Shortcut monad for SatError exceptions
type SatT name = ExceptT (SatError name)

-- -- FIXME: To variable module
-- lookupVariable :: (Ord name, MonadError (SatError name) m) =>
--     Map name v -> name -> m v
-- lookupVariable map name =
--     maybe (throwError $ MissingVariable name)
--     return (Map.lookup name map)

runSatT :: Monad m =>
    (SatError name -> m a) -> SatT name m a -> m a
runSatT handleError inner = runExceptT inner >>= \case
    Left err    -> handleError err
    Right r     -> return r

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
