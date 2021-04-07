
{- |
Description     : Utilities for the 'Optics' library
Stability       : experimental

-}
module Missing.Optics
    (   updating
    ) where

import Control.Monad.State.Class
import Optics.State
import Optics.Core

updating :: (Is k A_Getter, Is k A_Setter, MonadState s m) =>
    Optic' k is s a -> (a -> a) -> m a
updating lens f = do
    old <- use lens
    modifying' lens f
    return old
