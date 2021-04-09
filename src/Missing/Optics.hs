
{- |
Description     : Utilities for the 'Optics' library
Stability       : experimental

-}
module Missing.Optics
    (   updating
    ,   EmbedStateFun
    ,   stateFun
    ,   sGet
    ,   sSet
    ) where

import Control.Monad.State.Class
import Control.Monad.State.Strict
import Optics.State
import Optics.Core
import Optics.Zoom

{-----------------------------------------------------------------------------}

updating :: (Is k A_Getter, Is k A_Setter, MonadState s m) =>
    Optic' k is s a -> (a -> a) -> m a
updating lens f = do
    old <- use lens
    modifying' lens f
    return old

{-----------------------------------------------------------------------------}
-- State transitions over lenses

{- | A function to embed a state action on state @s@ in monad @m@.

A state action @(s -> (a, s))@ can be embedded in a stateful monad @m@ via
a @EmbedStateFun s m@. 'state' from 'MonadState' is one possible embedding
function (that works on the whole state).
-}
type EmbedStateFun s m = forall a. (s -> (a, s)) -> m a

{- | Turn a lens into a embedding function.

A lens from @s@ to @t@ can be used to embed state actions @(t -> (a, t))@
in a state monad @m@ with @MonadState s m@.

This is slightly more general than a 'zoom', because it only has a
'MonadState' constraint instead of requiring a concrete 'StateT'.

>>> flip evalState (0, 'a') $ stateFun _1 (\s -> (s, s + 1))
(0,(1,'a'))
-}
stateFun :: (Is k A_Getter, Is k A_Setter, MonadState s m) =>
    Optic' k is s t -> EmbedStateFun t m
stateFun lens action = do
    s0 <- use lens
    let (a, s1) = action s0
    assign lens s1
    return a

{-
TODO:
- These functions don't compose with lenses. Can we make them work like
    'modifying' etc.?
- Check strictness (and provide strict implementations)
-}

sGet :: EmbedStateFun s m -> m s
sGet f = f $ \s -> (s, s)

sSet :: EmbedStateFun s m -> s -> m ()
sSet f s = f $ const ((), s)
