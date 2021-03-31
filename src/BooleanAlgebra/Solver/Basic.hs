
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module BooleanAlgebra.Solver.Basic where

import Data.List (foldl')
import Data.Foldable
import Data.Monoid (Any(..), All(..))
import Control.Applicative

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- mtl / transformers
import Control.Monad.Identity
import Control.Monad.Cont
import Control.Monad.State

-- optics
--import Optics.TH (makeFieldLabelsNoPrefix)  -- req optics-0.4
import Optics.TH (makeFieldLabelsWith, noPrefixFieldLabels)
import Optics.State

-- splices only
import Optics.Lens
import Optics.Internal.Optic.Types
import Optics.Internal.Optic

import BooleanAlgebra.Base.Class
import BooleanAlgebra.Base.Expression
import BooleanAlgebra.Transform.Variable
import BooleanAlgebra.Solver.Class

import GHC.Stack (HasCallStack)
import Debug.Trace

{-----------------------------------------------------------------------------}
-- Nondetminism monad

type NonDetM r = ContT r []

instance Alternative (NonDetM r) where
    empty = lift empty
    (<|>) (ContT l) (ContT r) = ContT $ \c ->
        l c <|> r c

instance MonadPlus (NonDetM r)

runNonDetM :: forall a. (forall r. NonDetM r a) -> [a]
runNonDetM m = runContT m pure

evalNonDetM :: forall a. a -> (forall r. NonDetM r a) -> a
evalNonDetM failure m = case runNonDetM m of
    []      -> failure
    (x:xs)  -> x

choices :: Alternative m => [m a] -> m a
choices = foldl' (<|>) empty

chooseOf :: Alternative m => [a] -> m a
chooseOf = choices . fmap pure

{-----------------------------------------------------------------------------}
-- Solver state & monad

data SATState = SATState
    {   clauses :: [ Disjunction (Literal Int) ]
    ,   assignments :: Map Int Bool
    }

-- haskell-language-server crashes
--makeFieldLabelsWith noPrefixFieldLabels ''SATState
--makeFieldLabelsNoPrefix ''SATState

instance (k_amVd ~ Optics.Internal.Optic.Types.A_Lens,
          a_amVe ~ Map Int Bool,
          b_amVf ~ Map Int Bool) =>
         Optics.Internal.Optic.LabelOptic "assignments" k_amVd SATState SATState a_amVe b_amVf where
  {-# INLINE labelOptic #-}
  labelOptic
    = Optics.Lens.lensVL
        (\ f_amVg s_amVh
           -> case s_amVh of {
                SATState x1_amVi x2_amVj
                  -> (fmap (\ y_amVk -> (SATState x1_amVi) y_amVk))
                       (f_amVg x2_amVj) })
instance (k_amVl ~ Optics.Internal.Optic.Types.A_Lens,
          a_amVm ~ [Disjunction (Literal Int)],
          b_amVn ~ [Disjunction (Literal Int)]) =>
         Optics.Internal.Optic.LabelOptic "clauses" k_amVl SATState SATState a_amVm b_amVn where
  {-# INLINE labelOptic #-}
  labelOptic
    = Optics.Lens.lensVL
        (\ f_amVo s_amVp
           -> case s_amVp of {
                SATState x1_amVq x2_amVr
                  -> (fmap (\ y_amVs -> (SATState y_amVs) x2_amVr))
                       (f_amVo x1_amVq) })

initState :: [Disjunction (Literal Int)] -> SATState
initState cs = SATState cs mempty

type SolverM a = forall r. StateT SATState (NonDetM r) a

{-----------------------------------------------------------------------------}
-- Evaluating algebras
-- TODO: Formalize and put in seperate module

class EvalAlg f a where
    eval :: f a -> a

instance EvalAlg Disjunction Bool where
    eval (Disjunction xs) = getAny $ foldMap Any xs

instance EvalAlg Conjunction Bool where
    eval (Conjunction xs) = getAll $ foldMap All xs

{-----------------------------------------------------------------------------}
-- Actual solver

assignVar :: Int -> Bool -> SolverM ()
assignVar i val = modifying #assignments $ Map.insert i val

checkClauses :: SolverM ()
checkClauses = use #assignments >>= \asgn -> let
        lup :: Literal Int -> Maybe Bool
        lup (sgn, var) = xnor sgn <$> asgn Map.!? var
        check :: Disjunction (Literal Int) -> SolverM ()
        check disj = case eval <$> traverse lup disj of
            Just False  -> empty        -- unsatisfiable
            _           -> return ()    -- satisfied / undetermined
    in use #clauses >>= traverse_ check

solverLoop :: [Int] -> SolverM ()
solverLoop []       = return ()
solverLoop (x:xs)   = do
    -- traceM $ "SolverLoop " ++ show (x:xs)
    phase <- chooseOf [True, False]
    assignVar x phase
    checkClauses
    solverLoop xs

{-----------------------------------------------------------------------------}
-- Interface

data BasicSolver = BasicSolver

instance Monad m => Solver BasicSolver m where
    solveInt :: BasicSolver -> Int -> CNF Int -> SatT a m (SatResult Int)
    solveInt _ maxVar (CNF (Conjunction xs)) = return $ evalNonDetM Unsat $ do
        (_, state) <- runStateT (solverLoop [1..maxVar]) (initState xs)
        return $ Sat (assignments state)
