
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BooleanAlgebra.Problem.Encoding where

import Prelude hiding (all, and, or, not, (&&), (||))

import Data.Maybe (catMaybes)
import Data.Foldable (foldl')
import Data.Functor.Identity
import Control.Monad.State.Strict

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- optics
import Optics hiding (assign)
--import Optics.TH (makeFieldLabelsNoPrefix)  -- req optics-0.4
import Optics.TH (makeFieldLabelsWith, noPrefixFieldLabels)
import Optics.State (modifying, use)
import Data.Map.Optics
import qualified Optics.State as ST

import BooleanAlgebra.Base.Class
import BooleanAlgebra.Base.Expression
import BooleanAlgebra.Base.Logic
import BooleanAlgebra.Transform.CNF
import BooleanAlgebra.Solver.Class
import BooleanAlgebra.Solver.Basic

{-----------------------------------------------------------------------------}
-- Notes

{-
I think we are mixing possibly separate problems here:

- Variable domains
    The variable domains are required during encoding and decoding.
    Since the domain is fixed per variable, it should probably be
    part of it.
    The encoder / decoder needs to be able cast heterogenous types
    in a generic way, whereas user code can keep the actual type
    information. This means we likely need dynamic types a la
    'Data.Typeable' or 'Data.Dynamic'

- Encoding type
    Variable "choice" is just one specific possible encoding (although
    a very general one). We might want to encode other structures, for
    example bitvectors with addition and equality or sets directly
    as booleans.
    Since the chosen encoding is important for decoding, the encoding
    also needs to be part of a variable. The supported operations
    on variables also depend on a specific encoding.

- Encoding a representation for variables
    This is useful for debugging and visualizing, but not really
    neccessary. Variables can just be identified by an Int.
    Additionally, a monad for creating new variables might help.

- Problem structure
    For some problems, e.g. latin squares, there is a regular structure
    on a set of variables. We'd like to encode the whole structure, decoding
    it when the problem is solved, or even during solving for visualization
    (this requires the visualizer to understand the variable encoding, i.e.
    "sets" of possible values for choice variables, lists of "Maybe Bool"
    for bitvectors ...).
    We should be able to support such a structure using standard Haskell
    techniques (esp. Functor / Traversable).
-}

-- Variable name
type VarName = String
-- type Domain a = (Show a, Ord a)

data VarProps where
    EncodeSet :: [Int] -> VarProps
    deriving (Show, Eq, Ord)

data EncodeState = EncodeState
    {   esNextIndex :: Int
    ,   esVarProps :: Map Int VarProps
    ,   esNameMap :: Map VarName Int
    ,   esConstraints :: [BooleanExpr String]
    }
    deriving (Show, Eq)

-- haskell-language-server crashes
-- makeFieldLabelsWith noPrefixFieldLabels ''EncodeState
-- on this, too. issue probably: https://github.com/haskell/haskell-language-server/issues/1297
--makeLenses ''EncodeState

initEncodeState :: EncodeState
initEncodeState = EncodeState 0 mempty mempty []

{-----------------------------------------------------------------------------}
-- Manual splices

-- TODO Remove this section and use makeFieldLabelsWith when
-- haskell-language-server can handle template haskell without crashing.

-- makeFieldLabelsWith noPrefixFieldLabels ''EncodeState
-- ======>
instance (k_as5N ~ A_Lens,
            a_as5O ~ [BooleanExpr String],
            b_as5P ~ [BooleanExpr String]) =>
            LabelOptic "esConstraints" k_as5N EncodeState EncodeState a_as5O b_as5P where
    {-# INLINE labelOptic #-}
    labelOptic
        = lensVL
            (\ f_as5Q s_as5R
                -> case s_as5R of {
                    EncodeState x1_as5S x2_as5T x3_as5U x4_as5V
                        -> (fmap
                            (\ y_as5W -> (((EncodeState x1_as5S) x2_as5T) x3_as5U) y_as5W))
                            (f_as5Q x4_as5V) })
instance (k_as5X ~ A_Lens,
            a_as5Y ~ Map VarName Int,
            b_as5Z ~ Map VarName Int) =>
            LabelOptic "esNameMap" k_as5X EncodeState EncodeState a_as5Y b_as5Z where
    {-# INLINE labelOptic #-}
    labelOptic
        = lensVL
            (\ f_as60 s_as61
                -> case s_as61 of {
                    EncodeState x1_as62 x2_as63 x3_as64 x4_as65
                        -> (fmap
                            (\ y_as66 -> (((EncodeState x1_as62) x2_as63) y_as66) x4_as65))
                            (f_as60 x3_as64) })
instance (k_as67 ~ A_Lens, a_as68 ~ Int, b_as69 ~ Int) =>
            LabelOptic "esNextIndex" k_as67 EncodeState EncodeState a_as68 b_as69 where
    {-# INLINE labelOptic #-}
    labelOptic
        = lensVL
            (\ f_as6a s_as6b
                -> case s_as6b of {
                    EncodeState x1_as6c x2_as6d x3_as6e x4_as6f
                        -> (fmap
                            (\ y_as6g -> (((EncodeState y_as6g) x2_as6d) x3_as6e) x4_as6f))
                            (f_as6a x1_as6c) })
instance (k_as6h ~ A_Lens,
            a_as6i ~ Map Int VarProps,
            b_as6j ~ Map Int VarProps) =>
            LabelOptic "esVarProps" k_as6h EncodeState EncodeState a_as6i b_as6j where
    {-# INLINE labelOptic #-}
    labelOptic
        = lensVL
            (\ f_as6k s_as6l
                -> case s_as6l of {
                    EncodeState x1_as6m x2_as6n x3_as6o x4_as6p
                        -> (fmap
                            (\ y_as6q -> (((EncodeState x1_as6m) y_as6q) x3_as6o) x4_as6p))
                            (f_as6k x2_as6n) })

{-----------------------------------------------------------------------------}

newtype EVar a = EVar Int

type EnS = StateT EncodeState Identity
newtype EncodeM a = EncodeM { toStateT :: EnS a }
    deriving (Functor, Applicative, Monad)

-- internal
liftEncode :: (a -> EnS b) -> a -> EncodeM b
liftEncode f x = EncodeM $ f x

-- internal
esIncIndex :: EnS Int
esIncIndex = do
    i <- use #esNextIndex
    modifying #esNextIndex (+1)
    return i

-- internal
autoName :: String -> EnS (Int, String)
autoName nx = let
    check name err = use (#esNameMap % at name) >>= \case
        Just _  -> err
        Nothing -> return ()
    in case nx of
    "" -> do
        i <- esIncIndex
        let name = show i
        check name $ error "EncodeM interal error"
        return (i, name)
    name -> do
        i <- esIncIndex
        check name $ error $ "Variable " ++ show name ++ " is already defined!"
        return (i, name)

newChoiceVar :: String -> [Int] -> EncodeM (EVar Int)
newChoiceVar namex dom = EncodeM $ do
    (i, name) <- autoName namex
    modifying #esVarProps $ Map.insert i (EncodeSet dom)
    modifying #esConstraints $ (choose name dom :)
    modifying #esNameMap $ Map.insert name i
    return $ EVar i

runEncodeM :: EncodeM a -> (a, EncodeState)
runEncodeM m = runIdentity $ runStateT (toStateT m) initEncodeState

{-----------------------------------------------------------------------------}
-- Encoding variable assignments

encodeAssign :: Show a => String -> a -> String
encodeAssign v x = v ++ "=" ++ show x

assign :: BooleanAlgebra b String => VarName -> Int -> b String
assign v x = var $ encodeAssign v x

{-----------------------------------------------------------------------------}

-- Encode domain
choose :: BooleanAlgebra b String => String -> [Int] -> b String
choose v set = existsUnique set $ \x -> assign v x

-- Decode result
decodeChoose :: String -> [Int] -> Map String Bool -> Int
decodeChoose v set r = let
    findResult :: Int -> Maybe Int
    findResult x = let
        name = encodeAssign v x
        in case Map.lookup name r of
            Nothing     -> error $ "Missing result variable: " ++ show name
            Just False  -> Nothing
            Just True   -> Just x
    -- Find a unique true assignment
    in case catMaybes (findResult <$> set) of
        []      -> error $ "No valid assignment for: " ++ show v
        [x]     -> x
        xs      -> error $ "Ambiguous assignments: " ++ show v ++ " = " ++ show xs

{-----------------------------------------------------------------------------}

all :: (BooleanArithmetic b, Foldable t) => t a -> (a -> b) -> b
all t f = foldl' (\l r -> l `and` f r) true t

neq :: BooleanAlgebra b String => [Int] -> VarName -> VarName -> b String
neq dom v1 v2 =     -- Check domains are actually equal!
    forAll dom $ \x ->
    assign v1 x `excludes` assign v2 x

-- Basically the same as unique
allDifferent :: (BooleanAlgebra b String, Foldable t) => [Int] -> t VarName -> b String
allDifferent dom t =
    all t $ \v1 ->
    all t $ \v2 ->  -- TODO: Only need ordered pairs
    given (v1 /= v2) $
    neq dom v1 v2

{-----------------------------------------------------------------------------}

-- | Solve or throw an error
simpleSolve :: forall a. (Show a, Ord a, Monoid a) => BooleanExpr a -> Map a Bool
simpleSolve problem = let
    solution :: Either (SatError a) (SatResult a)
    solution = runIdentity $ runSatT (return . Left) $
        Right <$> solve BasicSolver (toCNF problem)
    in case solution of
        Left err            -> error (show err)
        Right Unsat         -> error "Unsat"
        Right (Sat result)  -> result

-- | Solve assignment problem
solveAssignment :: (Show a, Ord a, Monoid a) => BooleanExpr a -> [a]
solveAssignment problem = fmap fst $ filter snd $ Map.toList $ simpleSolve problem
