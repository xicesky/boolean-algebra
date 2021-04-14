
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BooleanAlgebra.Problem.Encoding where

import Prelude hiding (all, and, or, not, (&&), (||))

import Data.String (fromString)
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

-- For pretty-printing encoding state
import Prettyprinter
import Missing.Prettyprinter
import Term.Prettyprinter
import BooleanAlgebra.Base.Pretty

import Control.Monad.Naming.Class
import Control.Monad.Naming.GenNameT hiding (toStateT)

import BooleanAlgebra.Base.Class
import BooleanAlgebra.Base.Expression
import BooleanAlgebra.Base.Logic
import BooleanAlgebra.Transform.CNF
import BooleanAlgebra.Solver.Class
import BooleanAlgebra.Solver.Basic

import GHC.Stack (HasCallStack)

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
    Even for uniquely choosing a variable from a finite set of integers
    there are different possible encodings, for example "one hot" (with
    several variants), "unary" or "binary" encodings. See the papers
    below for some examples.

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
    techniques (esp. Functor / Traversable), so just supporting encodings
    for variables should be enough, given that variables can be used
    transparently using existing functions.

Interesting Papers:
    - 2009 Björk - Successful SAT Encoding Techniques DOI 10.3233/SAT190085
    - 2020 Gorjiara, Xu, Demsky - Satune: synthesizing efficient SAT encoders DOI 10.1145/3428214

-}

-- Variable name
type VarName = String
-- type Domain a = (Show a, Ord a)

data VarProps where
    EncodeSet :: [Int] -> VarProps
    deriving (Show, Eq, Ord)

data EncodeState = EncodeState
    {   esVarProps :: Map Int VarProps
    ,   esConstraints :: [BooleanExpr String]
    }
    deriving (Show, Eq)

-- TODO Could just be a reader
data DecodeState = DecodeState
    {   dsVarProps :: Map Int VarProps
    ,   dsSolution :: Map String Bool
    }

-- haskell-language-server crashes
-- makeFieldLabelsWith noPrefixFieldLabels ''EncodeState
-- makeFieldLabelsWith noPrefixFieldLabels ''DecodeState

initEncodeState :: EncodeState
initEncodeState = EncodeState mempty []

{-----------------------------------------------------------------------------}
-- Manual splices

-- TODO Remove this section and use makeFieldLabelsWith (above) when
-- haskell-language-server can handle template haskell without crashing.
-- https://github.com/haskell/haskell-language-server/issues/1297
-- https://github.com/haskell/haskell-language-server/issues/1342

{-# ANN module "HLint: ignore Redundant bracket" #-}
{-# ANN module "HLint: ignore Avoid lambda" #-}

-- makeFieldLabelsWith noPrefixFieldLabels ''EncodeState
-- ======>
instance (k_apWG ~ A_Lens,
          a_apWH ~ [BooleanExpr String],
          b_apWI ~ [BooleanExpr String]) =>
         LabelOptic "esConstraints" k_apWG EncodeState EncodeState a_apWH b_apWI where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_apWJ s_apWK
           -> case s_apWK of {
                EncodeState x1_apWL x2_apWM
                  -> (fmap (\ y_apWN -> (EncodeState x1_apWL) y_apWN))
                       (f_apWJ x2_apWM) })
instance (k_apWO ~ A_Lens,
          a_apWP ~ Map Int VarProps,
          b_apWQ ~ Map Int VarProps) =>
         LabelOptic "esVarProps" k_apWO EncodeState EncodeState a_apWP b_apWQ where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_apWR s_apWS
           -> case s_apWS of {
                EncodeState x1_apWT x2_apWU
                  -> (fmap (\ y_apWV -> (EncodeState y_apWV) x2_apWU))
                       (f_apWR x1_apWT) })

-- makeFieldLabelsWith noPrefixFieldLabels ''DecodeState
-- ======>
instance (k_aq1i ~ A_Lens,
          a_aq1j ~ Map String Bool,
          b_aq1k ~ Map String Bool) =>
         LabelOptic "dsSolution" k_aq1i DecodeState DecodeState a_aq1j b_aq1k where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_aq1l s_aq1m
           -> case s_aq1m of {
                DecodeState x1_aq1n x2_aq1o
                  -> (fmap (\ y_aq1p -> (DecodeState x1_aq1n) y_aq1p))
                       (f_aq1l x2_aq1o) })
instance (k_aq1q ~ A_Lens,
          a_aq1r ~ Map Int VarProps,
          b_aq1s ~ Map Int VarProps) =>
         LabelOptic "dsVarProps" k_aq1q DecodeState DecodeState a_aq1r b_aq1s where
  {-# INLINE labelOptic #-}
  labelOptic
    = lensVL
        (\ f_aq1t s_aq1u
           -> case s_aq1u of {
                DecodeState x1_aq1v x2_aq1w
                  -> (fmap (\ y_aq1x -> (DecodeState y_aq1x) x2_aq1w))
                       (f_aq1t x1_aq1v) })

{-----------------------------------------------------------------------------}
-- Prettyprinting

-- data EncodeState = EncodeState
--     {   esVarProps :: Map Int VarProps
--     ,   esConstraints :: [BooleanExpr String]
--     }

instance Pretty (Map Int VarProps) where
    pretty = viaShow

-- TODO: This overlaps with PrettyTerm String, find a solution without overlaps
-- instance PrettyTerm t => PrettyTerm [t] where
--     prettyTerm opts _ = aList (prettyTerm opts 0)

instance PrettyTerm [BooleanExpr String] where
    prettyTerm opts _ = aList (prettyTerm opts 0)

instance PrettyTerm EncodeState where
    prettyTerm :: PrettyOptions -> Precedence -> EncodeState -> Doc ann
    prettyTerm opts _ (EncodeState varProps constraints) = aRecord
        [   fromString "esVarProps = " <> pretty varProps
        ,   fromString "esConstraints = " <> prettyTerm opts 0 constraints
        ]

instance Pretty EncodeState where
    pretty = defaultPretty

{-----------------------------------------------------------------------------}

newtype EVar a = EVar Int
    deriving (Eq)

type EnS = StateT EncodeState (GenNameT VarName Identity)
type DeS = StateT DecodeState EnS

newtype EncodeM a = EncodeM { unEncodeM :: EnS a }
    deriving (Functor, Applicative, Monad)

newtype DecodeM a = DecodeM { unDecodeM :: DeS a }
    deriving (Functor, Applicative, Monad)

-- TODO: This is stupid
class Monad m => HasVarProps m where
    getVarProps :: Int -> m (Maybe VarProps)

instance HasVarProps EnS where
    getVarProps i = use (#esVarProps % at i)

instance HasVarProps DeS where
    getVarProps i = use (#dsVarProps % at i)

runEncodeM :: EncodeM a -> (a, EncodeState)
runEncodeM m = runIdentity $ runGenNameTString 1 $ do
    setNamePrefix "_"   -- unnamed variables start with underscores
    runStateT (unEncodeM m) initEncodeState

withSolution :: DecodeM a -> EncodeM (Maybe a)
withSolution m = EncodeM $ do
    cs <- use #esConstraints
    props <- use #esVarProps
    
    case maybeSolve (forAll (reverse cs) id) of
        Nothing     -> return Nothing
        Just soln   -> do
            (v, _) <- runStateT (unDecodeM m) (DecodeState props soln)
            return (Just v)

addConstraint :: BooleanExpr String -> EnS ()
addConstraint c = modifying #esConstraints (c :)

{- | Create a new variable using "one hot" encoding.

If the name is empty, it will be autogenerated. If the name is non-empty,
but already exists, this function will fail with an 'error'.
-}
newChoiceVar :: HasCallStack => String -> [Int] -> EncodeM (EVar Int)
newChoiceVar "" dom = EncodeM (generateName "") >>= (`newChoiceVar` dom)
newChoiceVar name dom = EncodeM $ do
    i <- unsafeNewExactName name
    modifying #esVarProps $ Map.insert i (EncodeSet dom)
    addConstraint $ choose name dom
    return $ EVar i

getDom :: HasVarProps m => Int -> m [Int]
getDom i = do
    mp <- getVarProps i
    case mp of
        Nothing             -> error "Invalid variable"
        Just (EncodeSet s)  -> return s

getChoiceVal :: EVar Int -> DecodeM Int
getChoiceVal (EVar i) = DecodeM $ do
    n <- unsafeNameOf i
    dom <- getDom i
    soln <- use #dsSolution
    return $ decodeChoose n dom soln

constraint :: BooleanExpr String -> EncodeM ()
constraint cond = EncodeM $ addConstraint cond

(&/=) :: EVar Int -> Int -> EncodeM ()
(EVar a) &/= b = EncodeM $ do
    dom <- getDom a
    n <- unsafeNameOf a
    -- TODO use set instead of list
    when (b `elem` dom) $ addConstraint $
        not $ assign n b

(&/=&) :: EVar Int -> EVar Int -> EncodeM ()
(EVar a) &/=& (EVar b) = EncodeM $ do
    domA <- getDom a
    domB <- getDom b
    na <- unsafeNameOf a
    nb <- unsafeNameOf b

    forM_ domB $ \v ->
        when (v `elem` domA) $ addConstraint $
            (assign na v) `excludes` (assign nb v)

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

allDifferent :: Traversable t => t (EVar Int) -> EncodeM ()
allDifferent t = 
    forM_ t $ \v1 ->
    forM_ t $ \v2 ->
    when (v1 /= v2) $
    v1 &/=& v2

{-----------------------------------------------------------------------------}

-- | Solve or return 'Nothing' on 'Unsat'
maybeSolve :: forall a. (Show a, Ord a) => BooleanExpr a -> Maybe (Map a Bool)
maybeSolve problem = let
    solution :: Either (SatError a) (SatResult a)
    solution = runIdentity $ runSatT (return . Left) $
        Right <$> solve BasicSolver (toCNF problem)
    in case solution of
        Left err            -> error (show err)
        Right Unsat         -> Nothing
        Right (Sat result)  -> Just result

-- | Solve or throw an error
simpleSolve :: forall a. (Show a, Ord a) => BooleanExpr a -> Map a Bool
simpleSolve problem = let
    solution :: Either (SatError a) (SatResult a)
    solution = runIdentity $ runSatT (return . Left) $
        Right <$> solve BasicSolver (toCNF problem)
    in case solution of
        Left err            -> error (show err)
        Right Unsat         -> error "Unsat"
        Right (Sat result)  -> result

-- | Solve assignment problem
solveAssignment :: (Show a, Ord a) => BooleanExpr a -> [a]
solveAssignment problem = fmap fst $ filter snd $ Map.toList $ simpleSolve problem
