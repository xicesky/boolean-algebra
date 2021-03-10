
module BooleanAlgebra.Base.Pretty where

{-----------------------------------------------------------------------------}
-- TODO !!

prettyBool :: Show a => a -> String
prettyBool = show

printBool :: Show a => a -> IO ()
printBool = putStrLn . prettyBool

{-----------------------------------------------------------------------------}
-- {-----------------------------------------------------------------------------}

-- import Data.Bool (bool)
-- import Data.List (intersperse)
-- --import Control.Monad.Reader
-- import Data.Functor.Identity (Identity(..))

-- import Data.Tree (Tree(..))
-- import Data.Tree.View (showTree)

-- import Data.Comp
-- import Data.Comp.Render (Render(..), stringTree)
-- import Data.Comp.Derive

-- import BooleanAlgebra.Util.THUtil
-- import BooleanAlgebra.Util.Util
-- import BooleanAlgebra.Base.Expression

-- {-----------------------------------------------------------------------------}
-- -- "Pretty" printer
-- -- uses unicode symbols, but doesn't generate valid haskell expressions
-- -- Src: https://en.wikipedia.org/wiki/List_of_logic_symbols

-- {- TODO:
--     Use https://hackage.haskell.org/package/prettyprinter-1.2.0.1#readme
-- -}

-- -- Monad used for pretty-printing
-- type PrettyM m = m

-- -- | Algebra for pretty-printing terms with compdata
-- class (Traversable f, Render f) => PrettyBool f where
--     -- showsPrec for our pretty printer
--     prettyPrintBoolAlg :: Monad m => AlgM (PrettyM m) f (Int -> ShowS)

-- -- Lift prettyPrintBoolAlg over sums of functors
-- $(deriveLiftSum [''PrettyBool])


-- -- FIXME: Tree rendering will show numbers, not names
-- -- and should be monadic

-- -- FIXME: needed ????
-- class PrettyWithoutNames a where
--     prettyShowSPrecM :: Monad m => a -> Int -> PrettyM m ShowS


-- -- | Class for pretty-printing in boolean expression fashing
-- class PrettyAlmostBool a where
--     prettyPrintAB :: a -> Int -> ShowS
--     prettyTree :: a -> Tree String

-- prettyBool :: PrettyAlmostBool a => a -> String
-- prettyBool e = prettyPrintAB e 0 ""

-- printBool :: PrettyAlmostBool a => a -> IO ()
-- printBool = putStrLn . prettyBool

-- drawBool :: PrettyAlmostBool a => a -> IO ()
-- drawBool = putStrLn . showTree . prettyTree

-- {-----------------------------------------------------------------------------}
-- -- Utilities

-- -- Generalized show function for constructors
-- -- Copied over from Data.Comp.Derive.Show for manual ShowF instances
-- showCon :: String -> [String] -> String
-- showCon con [] = con
-- showCon con args = "(" ++ con ++ " " ++ unwords args ++ ")"

-- -- | Concatenate a list of ShowS using a seperator
-- ccShowList :: String -> String -> String -> [ShowS] -> ShowS
-- ccShowList begin sep end list = let
--     sList = foldr (.) id $ intersperse (showString sep) list
--     in showString begin . sList . showString end

-- {- | Concatenate lists of precedence-dependent arguments with an operator
-- -}
-- ccListOp :: (Int, String) -> (Int -> ShowS) -> [Int -> ShowS] -> Int -> ShowS
-- ccListOp _          empty []    = empty
-- ccListOp _          _     [e]   = e
-- ccListOp (prec, op) _     es    = \d -> let
--     listOfShowS :: [ShowS]
--     listOfShowS = fmap ($ prec+1) es
--     in showParen (d > prec) $ ccShowList "" op "" listOfShowS

-- {-----------------------------------------------------------------------------}
-- -- Instances for basic boolean expressions

-- {- Side note: This can't work, can it?
-- rr :: Monad m => (a -> m b) -> m (a -> b)
-- -}

-- instance PrettyBool BooleanValue where
--     prettyPrintBoolAlg :: Monad m => BooleanValue (Int -> ShowS) -> PrettyM m (Int -> ShowS)
--     prettyPrintBoolAlg BTrue = return $ \_ -> showString "⊤"
--     prettyPrintBoolAlg BFalse = return $ \_ -> showString "⊥"

-- instance Render BooleanValue

-- instance PrettyBool BooleanVariable where
--     prettyPrintBoolAlg :: Monad m => BooleanVariable (Int -> ShowS) -> PrettyM m (Int -> ShowS)
--     prettyPrintBoolAlg (BVariable name) = do
--         return $ \_ -> showString name

-- instance Render BooleanVariable

-- instance PrettyBool BooleanNot where
--     prettyPrintBoolAlg :: Monad m => BooleanNot (Int -> ShowS) -> PrettyM m (Int -> ShowS)
--     prettyPrintBoolAlg (BNot e) = return $ \d -> showParen (d > prec) $
--         showString "¬" . e (prec+1)
--         where prec = 10

-- instance Render BooleanNot

-- instance PrettyBool BooleanOp where
--     prettyPrintBoolAlg :: Monad m => BooleanOp (Int -> ShowS) -> PrettyM m (Int -> ShowS)
--     prettyPrintBoolAlg (BAnd a b) = return $ \d -> showParen (d > prec) $
--         a (prec+1) . showString "∧" . b (prec+1)
--         where prec = 6
--     prettyPrintBoolAlg (BOr a b) = return $ \d -> showParen (d > prec) $
--         a (prec+1) . showString "∨" . b (prec+1)
--         where prec = 3

-- instance Render BooleanOp

-- -- All our normal terms are pretty-printable
-- instance PrettyBool e => PrettyAlmostBool (Term e) where
--     prettyPrintAB :: Term e -> Int -> ShowS
--     prettyPrintAB = runIdentity . cataM prettyPrintBoolAlg
--     prettyTree :: Term e -> Tree String
--     prettyTree t = stringTree t

-- -- Non-recursive terms can be pretty-printed for any param type
-- instance PrettyAlmostBool (BooleanValue a) where
--     prettyPrintAB :: BooleanValue a -> Int -> ShowS
--     prettyPrintAB = runIdentity . prettyPrintBoolAlg . constmap
--     prettyTree = stringTreeAlg . constmap

-- -- Non-recursive terms can be pretty-printed for any param type
-- instance PrettyAlmostBool (BooleanVariable a) where
--     prettyPrintAB :: BooleanVariable a -> Int -> ShowS
--     prettyPrintAB = runIdentity . prettyPrintBoolAlg . constmap
--     prettyTree = stringTreeAlg . constmap

-- {-----------------------------------------------------------------------------}
-- -- Instances for simplified expressions

-- -- Pretty-print types like: Either (BooleanValue ()) (Term BooleanExprSimpF)
-- -- actually all instances with "Either a b" are pretty-printable, if both a and b are.
-- -- We see the "Either" instance as "untagged" because we assume that a and b are disjunct.
-- instance (PrettyAlmostBool a, PrettyAlmostBool b) => PrettyAlmostBool (Either a b) where
--     prettyPrintAB :: Either a b -> Int -> ShowS
--     prettyPrintAB (Left v) = prettyPrintAB v
--     prettyPrintAB (Right e) = prettyPrintAB e
--     prettyTree (Left v ) = Node "Left"  [prettyTree v]
--     prettyTree (Right e) = Node "Right" [prettyTree e]

-- {-----------------------------------------------------------------------------}
-- -- Instances for boolean literals

-- instance PrettyBool BooleanLit where
--     prettyPrintBoolAlg :: Monad m => BooleanLit (Int -> ShowS) -> PrettyM m (Int -> ShowS)
--     prettyPrintBoolAlg (BooleanLit i) = do
--         let sign = i > 0
--         let neg = bool (showString "¬") id sign
--         return $ \_ ->
--             neg . shows (abs i)

-- instance Render BooleanLit

-- -- Non-recursive terms can be pretty-printed for any param type
-- instance PrettyAlmostBool (BooleanLit a) where
--     prettyPrintAB :: BooleanLit a -> Int -> ShowS
--     prettyPrintAB = runIdentity . prettyPrintBoolAlg . constmap
--     prettyTree = stringTreeAlg . constmap

-- -- When names are provided in addition to the term, we can show them
-- -- FIXME later
-- instance PrettyAlmostBool a => PrettyAlmostBool ([String], a) where
--     prettyPrintAB (names, a) = prettyPrintAB a
--     prettyTree (names, a) = prettyTree a

-- {-----------------------------------------------------------------------------}
-- -- Instances for aggregate form

-- {- TODO: Fix bugs in compdata

-- Bug #1:
--     show-ing terms of BooleanCD is wrong and puts quotes where none belong:
--         putStrLn $ show exampleExpr03
--         (BooleanCD [["(BooleanLit True \"a\")","(BooleanLit False \"b\")"],["(BooleanLit False \"c\")","(BooleanLit True \"d\")"]])
    
--     Data.Comp.Derive.Show handles arguments this way:
--         mkShow :: (Bool, ExpQ) -> ExpQ
--         mkShow (isFArg, var)
--             | isFArg = var
--             | otherwise = [| show $var |]
--     Apparently [[e]] is not a functor argument - this can lead to other bugs!

--     This one will be hard to get right, but should accept nested functors:
--         data Meh e = Meh e          deriving Functor
--         data Muh e = Muh (Meh e)    deriving Functor

-- Bug #2:
--     compdata should use showsPrec instead of show
--     1. Performance (maybe not that relevant, ghc rewrites a lot of that stuff)
--     2. Precedence is important, don't put parens everywhere

-- -}

-- -- Custom instance of ShowF - workaround for a bug in compdata
-- instance ShowF Conjunction where
--     showF (Conjunction xs) = let
--         ccList :: [ShowS] -> ShowS
--         ccList = ccShowList "[" ", " "]"
--         strCDs :: ShowS
--         strCDs = ccList . fmap (++) $ xs
--         in showCon "Conjunction" [strCDs ""]

-- instance ShowF Disjunction where
--     showF (Disjunction xs) = let
--         ccList :: [ShowS] -> ShowS
--         ccList = ccShowList "[" ", " "]"
--         strCDs :: ShowS
--         strCDs = ccList . fmap (++) $ xs
--         in showCon "Disjunction" [strCDs ""]

-- -- Custom instance of ShowConstr
-- -- FIXME: not pretty, due to the defintion of ShowConstr
-- instance ShowConstr Conjunction where
--     showConstr :: Conjunction a -> String
--     showConstr _ = "Conjunction []"

-- -- Custom instance of ShowConstr
-- -- FIXME: not pretty, due to the defintion of ShowConstr
-- instance ShowConstr Disjunction where
--     showConstr :: Disjunction a -> String
--     showConstr _ = "Disjunction []"

-- -- Pretty-printer for Conjunction
-- instance PrettyBool Conjunction where
--     prettyPrintBoolAlg :: Monad m => Conjunction (Int -> ShowS) -> PrettyM m (Int -> ShowS)
--     prettyPrintBoolAlg (Conjunction ts)
--         = return $ showConjs ts where
--             showConjs :: [Int -> ShowS] -> Int -> ShowS
--             showConjs = ccListOp (6, "∧") empty where
--                 empty = prettyPrintAB BTrue

-- -- Pretty-printer for Disjunction
-- instance PrettyBool Disjunction where
--     prettyPrintBoolAlg :: Monad m => Disjunction (Int -> ShowS) -> PrettyM m (Int -> ShowS)
--     prettyPrintBoolAlg (Disjunction ts)
--         = return $ showDisjs ts where
--             showDisjs :: [Int -> ShowS] -> Int -> ShowS
--             showDisjs = ccListOp (3, "∨") empty where
--                 empty = prettyPrintAB BFalse

-- -- Special instance, BooleanCD is just "a little different"
-- instance Render Conjunction
-- instance Render Disjunction

-- {-----------------------------------------------------------------------------}
-- -- Instances for CNF

-- -- Pretty-printer for CNF
-- -- Idea: Maybe print each disjunction on a seperate line
-- instance PrettyAlmostBool a => PrettyAlmostBool (Conjunction a) where
--     prettyPrintAB :: Conjunction a -> Int -> ShowS
--     prettyPrintAB = runIdentity . prettyPrintBoolAlg . fmap prettyPrintAB
--     prettyTree = stringTreeAlg . fmap prettyTree

-- instance PrettyAlmostBool a => PrettyAlmostBool (Disjunction a) where
--     prettyPrintAB :: Disjunction a -> Int -> ShowS
--     prettyPrintAB = runIdentity . prettyPrintBoolAlg . fmap prettyPrintAB
--     prettyTree = stringTreeAlg . fmap prettyTree
