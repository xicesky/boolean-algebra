
module BooleanAlgebra.Transform.IntermediateForms where

import Data.Comp
import Data.Comp.Derive

import BooleanAlgebra.Base.Class
import qualified BooleanAlgebra.Base.Class as B
import BooleanAlgebra.Base.Expression

{-----------------------------------------------------------------------------}
-- Simplified form
-- without literal values (True, False)
-- (The output of simplify)

-- BooleanExprF without BooleanValue
type BooleanExprSimpF
    =   BooleanVariable
    :+: BooleanNot
    :+: BooleanOp


-- Simplified boolean expressions are either just "true" or "false"
-- or terms without any boolean values
type BooleanExprSimp = MaybeTrivial (Term BooleanExprSimpF)

instance Boolean (Term BooleanExprSimpF) where
    and = iBAnd
    or = iBOr
    not = iBNot

-- FIXME
-- instance BooleanPreAlgebra (Term BooleanExprSimpF) where
--     var = iBVar

instance Boolean BooleanExprSimp where
    and (Left x) (Left y)       = Left $ x `B.and` y
    and (Right x) (Right y)     = Right $ x `B.and` y
    and (Left BTrue) (Right y)  = Right y
    and (Left BFalse) (Right _) = Left BFalse
    and (Right x) left          = B.and left (Right x)

    or  (Left x) (Left y)       = Left $ x `B.or` y
    or  (Right x) (Right y)     = Right $ x `B.or` y
    or  (Left BTrue) (Right _)  = Left BTrue
    or  (Left BFalse) (Right y) = Right y
    or  (Right x) left          = B.or left (Right x)
    
    not (Left x)                = Left $ B.not x
    not (Right x)               = Right $ B.not x

{-----------------------------------------------------------------------------}
-- Boolean "literal" form
-- Literal = Variable + optional Negation

-- BooleanExpr without BooleanValue, BooleanVariable, BooleanNot
--  but using BooleanLit
type BooleanExprLitF
    =   BooleanLit
    :+: BooleanOp

type BooleanExprLit = Term BooleanExprLitF

{-----------------------------------------------------------------------------}
-- Aggregate form (output of aggregateConjDisj)

-- | Flattened BooleanExprLit
type BooleanExprFlatLitF
    =   BooleanLit
    :+: Conjunction
    :+: Disjunction

type BooleanExprFlatLit = Term BooleanExprFlatLitF

-- TODO: This could be done but needs a bigger impl which doesn't belong here!
-- instance Boolean (BooleanExprFlatLit) where
--     and = iBAnd
--     or = iBOr
--     not = iBNot
