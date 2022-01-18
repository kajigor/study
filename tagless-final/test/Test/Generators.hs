{-# LANGUAGE InstanceSigs #-}

module Test.Generators where

import qualified Test.Tasty.Hedgehog
import qualified Hedgehog.Gen as Gen
import Hedgehog (Gen)
import qualified Hedgehog.Range as Range
import DnfI ( Expr(..) )

defaultLower = 0

defaultUpper = 10

defaultRange = Range.linear defaultLower defaultUpper

genInt :: Gen Int
genInt = Gen.int defaultRange

genListBool :: Gen [Int]
genListBool = Gen.list defaultRange genInt

getSmallExpr maxDepth =
    Gen.filter (\x -> depth x < maxDepth) genExpr
  where
    depth (Lit _) = 1
    depth (Neg e) = 1 + depth e
    depth (Add x y) = 1 + (max (depth x) (depth y))
    depth (Mul x y) = 1 + (max (depth x) (depth y))

genExpr :: Gen Expr
genExpr =
    Gen.recursive Gen.choice [
      -- non recursive
        Lit <$> genInt
    ]
    [ -- recursive
        Neg <$> genExpr
      , genBinary Add
      , genBinary Mul
    ]
  where
    genBinary constr = do
      Gen.subterm2 genExpr genExpr constr
