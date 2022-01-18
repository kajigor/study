{-# LANGUAGE TemplateHaskell #-}

module Test.DnfI where

import DnfI ( isNorm, eval, norm )
import Test.Generators ( getSmallExpr, genExpr )
import Hedgehog ( assert, forAll, property, (===) )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Debug.Trace

hprop_norm = property $ do
  x <- forAll (getSmallExpr 10)
  let x' = norm x
  assert $ isNorm x'
  eval x === eval x'