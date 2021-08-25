module Test.Timeout where

import Test.Tasty ( localOption, mkTimeout, TestName, TestTree )
import Test.Tasty.HUnit ( testCase, Assertion, (@?=) )
import Lib ( slowFib, fib )

defaultTimeout :: Integer
defaultTimeout = 2000000

timeOutTest :: Integer -> TestName -> Assertion -> TestTree
timeOutTest timeout testName assertion =
  localOption (mkTimeout timeout) $ testCase testName assertion

test_slowFib =
  timeOutTest defaultTimeout "slowFib 10" $ slowFib 10 @?= 55

test_slowFibTimeout =
  timeOutTest defaultTimeout "slowFib 100" $ slowFib 100 @?= 354224848179261915075

test_fastFib =
  timeOutTest defaultTimeout "fib 10" $ fib 10 @?= 55

test_fastFibTimeout =
  timeOutTest defaultTimeout "fib 100" $ fib 100 @?= 354224848179261915075
